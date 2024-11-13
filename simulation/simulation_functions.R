# functions for simulation


#######
# This function is the big one that runs the full simulation. This simulation
# runs a number of years of socsim equal to each value in the segments vector
# and then assigns ancestry information and group to any new children produced
# by that simulation before continuing on with the simulation. This makes it 
# fast to measure ancestry and flexible and extensivel in terms of handling 
# how group is assigned. 
# sim_name - the name to call this simulation. This will be the name of the 
#            folder.
# pop_start - the population data.frame to start the simulation with. This can
#             be the result of a previous simulation, but if so the mar and 
#             ancestry files must also be provided to get correct results.
# segments - A vector where each value gives the number of years to sim for 
#            each segment. Segments must be 15 years or less in order to 
#            correctly assign children to group before they start partnering.
# endogamy - A vector of the same length as segments that gives the endogamy
#            parameter to use for each segment (between 0 and 1).
# inheritance - A vector of the same length that gives the inheritance value
#               to use when assigning children to groups. See the 
#               calculate_ancestry function for details. If left NULL, it will
#               default to random (50/50) assignment of mixed children to groups.
# mar - A marriage dataset to use for the start of the simulation. This can be left
#       null for simulations starting from scratch, but should be specified 
#       by the final mar file from a previous sim if starting from a previous
#       sim.
# ancestry - An ancestry dataset to use for the start of a simulation. This can
#            be left null for simulations starting from scratch, but should be 
#            specified by the final ancestry file from a previous sim if 
#            starting from a previous sim.
# fert_multiplier - A multiplier to apply to base fertility rates to dictate
#                   the overall growth rate of the population.
######

sim_name <- "test"
pop_start <- presim_opop
segments <- rep(10, 3)
endogamy <- rep(0.95, 3)
inheritance <- rep(0.5, 3)
pop_start <- presim_opop |>
  mutate(group = sample(1:2, nrow(presim_opop), replace = T, 
                        prob = c(0.5, 0.5)))

run_simulation <- function(sim_name, 
                           pop_start,
                           segments, 
                           endogamy,
                           inheritance = NULL,
                           mar = NULL,
                           ancestry = NULL,
                           fert_multiplier = 1) {
  
  # do some checks
  #if(max(segments) > 15) {
  #  stop("All segments must be 15 years or less or group assignment will not work correctly")
  #}
  
  if(length(segments) != length(endogamy)) {
    stop("The length of the segments argument and the endogamy argument must be the same.")
  }
  
  if(!is.null(inheritance) & (length(segments) != length(inheritance))) {
    stop("If inheritance rules are specified, the argument must be the same length as segments")
  }
  
  # check for null values on arguments and address
  if(is.null(inheritance)) {
    message("No inheritance rules specified, defaulting to random")
    inheritance <- rep(0.5, length(segments))
  }
  
  if(is.null(mar)) {
    # start with an empty data frame for mar
    mar <- data.frame()
  }
  
  # set up the directory for output 
  if(dir_exists(here(base_folder, sim_name))) {
    dir_delete(here(base_folder, sim_name))
  }
  
  folder <- create_simulation_folder(simulation_name = sim_name, 
                                     basefolder = base_folder)
  
  # presim files
  write.table(pop_start, here(folder, "presim.opop"), 
              row.names = F, col.names = F)
  write.table(mar, here(folder, "presim.omar"), 
              row.names = F, col.names = F)
  
  # create initial ancestry dataset if it does not exist
  if(is.null(ancestry)) {
    ancestry <- pop_start |>
      as_tibble() |>
      mutate(ancestry_group1 = as.numeric(group == 1),
             ancestry_group2 = as.numeric(group == 2),
             nearest_gen_locus = NA) |>
      select(pid, group, ancestry_group1, ancestry_group2, nearest_gen_locus)
  }
  
  # create rate file
  file_copy(here("simulation", "parameter_files", "basic_rates"), 
            here(folder, "basic_rates"))
  # add fertility rates
  create_fertility_rates(here(folder, "basic_rates"), fert_multiplier)
  
  # starting month
  month <- 1200
  
  for(i in 1:length(segments)) {
    
    len_years <- segments[i]
    year <- 0
    while(year < len_years) {
      # update sup file
      file_copy(here("simulation", "parameter_files", "group2_stub.sup"), 
                here(folder, "run.sup"), overwrite = TRUE)
      cat("\nduration", 12, "\n", file = here(folder, "run.sup"),
      append = TRUE)
      cat("include basic_rates\n", file = here(folder, "run.sup"),
          append = TRUE)
      cat("endogamy", endogamy[i], "\n", file = here(folder, "run.sup"),
          append = TRUE)
      cat("run\n", file = here(folder, "run.sup"),
          append = TRUE)
      
      # update final month, we always go one year at a time
      month <- month + 12
      
      # run the simulation - future is needed not to screw up other sims without
      # restarting R
      socsim(folder, "run.sup", seed = seed, process_method = "future")
      
      # get the new pop and mar data
      pop <- rsocsim::read_opop(folder, "run.sup", seed) |> 
        as_tibble()
      mar <- rsocsim::read_omar(folder, "run.sup", seed) |>
        as_tibble()
      
      # find the new kids and measure their ancestry, group, etc.
      new_kids <- calculate_ancestry(pop, ancestry, inheritance[i])
      # assign back the new group to pop
      pop$group[new_kids$pid] <- new_kids$group
      # add new kids to ancestry data for next generation
      ancestry <- new_kids |>
        select(pid, group, ancestry_group1, ancestry_group2, nearest_gen_locus) |>
        bind_rows(ancestry)
      
      # create new marriages
      new_marriages <- get_married(pop, mar, month)
      
      # add new marriages
      mar <- mar |>
        bind_rows(new_marriages)
      
      # re-assign marriage values in the pop file
      pop$marid[new_marriages$wpid] <- new_marriages$mid
      pop$mstat[new_marriages$wpid] <- 4
      pop$marid[new_marriages$hpid] <- new_marriages$mid
      pop$mstat[new_marriages$hpid] <- 4
      
      # make the result of last run the new presim
      write.table(pop, here(folder, "presim.opop"), 
                  row.names = F, col.names = F)
      write.table(mar, here(folder, "presim.omar"), 
                  row.names = F, col.names = F)
      
      year <- year + 1
    }
  }
  
  # write out final results
  write_csv(pop, here(folder, "final_pop.csv"))
  write_csv(mar, here(folder, "final_mar.csv"))
  write_csv(ancestry, here(folder, "ancestry.csv"))
  
}

get_married <- function(pop, mar, month_final) {
  
  # get singles
  singles <- pop |>
    filter(mstat != 4 & dod == 0) |>
    mutate(age = (month_final - dob) / 12,
           sex = factor(fem, levels = 0:1, labels = c("Male", "Female"))) |>
    select(pid, sex, group, age, marid) |>
    rename(prior = marid)
  
  matches <- match_partners(singles)
  
  # clean up a bit and return
  matches |>
    mutate(mid = max(mar$mid)+1:nrow(matches),
           dstart = month_final,
           dend = 0,
           rend = 16) |>
    select(mid, wpid, hpid, dstart, dend, rend, wprior, hprior)
  
}

match_partners <- function(singles) {
  
  # break singles into men and women
  single_women <- singles |>
    filter(sex == "Female") |>
    rename(wpid = pid, group_w = group, age_w = age, wprior = prior) |>
    select(wpid, group_w, age_w, wprior)
  single_men <- singles |>
    filter(sex == "Male") |>
    rename(hpid = pid, group_h = group, age_h = age, hprior = prior) |>
    select(hpid, group_h, age_h, hprior)
  
  ## TODO: track parents and grandparents to avoid incest
  
  # speed dating!
  matches <- single_women |>
    # women do the choosing
    group_by(wpid) |>
    group_split() |> 
    map(function(x) {
      # sample 50 partners for each woman
      slice_sample(single_men, n = 50) |> 
        bind_cols(x) |>
        # calculate covariates and odds ratios using Dem Research article numbers
        mutate(age_diff = age_h - age_w,
               exogamy = group_h != group_w,
               or = exp(0.072 * age_diff - 0.014 * age_diff^2 -2 * exogamy)) |>
        # we don't need the actual probabilities because weights will be 
        # standardized in slice_sample which amounts to the same thing
        # pick a partner!
        slice_sample(n = 1, weight_by = or)
    }) |> 
    bind_rows() |>
    # some men will be chosen multiple times (lucky!), get rid of duplicates
    # try again, ladies!
    filter(!duplicated(hpid))
  
  # now clean it up a bit too match the mar dataset
  
  return(matches)
}

####
# This function is used internally by run_simulation to measure ancestry of
# new children produced in each segment and to assign their group. New ancestry
# measures can be added and tracked here. This cannot be used on the final pop
# file produced by the simulation. 
# pop - the pop dataset for the current simulation. Anyone with a group ==3 will
#       be new children and get stuff measured and group determined.
# ancestry - the ancestry dataset for the current simulation.
# inheritance - a numeric between 0 and 1 indicating the probability of a kid
#               of mixed parentage being assigned to group 2. A value of 1 
#               indicates strict hypodescent and 0 indicates strict 
#               hyperdescent. 0.5 indicates a coin toss. 
calculate_ancestry <- function(pop, 
                               ancestry, 
                               inheritance = 0.5) {
  
  # get "moms" and "dads" for joining properly
  moms <- ancestry |>
    rename(mom = pid, 
           group_mom = group,
           ancestry_group1_mom = ancestry_group1, 
           ancestry_group2_mom = ancestry_group2,
           gen_locus_mom = nearest_gen_locus)
  dads <- ancestry |>
    rename(pop = pid, 
           group_pop = group,
           ancestry_group1_pop = ancestry_group1, 
           ancestry_group2_pop = ancestry_group2,
           gen_locus_pop = nearest_gen_locus)
  
  # create the new_kids object for all kids with group == 3
  new_kids <- pop |> filter(group == 3) |>
    left_join(moms) |>
    left_join(dads) |>
    select(pid, 
           starts_with("group"), 
           starts_with("ancestry"), 
           starts_with("gen_locus")) |>
    mutate(ancestry_group1 = ancestry_group1_mom/2 + ancestry_group1_pop/2,
           ancestry_group2 = ancestry_group2_mom/2 + ancestry_group2_pop/2,
           gen_locus_mom = gen_locus_mom + 1,
           gen_locus_pop = gen_locus_pop + 1,
           nearest_gen_locus = case_when(
             group_mom != group_pop ~ 1,
             is.na(gen_locus_mom) & is.na(gen_locus_pop) ~ NA_integer_,
             !is.na(gen_locus_mom) & 
               (is.na(gen_locus_pop) |
                  gen_locus_mom <= gen_locus_pop) ~ gen_locus_mom,
             TRUE ~ gen_locus_pop))
  
  # assign group to kid
  new_kids <- new_kids |>
    mutate(
      group = case_when(
        group_mom == 1 & group_pop == 1 ~ 1,
        group_mom == 2 & group_pop == 2 ~ 2,
        TRUE ~ 3))
  new_kids$group[new_kids$group == 3] <- sample(1:2, 
                                                replace = TRUE, 
                                                prob = c(1-inheritance, 
                                                         inheritance),
                                                size = sum(new_kids$group == 3))
  
  return(new_kids)
  
}

# this function helps tune the 2000 US fertility rates with a simple 
# multiplier to get a roughly stationary population
create_fertility_rates <- function(file, multiplier) {
  fert <- read_table(here("simulation", "parameter_files", "fertility_rates"), 
                     col_names = c("age", "not_sure", "rate"),
                     col_types = cols(age = "i", 
                                      not_sure = "i",
                                      rate = "d"),
                     comment = "*") 
  # turn into characters to avoid scientific notation
  fert$rate <- as.character(fert$rate * multiplier)
  # turn off scientific notation for correct writing out of fertility rates
  options(scipen = 999)
  cat("\n\n*** Fertility Rates ***\n\n", file = file, append = TRUE)
  cat("birth 1 F single 0\n", file = file, append = TRUE)
  cat("111 0 0\n\n", file = file, append = TRUE)
  cat("birth 2 F single 0\n", file = file, append = TRUE)
  cat("111 0 0\n\n", file = file, append = TRUE)
  cat("birth 1 F married 0\n", file = file, append = TRUE)
  write_delim(fert, file = file, col_names = FALSE, append =TRUE)
  cat("\n\nbirth 2 F married 0\n", file = file, append = TRUE)
  write_delim(fert, file = file, col_names = FALSE, append =TRUE)
}

# get the simulation parameters from google sheets
get_sim_parameters <- function(sim_name, sheet_id) {
  sim_desc <- googlesheets4::range_read(sheet_id, 
                                        range = paste(sim_name, "B1", 
                                                      sep="!"),
                                        col_names = "desc")
  
  sim_start <- googlesheets4::range_read(sheet_id, 
                                         range = paste(sim_name, "A4:C5", 
                                                       sep="!"))
  
  sim_segments <- googlesheets4::range_read(sheet_id, 
                                            range = paste(sim_name, "A8:C1000", 
                                                          sep="!")) |>
    filter(!is.na(segment_length))
  
  return(list(desc = sim_desc$desc[1], start = sim_start, segments = sim_segments))
}

# a function to write new simulation parameters to the google sheet
create_new_simulation <- function(sheet_id,
                                  sim_name,
                                  description,
                                  group1_prop = 0.5,
                                  starting_sim = "",
                                  fert_multiplier = 1,
                                  segment_length,
                                  endogamy,
                                  inheritance,
                                  overwrite = FALSE) {
  
  existing_sim_names <- googlesheets4::sheet_names(sheet_id)
  if(!overwrite & sim_name %in% existing_sim_names) {
    stop(paste(sim_name, "is an already existing sim name in the sheet"))
  }
  
  if(starting_sim != "" & !(starting_sim %in% existing_sim_names)) {
    stop("Specified starting sim does not currently exist in the sheet")
  }
  
  if(length(segment_length) != length(endogamy)) {
    stop("segment_length and endogamy arguments must be the same length")
  }
  
  if(length(segment_length) != length(inheritance)) {
    stop("segment_length and inheritance arguments must be the same length")
  }
  
  sim_start <- tibble(group1_prop, starting_sim, fert_multiplier)
  
  sim_segments <- tibble(segment_length, endogamy, inheritance)
  
  if(sim_name %in% existing_sim_names) {
    # blank out this sheet - this is a bit crude, feels like there must
    # be a better way to do it
    googlesheets4::range_write(sheet_id,
                               tibble(x = rep("", 1000), 
                                      y = rep("", 1000), 
                                      z = rep("", 1000)),
                               range = paste(sim_name, "A1", sep="!"),
                               col_names = FALSE)
  } else {
    googlesheets4::sheet_add(sheet_id, sim_name) 
  }
  googlesheets4::range_write(sheet_id,
                             tibble(x = "description", y = description),
                             range = paste(sim_name, "A1", sep="!"),
                             col_names = FALSE)
  googlesheets4::range_write(sheet_id, sim_start, 
                             range = paste(sim_name, "A4", sep="!"))
  googlesheets4::range_write(sheet_id, sim_segments, 
                             range = paste(sim_name, "A8", sep="!"))
  
  # add comments
  googlesheets4::range_write(sheet_id,
                             tibble(x = "# starting simulation parameters"),
                             range = paste(sim_name, "A3", sep="!"),
                             col_names = FALSE)
  googlesheets4::range_write(sheet_id,
                             tibble(x = "# segment parameters"),
                             range = paste(sim_name, "A7", sep="!"),
                             col_names = FALSE)
  
}
