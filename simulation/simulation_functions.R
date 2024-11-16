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
run_simulation <- function(sim_name, 
                           pop_start,
                           segment_df, 
                           mar = NULL,
                           ancestry = NULL,
                           fert_multiplier = 1) {
  
  ##  Create some additional things where necessary ##
  
  # starting month, so we can track the overall time in the sim
  month <- max(pop_start$dob)
  
  if(is.null(mar)) {
    # start with a big marriage party!
    # use the log odds for the first segment
    lodds <- segment_df |> 
      slice(1) |> 
      select(starts_with("lodds")) |>
      unlist(use.names = TRUE)
    mar <- get_married(pop_start, lodds, month, 0)
    pop_start$marid[mar$wpid] <- mar$mid
    pop_start$mstat[mar$wpid] <- 4
    pop_start$marid[mar$hpid] <- mar$mid
    pop_start$mstat[mar$hpid] <- 4
  }
  
  if(is.null(ancestry)) {
    ancestry <- pop_start |>
      as_tibble() |>
      mutate(ancestry_group1 = as.numeric(group == 1),
             ancestry_group2 = as.numeric(group == 2),
             nearest_gen_locus = NA) |>
      select(pid, group, ancestry_group1, ancestry_group2, nearest_gen_locus)
  }
    
  ## file management ##
  
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
  
  # create rate file
  file_copy(here("simulation", "parameter_files", "basic_rates"), 
            here(folder, "basic_rates"))
  # add fertility rates
  create_fertility_rates(here(folder, "basic_rates"), fert_multiplier)
  
  ## start the sim ##
  for(i in 1:nrow(segment_df)) {
    
    # get segment specific parameters
    inheritance <- segment_df |> 
      slice(i) |> 
      select(starts_with("inherit_")) |>
      unlist(use.names = TRUE)
    
    lodds <- segment_df |> 
      slice(i) |> 
      select(starts_with("lodds")) |>
      unlist(use.names = TRUE)
    
    year <- 0
    while(year < segment_df$segment_length[i]) {
      # update sup file
      file_copy(here("simulation", "parameter_files", "group2_stub.sup"), 
                here(folder, "run.sup"), overwrite = TRUE)
      cat("\nduration", 12, "\n", file = here(folder, "run.sup"),
      append = TRUE)
      cat("include basic_rates\n", file = here(folder, "run.sup"),
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
      new_kids <- calculate_ancestry(pop, ancestry, inheritance)
      # assign back the new group to pop
      pop$group[new_kids$pid] <- new_kids$group
      # add new kids to ancestry data for next generation
      ancestry <- new_kids |>
        select(pid, group, ancestry_group1, ancestry_group2, nearest_gen_locus) |>
        bind_rows(ancestry)
      
      # create new marriages
      new_marriages <- get_married(pop, lodds, month, max(mar$mid))
      
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

get_married <- function(pop, lodds, month_current, mid_max) {
  
  # get singles
  singles <- pop |>
    filter(mstat != 4 & dod == 0) |>
    mutate(age = (month_current - dob) / 12,
           sex = factor(fem, levels = 0:1, labels = c("Male", "Female"))) |>
    select(pid, sex, group, age, marid) |>
    rename(prior = marid)
  
  # break singles into men and women - restrict age a little differently
  single_women <- singles |>
    filter(sex == "Female") |>
    filter(age >= 18 & age <= 60) |>
    rename(wpid = pid, group_w = group, age_w = age, wprior = prior) |>
    select(wpid, group_w, age_w, wprior)
  single_men <- singles |>
    filter(sex == "Male") |>
    filter(age >= 20 & age <= 70) |>
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
      choice_set <- single_men |> 
        slice_sample(n = 50) |> 
        bind_cols(x) |>
        # calculate covariates and odds ratios using Dem Research article numbers
        mutate(age_diff = age_h - age_w,
               exogamy = group_h != group_w,
               exogamy_param = case_when(
                 (group_h == 1 & group_w == 2) | 
                   (group_h == 2 & group_w == 1) ~ lodds["lodds12"],
                 (group_h == 1 & group_w == 3) | 
                   (group_h == 3 & group_w == 1) ~ lodds["lodds13"],
                 (group_h == 2 & group_w == 3) | 
                   (group_h == 3 & group_w == 2) ~ lodds["lodds23"],
                 TRUE ~ 0),
               # deal with missing values - should be no exogamy. Would be nice
               # if we could just put negative infinity here. That works for 
               # cases of exogamy because -Inf * 1 = -Inf, but for cases of 
               # non-exogamy -Inf * 0 = NaN!
               exogamy_param = ifelse(!is.na(exogamy_param), 
                                      exogamy_param,
                                      ifelse(exogamy, -Inf, 0)),
               or = exp(0.072 * age_diff - 0.014 * age_diff^2 +
                          exogamy_param * exogamy))
      
      # How good are the choices?
      # if the maximum odds ratio is low, then that suggests we didn't have
      # a lot of good choices and so we will wait. We will also wait if
      # the absolute number of choices is below a threshold.
      # this helps to avoid mismatching in situations of data sparseness.
      if(nrow(choice_set) < 4 | max(choice_set$or) < 0.12) {
        return(NULL)
      }
      
      # pick a partner!
      # we don't need the actual probabilities because weights will be 
      # standardized in slice_sample which amounts to the same thing
      return(slice_sample(choice_set, n = 1, weight_by = or))
    }) |> 
    bind_rows() |>
    # some men will be chosen multiple times (lucky!), get rid of duplicates
    # try again, ladies!
    filter(!duplicated(hpid))
  
  # clean up a bit and return
  matches <- matches |>
    mutate(mid = mid_max+1:nrow(matches),
           dstart = month_current,
           dend = 0,
           rend = 16) |>
    select(mid, wpid, hpid, dstart, dend, rend, wprior, hprior)
  
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
                               inheritance) {
  
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
  new_kids <- pop |> filter(group == 4) |>
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
  
  # assign groups to kid, because each kid will need to be sampled
  # at different probabilities, we need to split and map this
  new_kids <- new_kids |>
    group_by(pid) |>
    group_split() |>
    map(function(x) {
      if(x$group_mom == x$group_pop) {
        x$group <- x$group_mom
      } else {
        # calculate probability weights for the sampling
        lor <- c(
          inheritance["inherit_g1_intercept"]+
            inheritance["inherit_g1_slope"] * x$ancestry_group1,
          inheritance["inherit_g2_intercept"]+
            inheritance["inherit_g2_slope"] * x$ancestry_group1,
          0)
        probs <- exp(lor)/sum(exp(lor))  
        # now sample a group
        x$group <- sample(1:3, 1, prob = probs)
      }
      return(x)
    }) |>
    bind_rows()
  
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
                                            range = paste(sim_name, "A8:H1000", 
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
