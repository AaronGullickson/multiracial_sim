# functions for running SOCSIM simulation. The main function is run_simulation
# with a variety of helper functions


####
# This function is the big one that runs the full simulation. This simulation
# runs a single year of simulation using SOCSIM and then uses R to assign
# ancestry to new kids and marriages to existing singles. Marriages should 
# not be asssigned within SOCSIM itself (i.e. set all marriage rates to zero
# in SOCSIM). The two helper functions get_married and calculate_ancestry assign
# marriages and ancestry, respectively. Most of the important parameters for the 
# simulation are assigned by the segment_df argument.
# sim_name - the name to call this simulation. This will be the name of the 
#            folder.
# pop_start - the population data.frame to start the simulation with. This can
#             be the result of a previous simulation, but if so the mar and 
#             ancestry files must also be provided to get correct results.
# segment_df - a data.frame or tibble where each row holds the parameters for 
#              a given segment of the simulation. The required variables are:
#              segment_length - The length of this segment in years.
#              lodds12 - the log odds of intermarriage for groups 1 and 2
#              lodds13 - the log odds of intermarriage for groups 1 and 3
#              lodds23 - the log odds of intermarriage for groups 2 and 3
#              inherit_g1_intercept - the group 1 intercept for the inheritance 
#                                     model
#              inherit_g1_slope - the group 1 slope for the inheritance model
#              inherit_g2_intercept - the group 2 intercept for the inheritance 
#                                     model
#              inherit_g2_slope - the group 2 slope for the inheritance model
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
####
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

####
# This function will take existing single people and assign them to marriages,
# writing the results into a new mar file that is returned and can be added
# to the existing mar file. Marriages are determined by selecting a certain 
# number of potential male partners for every woman. Then these potential
# marriages are scored by a log-odds formula that adjusts for age differences
# and group exogamy. Those scores are used to get the probability of each union
# from which one is selected. If all the scores are low or there are only a few
# potential partners available, then the woman decides to wait. The program
# also checks for relatedness within two generations and removes those potential
# partners. Since some women will choose the same partner, not everyone will get 
# their choice. Those who get scooped will wait until next year.
# The arguments are:
# pop - the population dataset for the simulation at its current state. This is 
#       used to determine single people as well as relatedness.
# lodds - a named vector with the names "lodds12", "lodds13", and "lodds23" 
#         which gives the log odds of a union between the specified groups
#         (e.g. lodds12 is between groups 1 and 2).
# month_current - the last of month of the simulation in its current state. This 
#                 is used as the marriage date for each marriage.
# mid_max - the marriage id of the last marriage among the existing marriages
#           in the simulation. Used to determine the ids of new marriages.
####
get_married <- function(pop, lodds, month_current, mid_max) {
  
  # get singles
  singles <- pop |>
    filter(mstat != 4 & dod == 0) |>
    mutate(age = (month_current - dob) / 12,
           sex = factor(fem, levels = 0:1, labels = c("Male", "Female")),
           mom = ifelse(mom == 0, NA, mom),
           dad = ifelse(pop == 0, NA, pop)) |>
    select(pid, sex, group, age, mom, dad, marid) |>
    rename(prior = marid)
  
  # get grandparent ids, to check for cousin-ness
  maternal <- pop |>
    filter(fem == 1) |>
    mutate(mom = ifelse(mom == 0, NA, mom),
           dad = ifelse(pop == 0, NA, pop)) |>
    rename(mom = pid, gmom_mat = mom, gdad_mat = dad) |>
    select(mom, gmom_mat, gdad_mat)
  
  fraternal <- pop |>
    filter(fem == 0) |>
    select(pid, mom, pop) |>
    mutate(mom = ifelse(mom == 0, NA, mom),
           dad = ifelse(pop == 0, NA, pop)) |>
    rename(dad = pid, gmom_frat = mom, gdad_frat = dad) |>
    select(dad, gmom_frat, gdad_frat)
  
  singles <- singles |>
    left_join(maternal) |>
    left_join(fraternal)
  
  # break singles into men and women - restrict age a little differently
  # for each group
  single_women <- singles |>
    filter(sex == "Female") |>
    filter(age >= 18 & age <= 60) |>
    rename(wpid = pid, group_w = group, age_w = age, wprior = prior,
           mom_w = mom, dad_w = dad, gmom_mat_w = gmom_mat, 
           gdad_mat_w = gdad_mat, gmom_frat_w = gmom_frat, 
           gdad_frat_w = gdad_frat) |>
    select(wpid, group_w, age_w, wprior, mom_w, dad_w, starts_with("gmom_"),
           starts_with("gdad_"))
  single_men <- singles |>
    filter(sex == "Male") |>
    filter(age >= 20 & age <= 70) |>
    rename(hpid = pid, group_h = group, age_h = age, hprior = prior,
           mom_h = mom, dad_h = dad, gmom_mat_h = gmom_mat, 
           gdad_mat_h = gdad_mat, gmom_frat_h = gmom_frat, 
           gdad_frat_h = gdad_frat) |>
    select(hpid, group_h, age_h, hprior, mom_h, dad_h, starts_with("gmom_"),
           starts_with("gdad_"))
  
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
      
      # check for icky related-ness levels and remove
      # first create a vector of unacceptable shared relatives for woman
      ancestors_w <- x |> 
        select(mom_w, dad_w, starts_with(c("gmom_","gdad_"))) |> 
        unlist() |> 
        na.omit()
      # now use the if_all approach to check if any of the potential
      # husband's ancestors are the same and filter out if so
      choice_set <- choice_set |>
        filter(if_all(c(mom_h, dad_h, ends_with(c("_mat_h","_frat_h"))), 
                      ~ !(.x %in% ancestors_w)))
      
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
# inheritance - a vector containing the inheritance parameters for the 
#               multinomial model that will determine the probability of being
#               assigned to a given group. This vector must have the following
#               names for items: inherit_g1_intercept, inherit_g1_slope,
#               inherit_g2_intercept, inherit_g2_slope
####
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
