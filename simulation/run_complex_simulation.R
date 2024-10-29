
# Load libraries and basics ----------------------------------------------

library(here)
source(here("utils", "check_packages.R"))

base_folder = here("simulation", "sims")
seed <- 42

fert_multiplier <- 1.09

# turn off scientific notation for correct writing out of fertility rates
options(scipen = 999)

# Functions --------------------------------------------------------------

# this function helps tune the 2000 US fertility rates with a simple 
# multiplier to get a roughly stationary population
create_fertility_rates <- function(file, multiplier) {
  fert <- read_table(here("simulation", "rates", "fertility_rates"), 
                     col_names = c("age", "not_sure", "rate"),
                     col_types = cols(age = "i", 
                                      not_sure = "i",
                                      rate = "d"),
                     comment = "*") 
  # turn into characters to avoid scientific notation
  fert$rate <- as.character(fert$rate * multiplier)
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

# for testing
#sim_name <- "test"
#pop_start <- presim_even.opop
#segments <- rep(10, 30)
#endogamy <- rep(0.999, 30)
#inheritance <- rep("hypodescent", 30)

run_simulation <- function(sim_name, 
                           pop_start, 
                           segments, 
                           endogamy,
                           inheritance,
                           fert_multiplier) {
  
  if(dir_exists(here(base_folder, sim_name))) {
    dir_delete(here(base_folder, sim_name))
  }
  
  folder <- create_simulation_folder(simulation_name = sim_name, 
                                     basefolder = base_folder)
  
  # presim files
  write.table(pop_start, here(folder, "presim.opop"), 
              row.names = F, col.names = F)
  write.table(data.frame(), here(folder, "presim.omar"), 
              row.names = F, col.names = F)
  
  # create initial ancestry dataset
  ancestry <- pop_start |>
    as_tibble() |>
    mutate(ancestry_group1 = as.numeric(group == 1),
           ancestry_group2 = as.numeric(group == 2),
           nearest_gen_locus = NA) |>
    select(pid, group, ancestry_group1, ancestry_group2, nearest_gen_locus)
  
  # create rate file
  file_copy(here("simulation", "rates", "basic_rates"), 
            here(folder, "basic_rates"))
  # add fertility rates
  create_fertility_rates(here(folder, "basic_rates"), fert_multiplier)
  
  for(i in 1:length(segments)) {
    
    # update sup file
    file_copy(here("simulation", "supfiles", "group2_stub.sup"), 
              here(folder, "run.sup"), overwrite = TRUE)
    cat("\nduration", segments[i]*12, "\n", file = here(folder, "run.sup"),
        append = TRUE)
    cat("include basic_rates\n", file = here(folder, "run.sup"),
        append = TRUE)
    cat("endogamy", endogamy[i], "\n", file = here(folder, "run.sup"),
        append = TRUE)
    cat("run\n", file = here(folder, "run.sup"),
        append = TRUE)
    
    # run the simulation
    socsim(folder, "run.sup", seed = seed)
    
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
    
    # make the result of last run the new presim
    write.table(pop, here(folder, "presim.opop"), 
                row.names = F, col.names = F)
    write.table(mar, here(folder, "presim.omar"), 
                row.names = F, col.names = F)
  }
  
  # write out final results
  write_csv(pop, here(folder, "final_pop.csv"))
  write_csv(mar, here(folder, "final_mar.csv"))
  write_csv(ancestry, here(folder, "ancestry.csv"))
  
}


calculate_ancestry <- function(pop, 
                               ancestry, 
                               inheritance_method = "random") {
  
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
  
  # assign group to kid depending on method
  if(inheritance_method == "hypodescent") {
    new_kids <- new_kids |>
      mutate(group = ifelse(group_mom == 2 | group_pop == 2, 2, 1))
  } else if(inheritance_method == "hyperdescent") {
    new_kids <- new_kids |>
      mutate(group = ifelse(group_mom == 1 | group_pop == 1, 1, 2))
  } else {
    # randomly assign group
    new_kids <- new_kids |>
      mutate(
        group = case_when(
          group_mom == 1 & group_pop == 1 ~ 1,
          group_mom == 2 & group_pop == 2 ~ 2,
          TRUE ~ 3))
    new_kids$group[new_kids$group == 3] <- sample(1:2, replace = TRUE, 
                                                  size = sum(new_kids$group == 3))
  }
  
  return(new_kids)
  
}

# Create starter pop ------------------------------------------------------

# Set size of initial population
size_opop <-  30000

# Create data.frame with 14 columns and nrows = size_opop
presim_even.opop <- setNames(data.frame(matrix(data = 0, ncol = 14, nrow = size_opop)), 
                        c("pid","fem","group","nev","dob","mom","pop",
                          "nesibm","nesibp","lborn","marid","mstat","dod","fmult"))

# Add pid 1:sizeopop
presim_even.opop$pid <- 1:size_opop

# Add sex randomly
presim_even.opop$fem <- sample(0:1, nrow(presim_even.opop), replace = T)

# Add random dates of birth (max age around 70)
presim_even.opop$dob <- sample(360:1200, nrow(presim_even.opop), replace = T)

# sample between two groups 
presim_uneven.opop <- presim_highly_uneven.opop <- presim_even.opop

presim_even.opop$group <- sample(1:2, nrow(presim_even.opop), replace = T,
                            prob = c(0.5, 0.5))

presim_uneven.opop$group <- sample(1:2, nrow(presim_uneven.opop), replace = T,
                            prob = c(0.75, 0.25))

presim_highly_uneven.opop$group <- sample(1:2, nrow(presim_highly_uneven.opop), replace = T,
                            prob = c(0.9, 0.1))



# Run simulations ---------------------------------------------------------

run_simulation("test", 
               presim_even.opop, 
               segments = rep(10, 30),
               endogamy = rep(0.999, 30),
               inheritance = rep("random", 30),
               fert_multiplier = fert_multiplier)
