library(here)
source(here("utils", "check_packages.R"))
source(here("utils", "functions.R"))
source(here("simulation", "simulation_functions.R"))

# base folder for the sim data
base_folder = here("data", "data_constructed", "sims")
# check to see if base folder exists, and if not, create it
if(dir_exists(base_folder)) {
  dir_delete(base_folder)
}
dir_create(base_folder)

# lets randomize the seed each time this is run to see how much results vary
seed <- sample(1:100, 1)
set.seed(seed)

# Create starter pop ------------------------------------------------------

# Set size of initial population
size_opop <-  1000

# Create data.frame with 14 columns and nrows = size_opop
presim_opop <- setNames(data.frame(matrix(data = 0, ncol = 14, nrow = size_opop)), 
                        c("pid","fem","group","nev","dob","mom","pop",
                          "nesibm","nesibp","lborn","marid","mstat","dod",
                          "fmult")) |>
  as_tibble()

# Add pid 1:sizeopop
presim_opop$pid <- 1:size_opop

# Add sex randomly
presim_opop$fem <- sample(0:1, nrow(presim_opop), replace = TRUE)

# Add random dates of birth (max age around 70)
presim_opop$dob <- sample(360:1200, nrow(presim_opop), replace = TRUE)

sheet_id <- "1ad-fJUCjRy_zslI2MMce8UaolepG536-IZr1yNNIuZ8"
googlesheets4::gs4_deauth()

for(sim_name in c("uneven_hypo_baseline", "uneven_hyper_baseline")) {

  #sim_name <- "uneven_hypo_baseline"
  sim_param <- get_sim_parameters(sim_name, sheet_id)
  
  pop_start <- presim_opop |>
    mutate(group = sample(1:2, nrow(presim_opop), replace = T, 
                          prob = c(sim_param$start$group1_prop, 
                                   1 - sim_param$start$group1_prop)))
  
  mar <- NULL
  ancestry <- NULL
  fert_multiplier <- 1.15
  segment_df <- sim_param$segments
  segment_df$segment_length <- 10
  
  future::plan(sequential)  # or multisession/workers again
  gc()                      # force cleanup
  
  run_simulation(sim_name, 
                 pop_start = pop_start,
                 segment_df = segment_df,
                 mar = mar,
                 ancestry = ancestry,
                 fert_multiplier = fert_multiplier,
                 seed = seed)
}