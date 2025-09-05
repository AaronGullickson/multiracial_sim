# This script will read in simulation parameter data from an external
# google sheet located here:
# https://docs.google.com/spreadsheets/d/18jeYYzzQxIGWYdt7H9VWyo1T1yrUQt_jlydeE2f2uCs/edit?gid=654203504#gid=654203504
# Each simulation is tracked on a different tab in the overall file. 
# The script will run each simulation and the diagnostic report and place the 
# output in the data_constructed and _products directory, respectively.

# only run this script when we are rendering the entire project
# comment out to source this script interactively
if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  quit()
}

# Load libraries and basics ----------------------------------------------

library(here)
source(here("utils", "check_packages.R"))
source(here("utils", "functions.R"))
source(here("simulation", "simulation_functions.R"))

# base folder for the sim data
base_folder = here("data", "data_constructed", "sims")
# check to see if base folder exists, and if not, create it
if(!dir_exists(base_folder)) {
  dir_create(base_folder)
}

# output path for the reports
output_path <- here("_products", "sim_diagnostics")
if(!dir_exists(output_path)) {
  dir_create(output_path)
}

# lets randomize the seed each time this is run to see how much results vary
seed <- sample(1:100, 1)
set.seed(seed)

# sheet id to read from on google sheets
sheet_id <- "1ad-fJUCjRy_zslI2MMce8UaolepG536-IZr1yNNIuZ8"

# de-authorize googlesheets4 so it won't ask about authorization
googlesheets4::gs4_deauth()

# Create starter pop ------------------------------------------------------

# Set size of initial population
size_opop <-  50000

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


# Test simulations --------------------------------------------------------

# sim_name <- "test"
# pop_start <- presim_opop |>
#   mutate(group = sample(1:2, nrow(presim_opop), replace = T,
#                         prob = c(0.8, 0.2)))
# segment_df <- tribble(
#   ~segment_length, ~lodds12, ~lodds13, ~lodds23, ~inherit_g1_intercept, ~inherit_g1_slope, ~inherit_g2_intercept, ~inherit_g2_slope,
#   300, -5, NA, NA, 3, 3, 10, -3
# )
# fert_multiplier <- 1.09
# mar <- NULL
# ancestry <- NULL
# 
# run_simulation("test",
#                pop_start,
#                segment_df,
#                fert_multiplier = fert_multiplier)

# Run simulations from googlesheets ---------------------------------------

sim_names <- googlesheets4::sheet_names(sheet_id)

for(sim_name in sim_names) {
  
  if(str_detect(sim_name, "IGNORE$")) {
    next
  }
  
  tryCatch(
    expr = {
      # read data from googlesheets
      sim_param <- get_sim_parameters(sim_name, sheet_id)
      
      # get starting pop stuff
      if(is.na(sim_param$start$starting_sim)) {
        pop_start <- presim_opop |>
          mutate(group = sample(1:2, nrow(presim_opop), replace = T, 
                                prob = c(sim_param$start$group1_prop, 
                                         1 - sim_param$start$group1_prop)))
        mar_start <- NULL
        ancestry_start <- NULL
      } else {
        pop_start <- read_csv(here(base_folder, 
                                   sim_param$start$starting_sim, 
                                   "final_pop.csv"))
        mar_start <- read_csv(here(base_folder, 
                                   sim_param$start$starting_sim,
                                   "final_mar.csv"))
        ancestry_start <- read_csv(here(base_folder, 
                                        sim_param$start$starting_sim, 
                                        "ancestry.csv"))
      }
      
      # run the simulation
      run_simulation(sim_name, 
                     pop_start = pop_start,
                     segment_df = sim_param$segments,
                     mar = mar_start,
                     ancestry = ancestry_start,
                     fert_multiplier = sim_param$start$fert_multiplier,
                     seed = seed)
      
      # now create the report
      # annoyingly, I have to set the working directory here to get it to work. 
      # setting execute_dir argument does not work.
      setwd(here("simulation"))
      report_name <- paste("diagnostics_", sim_name, ".html", sep="")
      quarto_render(input = here("simulation", "check_simulation.qmd"), 
                    output_format = "html",
                    output_file = report_name,
                    execute_params = list(sim = sim_name, sheet_id = sheet_id))
      # annoyingly, it will not put them where they belong, so lets move the report
      # manually over to products
      file_move(here("simulation", report_name), output_path)
      # set working directory back
      setwd(here(""))
    }, 
    error = function(err) {
      #if we hit an error here, report the error in a file.log and 
      #move on to the next simulation
      if(!dir_exists(here(base_folder, sim_name))) {
        dir_create(here(base_folder, sim_name))
      }
      cat(paste(err, sep="\n"), 
          file = here(base_folder, sim_name, "error.log"))
    })
}
