
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


seed <- 42

sheet_id <- "18jeYYzzQxIGWYdt7H9VWyo1T1yrUQt_jlydeE2f2uCs"

# Create starter pop ------------------------------------------------------

# Set size of initial population
size_opop <-  30000

# Create data.frame with 14 columns and nrows = size_opop
presim_opop <- setNames(data.frame(matrix(data = 0, ncol = 14, nrow = size_opop)), 
                        c("pid","fem","group","nev","dob","mom","pop",
                          "nesibm","nesibp","lborn","marid","mstat","dod","fmult"))

# Add pid 1:sizeopop
presim_opop$pid <- 1:size_opop

# Add sex randomly
presim_opop$fem <- sample(0:1, nrow(presim_opop), replace = T)

# Add random dates of birth (max age around 70)
presim_opop$dob <- sample(360:1200, nrow(presim_opop), replace = T)


# Run simulations from googlesheets ---------------------------------------

sim_names <- googlesheets4::sheet_names(sheet_id)

for(sim_name in sim_names) {
  
  # read data from googlesheets
  sim_start <- googlesheets4::range_read(sheet_id, 
                                         range = paste(sim_name, "A2:C3", 
                                                       sep="!"))
  
  sim_segments <- googlesheets4::range_read(sheet_id, 
                                            range = paste(sim_name, "A6:C1000", 
                                                          sep="!")) |>
    filter(!is.na(segment_length))
  
  # get starting pop stuff
  if(is.na(sim_start$starting_sim)) {
    pop_start <- presim_opop |>
      mutate(group = sample(1:2, nrow(presim_opop), replace = T, 
                            prob = c(sim_start$group1_prop, 
                                     1 - sim_start$group1_prop)))
    mar_start <- NULL
    ancestry_start <- NULL
  } else {
    pop_start <- read_csv(here(base_folder, sim_start$starting_sim, 
                               "final_pop.csv"))
    mar_start <- read_csv(here(base_folder, sim_start$starting_sim,
                               "final_mar.csv"))
    ancestry_start <- read_csv(here(base_folder, sim_start$starting_sim, 
                                    "ancestry.csv"))
  }
  
  # run the simulation
  run_simulation(sim_name, 
                 pop_start = pop_start,
                 mar = mar_start,
                 ancestry = ancestry_start,
                 segments = sim_segments$segment_length,
                 endogamy = sim_segments$endogamy,
                 inheritance = sim_segments$inheritance)
  
  # now create the report
  # annoyingly, I have to set the working directory here to get it to work. 
  # setting execute_dir argument does not work.
  setwd(here("simulation"))
  report_name <- paste("diagnostics_", sim_name, ".html", sep="")
  quarto_render(input = here("simulation", "check_simulation.qmd"), 
                output_format = "html",
                output_file = report_name,
                execute_params = list(sim = sim_name))
  # annoyingly, it will not put them where they belong, so lets move the report
  # manually over to products
  file_move(here("simulation", report_name), output_path)
  # set working directory back
  setwd(here(""))
  
}