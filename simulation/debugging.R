# This script will read in simulation parameter data from an external
# google sheet located here:
# https://docs.google.com/spreadsheets/d/18jeYYzzQxIGWYdt7H9VWyo1T1yrUQt_jlydeE2f2uCs/edit?gid=654203504#gid=654203504
# Each simulation is tracked on a different tab in the overall file. 
# The script will run each simulation and the diagnostic report and place the 
# output in the data_constructed and _products directory, respectively.

# only run this script when we are rendering the entire project
# comment out to source this script interactively
if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  #quit()
}


library(here)
source(here("utils", "check_packages.R"))
source(here("utils", "functions.R"))
source(here("simulation", "simulation_functions.R"))


# Set up base information -------------------------------------------------

# base folder for the sim data
base_folder = here("data", "data_constructed", "sims")
# remove old base folder and create new one
if(dir_exists(base_folder)) {
  dir_delete(base_folder)
}
dir_create(base_folder)

# output path for the reports
output_path <- here("_products", "sim_diagnostics")
if(!dir_exists(output_path)) {
  dir_create(output_path)
}

# lets randomize the seed each time this is run to see how much results vary
seed <- sample(1:100, 1)
set.seed(seed)

# set up google sheet for reading
sheet_id <- "1ad-fJUCjRy_zslI2MMce8UaolepG536-IZr1yNNIuZ8"
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


# Run the sims ------------------------------------------------------------

sim_names <- googlesheets4::sheet_names(sheet_id)

for(sim_name in sim_names) {

  if(str_detect(sim_name, "IGNORE$")) {
    next
  }

  # get sim parameters
  sim_param <- get_sim_parameters(sim_name, sheet_id)
  segment_df <- sim_param$segments
  fert_multiplier <- sim_param$start$fert_multiplier
  
  # get starting data
  if(is.na(sim_param$start$starting_sim)) {
    pop_start <- presim_opop |>
      mutate(group = sample(1:2, nrow(presim_opop), replace = T, 
                            prob = c(sim_param$start$group1_prop, 
                                     1 - sim_param$start$group1_prop)))
    mar <- NULL
    ancestry <- NULL
  } else {
    pop_start <- read_csv(here(base_folder, 
                               sim_param$start$starting_sim, 
                               "final_pop.csv"))
    mar <- read_csv(here(base_folder, 
                         sim_param$start$starting_sim,
                         "final_mar.csv"))
    ancestry <- read_csv(here(base_folder, 
                              sim_param$start$starting_sim, 
                              "ancestry.csv"))
  }

  # reset future so we don't get shenanigans
  future::plan(sequential)
  gc()
  
  run_simulation(sim_name, 
                 pop_start = pop_start,
                 segment_df = segment_df,
                 mar = mar,
                 ancestry = ancestry,
                 fert_multiplier = fert_multiplier,
                 seed = seed)
  
  # Prepare diagnostic report within a try/catch
  tryCatch({
    # render report and then move it
    report_name <- paste0("diagnostics_", sim_name, ".html")
    quarto_render(input = here("simulation", "check_simulation.qmd"), 
                  output_format = "html",
                  output_file = report_name,
                  execute_params = list(sim = sim_name, sheet_id = sheet_id))
    file_move(here("simulation", report_name), here(output_path, report_name))
    
  }, error = function(e) {
    sim_path <- here(base_folder, sim_name)
    if (!dir_exists(sim_path)) {
      dir_create(sim_path)
    }
    
    log_file <- file.path(sim_path, "error_render.log")
    cat("Render Error:\n", conditionMessage(e), "\n\n", file = log_file)
    tb <- capture.output(traceback())
    cat("Traceback:\n", paste(tb, collapse = "\n"), "\n\n", file = log_file, 
        append = TRUE)
  })
}