library(here)

source(here("utils", "check_packages.R"))

base_folder = here("simulation", "sims")
seed <- 42


# Basic test --------------------------------------------------------------

folder <- create_simulation_folder(simulation_name = "test", 
                                   basefolder = base_folder)
supfile <-  create_sup_file(folder)

socsim(folder, supfile, seed)

