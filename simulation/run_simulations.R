# test change

library(here)

source(here("utils", "check_packages.R"))

base_folder = here("simulation", "sims")
seed <- 42


# Create starter pop ------------------------------------------------------

# Set size of initial population
size_opop <-  20000

# Create data.frame with 14 columns and nrows = size_opop
presim.opop <- setNames(data.frame(matrix(data = 0, ncol = 14, nrow = size_opop)), 
                        c("pid","fem","group","nev","dob","mom","pop",
                          "nesibm","nesibp","lborn","marid","mstat","dod","fmult"))

# Add pid 1:sizeopop
presim.opop$pid <- 1:size_opop

# Add sex randomly
presim.opop$fem <- sample(0:1, nrow(presim.opop), replace = T)

# sample between two groups 
presim.opop$group <- sample(1:2, nrow(presim.opop), replace = T)

# Add random dates of birth (max age around 50)
presim.opop$dob <- sample(600:1200, nrow(presim.opop), replace = T)

## Create an empty data frame for presim.omar
presim.omar <- data.frame()

# Write initial population for pre-simulation (without fertility multiplier)
#write.table(presim.opop, "presim.opop", row.names = F, col.names = F)

# Write empty omar for pre-simulation
#write.table(presim.omar, "presim.omar", row.names = F, col.names = F)


# Basic test --------------------------------------------------------------

folder <- create_simulation_folder(simulation_name = "test", 
                                   basefolder = base_folder)
supfile <-  create_sup_file(folder)

socsim(folder, supfile, seed)

# Two-Group Test ----------------------------------------------------------

folder <- create_simulation_folder(simulation_name = "group2", 
                                   basefolder = base_folder)

# populate folder
write.table(presim.opop, here(folder, "presim.opop"), 
            row.names = F, col.names = F)
write.table(data.frame(), here(folder, "presim.omar"), 
            row.names = F, col.names = F)

dir.create(here(folder, "rates"))
rate_files <- list.files(here("simulation", "rates"))
file.copy(here("simulation", "rates", rate_files), 
          here(folder, "rates", rate_files))
file.copy(here("simulation", "supfiles", "group2.sup"),
          here(folder, "group2.sup"))


socsim(folder, "group2.sup", seed)
