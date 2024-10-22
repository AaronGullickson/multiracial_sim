# test change

library(here)
source(here("utils", "check_packages.R"))

base_folder = here("simulation", "sims")
seed <- 42

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

# Add random dates of birth (max age around 70)
presim.opop$dob <- sample(360:1200, nrow(presim.opop), replace = T)

## Create an empty data frame for presim.omar
presim.omar <- data.frame()

# Write initial population for pre-simulation (without fertility multiplier)
#write.table(presim.opop, "presim.opop", row.names = F, col.names = F)

# Write empty omar for pre-simulation
#write.table(presim.omar, "presim.omar", row.names = F, col.names = F)


# Basic test --------------------------------------------------------------

dir_delete(here(base_folder, "baseline"))

folder <- create_simulation_folder(simulation_name = "baseline", 
                                   basefolder = base_folder)

# populate folder
write.table(presim.opop, here(folder, "presim.opop"), 
            row.names = F, col.names = F)
write.table(data.frame(), here(folder, "presim.omar"), 
            row.names = F, col.names = F)

file_copy(here("simulation", "rates", "basic_rates"), 
          here(folder, "basic_rates"))
file_copy(here("simulation", "supfiles", "baseline.sup"), 
          here(folder, "baseline.sup"))

create_fertility_rates(here(folder, "basic_rates"), 1.05)


socsim(folder, "baseline.sup", seed)