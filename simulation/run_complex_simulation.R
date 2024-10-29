
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
  
  # rate file
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
    socsim(folder, "run.sup", seed = seed)
    
    # make the result of last run the new presim
    pop <- rsocsim::read_opop(folder, "run.sup", seed)
    mar <- rsocsim::read_omar(folder, "run.sup", seed)
    
    # TODO: calculate ancestry of new kids and assign to a group
    
    write.table(pop, here(folder, "presim.opop"), 
                row.names = F, col.names = F)
    write.table(mar, here(folder, "presim.omar"), 
                row.names = F, col.names = F)
    
    
  }                       
  
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


