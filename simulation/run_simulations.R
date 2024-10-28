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

fert_multiplier <- 1.05

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

## Create an empty data frame for presim.omar
presim.omar <- data.frame()

# 2 group even, baseline ---------------------------------------------------

# create baseline even pop which will be used as the next presim for
# high and low cases

if(dir_exists(here(base_folder, "group2_even_baseline"))) {
  dir_delete(here(base_folder, "group2_even_baseline"))
}

folder <- create_simulation_folder(simulation_name = "group2_even_baseline", 
                                   basefolder = base_folder)

# populate folder
write.table(presim_even.opop, here(folder, "presim.opop"), 
            row.names = F, col.names = F)
write.table(data.frame(), here(folder, "presim.omar"), 
            row.names = F, col.names = F)

file_copy(here("simulation", "rates", "basic_rates"), 
          here(folder, "basic_rates"))
file_copy(here("simulation", "supfiles", "group2_baseline.sup"), 
          here(folder, "group2_baseline.sup"))

create_fertility_rates(here(folder, "basic_rates"), fert_multiplier)

socsim(folder, "group2_baseline.sup", seed)

# 2 group uneven, baseline -------------------------------------------------

if(dir_exists(here(base_folder, "group2_uneven_baseline"))) {
  dir_delete(here(base_folder, "group2_uneven_baseline"))
}

folder <- create_simulation_folder(simulation_name = "group2_uneven_baseline", 
                                   basefolder = base_folder)

# populate folder
write.table(presim_uneven.opop, here(folder, "presim.opop"), 
            row.names = F, col.names = F)
write.table(data.frame(), here(folder, "presim.omar"), 
            row.names = F, col.names = F)

file_copy(here("simulation", "rates", "basic_rates"), 
          here(folder, "basic_rates"))
file_copy(here("simulation", "supfiles", "group2_baseline.sup"), 
          here(folder, "group2_baseline.sup"))

create_fertility_rates(here(folder, "basic_rates"), fert_multiplier)

socsim(folder, "group2_baseline.sup", seed)

# 2 group highly uneven, baseline --------------------------------------------

if(dir_exists(here(base_folder, "group2_highly_uneven_baseline"))) {
  dir_delete(here(base_folder, "group2_highly_uneven_baseline"))
}

folder <- create_simulation_folder(simulation_name = "group2_highly_uneven_baseline", 
                                   basefolder = base_folder)

# populate folder
write.table(presim_highly_uneven.opop, here(folder, "presim.opop"), 
            row.names = F, col.names = F)
write.table(data.frame(), here(folder, "presim.omar"), 
            row.names = F, col.names = F)

file_copy(here("simulation", "rates", "basic_rates"), 
          here(folder, "basic_rates"))
file_copy(here("simulation", "supfiles", "group2_baseline.sup"), 
          here(folder, "group2_baseline.sup"))

create_fertility_rates(here(folder, "basic_rates"), fert_multiplier)

socsim(folder, "group2_baseline.sup", seed)

# 2 group even, high -------------------------------------------------------

if(dir_exists(here(base_folder, "group2_even_high"))) {
  dir_delete(here(base_folder, "group2_even_high"))
}

folder <- create_simulation_folder(simulation_name = "group2_even_high", 
                                   basefolder = base_folder)

# populate folder
file_copy(here("simulation", "sims", "group2_even_baseline",
               "sim_results_group2_baseline.sup_42_", "result.opop"),
          here(folder, "presim.opop"))
file_copy(here("simulation", "sims", "group2_even_baseline",
               "sim_results_group2_baseline.sup_42_", "result.omar"),
          here(folder, "presim.omar"))
file_copy(here("simulation", "rates", "basic_rates"), 
          here(folder, "basic_rates"))
file_copy(here("simulation", "supfiles", "group2_high.sup"), 
          here(folder, "group2_high.sup"))

create_fertility_rates(here(folder, "basic_rates"), fert_multiplier)

socsim(folder, "group2_high.sup", seed)

# 2 group even, low -------------------------------------------------------

if(dir_exists(here(base_folder, "group2_even_low"))) {
  dir_delete(here(base_folder, "group2_even_low"))
}

folder <- create_simulation_folder(simulation_name = "group2_even_low", 
                                   basefolder = base_folder)

# populate folder
file_copy(here("simulation", "sims", "group2_even_baseline",
               "sim_results_group2_baseline.sup_42_", "result.opop"),
          here(folder, "presim.opop"))
file_copy(here("simulation", "sims", "group2_even_baseline",
               "sim_results_group2_baseline.sup_42_", "result.omar"),
          here(folder, "presim.omar"))
file_copy(here("simulation", "rates", "basic_rates"), 
          here(folder, "basic_rates"))
file_copy(here("simulation", "supfiles", "group2_low.sup"), 
          here(folder, "group2_low.sup"))

create_fertility_rates(here(folder, "basic_rates"), fert_multiplier)


socsim(folder, "group2_low.sup", seed)

# 2 group uneven, high -------------------------------------------------------

# if(dir_exists(here(base_folder, "group2_uneven_high"))) {
#   dir_delete(here(base_folder, "group2_uneven_high"))
# }

# folder <- create_simulation_folder(simulation_name = "group2_uneven_high", 
#                                    basefolder = base_folder)

# # populate folder
# write.table(presim_uneven.opop, here(folder, "presim.opop"), 
#             row.names = F, col.names = F)
# write.table(data.frame(), here(folder, "presim.omar"), 
#             row.names = F, col.names = F)

# file_copy(here("simulation", "rates", "basic_rates"), 
#           here(folder, "basic_rates"))
# file_copy(here("simulation", "supfiles", "group2_high.sup"), 
#           here(folder, "group2_high.sup"))

# create_fertility_rates(here(folder, "basic_rates"), 1.05)


# socsim(folder, "group2_high.sup", seed)

# 2 group uneven, low -------------------------------------------------------

# if(dir_exists(here(base_folder, "group2_uneven_low"))) {
#   dir_delete(here(base_folder, "group2_uneven_low"))
# }

# folder <- create_simulation_folder(simulation_name = "group2_uneven_low", 
#                                    basefolder = base_folder)

# # populate folder
# write.table(presim_uneven.opop, here(folder, "presim.opop"), 
#             row.names = F, col.names = F)
# write.table(data.frame(), here(folder, "presim.omar"), 
#             row.names = F, col.names = F)

# file_copy(here("simulation", "rates", "basic_rates"), 
#           here(folder, "basic_rates"))
# file_copy(here("simulation", "supfiles", "group2_low.sup"), 
#           here(folder, "group2_low.sup"))

# create_fertility_rates(here(folder, "basic_rates"), 1.05)


# socsim(folder, "group2_low.sup", seed)

# 2 group highly uneven, high -------------------------------------------------------

# if(dir_exists(here(base_folder, "group2_highly_uneven_high"))) {
#   dir_delete(here(base_folder, "group2_highly_uneven_high"))
# }

# folder <- create_simulation_folder(simulation_name = "group2_highly_uneven_high", 
#                                    basefolder = base_folder)

# # populate folder
# write.table(presim_highly_uneven.opop, here(folder, "presim.opop"), 
#             row.names = F, col.names = F)
# write.table(data.frame(), here(folder, "presim.omar"), 
#             row.names = F, col.names = F)

# file_copy(here("simulation", "rates", "basic_rates"), 
#           here(folder, "basic_rates"))
# file_copy(here("simulation", "supfiles", "group2_high.sup"), 
#           here(folder, "group2_high.sup"))

# create_fertility_rates(here(folder, "basic_rates"), 1.05)


# socsim(folder, "group2_high.sup", seed)

# 2 group highly uneven, low -------------------------------------------------------

# if(dir_exists(here(base_folder, "group2_highly_uneven_low"))) {
#   dir_delete(here(base_folder, "group2_highly_uneven_low"))
# }

# folder <- create_simulation_folder(simulation_name = "group2_highly_uneven_low", 
#                                    basefolder = base_folder)

# # populate folder
# write.table(presim_highly_uneven.opop, here(folder, "presim.opop"), 
#             row.names = F, col.names = F)
# write.table(data.frame(), here(folder, "presim.omar"), 
#             row.names = F, col.names = F)

# file_copy(here("simulation", "rates", "basic_rates"), 
#           here(folder, "basic_rates"))
# file_copy(here("simulation", "supfiles", "group2_low.sup"), 
#           here(folder, "group2_low.sup"))

# create_fertility_rates(here(folder, "basic_rates"), 1.05)


# socsim(folder, "group2_low.sup", seed)
