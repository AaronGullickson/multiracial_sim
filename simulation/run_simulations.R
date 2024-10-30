
# Load libraries and basics ----------------------------------------------

library(here)
source(here("utils", "check_packages.R"))
source(here("utils", "functions.R"))
source(here("simulation", "simulation_functions.R"))


base_folder = here("simulation", "sims")
seed <- 42

fert_multiplier <- 1.09

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
presim_uneven.opop <- presim_even.opop

presim_even.opop$group <- sample(1:2, nrow(presim_even.opop), replace = T,
                            prob = c(0.5, 0.5))

presim_uneven.opop$group <- sample(1:2, nrow(presim_uneven.opop), replace = T,
                            prob = c(0.8, 0.2))


# Run baseline simulations ---------------------------------------------------

run_simulation("even_hypo_baseline", 
               presim_even.opop, 
               segments = rep(10, 30),
               endogamy = rep(0.999, 30),
               inheritance = rep("hypodescent", 30),
               fert_multiplier = fert_multiplier)

run_simulation("even_hyper_baseline", 
               presim_even.opop, 
               segments = rep(10, 30),
               endogamy = rep(0.999, 30),
               inheritance = rep("hyperdescent", 30),
               fert_multiplier = fert_multiplier)

run_simulation("even_random_baseline", 
               presim_even.opop, 
               segments = rep(10, 30),
               endogamy = rep(0.999, 30),
               inheritance = rep("random", 30),
               fert_multiplier = fert_multiplier)

run_simulation("uneven_hypo_baseline", 
               presim_uneven.opop, 
               segments = rep(10, 30),
               endogamy = rep(0.999, 30),
               inheritance = rep("hypodescent", 30),
               fert_multiplier = fert_multiplier)

run_simulation("uneven_hyper_baseline", 
               presim_uneven.opop, 
               segments = rep(10, 30),
               endogamy = rep(0.999, 30),
               inheritance = rep("hyperdescent", 30),
               fert_multiplier = fert_multiplier)

run_simulation("uneven_random_baseline", 
               presim_uneven.opop, 
               segments = rep(10, 30),
               endogamy = rep(0.999, 30),
               inheritance = rep("random", 30),
               fert_multiplier = fert_multiplier)

# Run full simulations ---------------------------------------------------

# base off the uneven hypo
pop_uneven_hypo <- read_csv(here("simulation", "sims", "uneven_hypo_baseline", 
                                 "final_pop.csv"))
mar_uneven_hypo <- read_csv(here("simulation", "sims", "uneven_hypo_baseline", 
                                 "final_mar.csv"))
ancestry_uneven_hypo <- read_csv(here("simulation", "sims", 
                                      "uneven_hypo_baseline", "ancestry.csv"))

# add 100 years of increasing exogamy but no change in hypodescent
run_simulation("uneven_hypo_increase", 
               pop_uneven_hypo, 
               segments = c(rep(5, 20)),
               endogamy = seq(from = 0.989, by = -0.005, length.out = 20),
               inheritance = rep("hypodescent", 20),
               mar = mar_uneven_hypo,
               ancestry = ancestry_uneven_hypo,
               fert_multiplier = fert_multiplier)

# same as above, but change to random
run_simulation("uneven_hypo_increase_change", 
               pop_uneven_hypo, 
               segments = rep(5, 20),
               endogamy = seq(from = 0.989, by = -0.005, length.out = 20),
               inheritance = rep("random", 20),
               mar = mar_uneven_hypo,
               ancestry = ancestry_uneven_hypo,
               fert_multiplier = fert_multiplier)
