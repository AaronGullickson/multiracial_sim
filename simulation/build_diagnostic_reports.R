# This script will build the parameterized diagnostic reports for all of the 
# simulations and place them in the _products directory

# Load libraries and basics ----------------------------------------------

library(here)
source(here("utils", "check_packages.R"))
source(here("utils", "functions.R"))

# check for existence of output directory
output_path <- here("_products", "sim_diagnostics")
if(!dir_exists(output_path)) {
  dir_create(output_path)
}


# Make table of sims ------------------------------------------------------

# put all simulations you want a report for into this vector 

sim_names <- c("even_hypo_baseline",
               "even_hyper_baseline",
               "even_random_baseline",
               "uneven_hypo_baseline",
               "uneven_hyper_baseline",
               "uneven_random_baseline",
               "uneven_hypo_increase",
               "uneven_hypo_increase_change")

# Render the reports ------------------------------------------------------

# annoyingly, I have to set the working directory here to get it to work. 
# setting execute_dir argument does not work.
setwd(here("simulation"))

for(sim_name in sim_names) {
  report_name <- paste("diagnostics_", sim_name, ".html", sep="")
  quarto_render(input = here("simulation", "check_simulation.qmd"), 
                output_format = "html",
                output_file = report_name,
                execute_params = list(sim = sim_name))
  
  # annoyingly, it will not put them where they belong, so lets move the report
  # manually over to products
  file_move(here("simulation", report_name), output_path)
}

# set working directory back
setwd(here(""))


