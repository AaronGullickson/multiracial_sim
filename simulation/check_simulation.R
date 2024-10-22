library(here)
source(here("utils", "check_packages.R"))
sim_folder <- "baseline"
sim_name <- "sim_results_baseline.sup_42_"
base_folder = here("simulation", "sims", sim_folder, sim_name)

pop <- read_table(here(base_folder, "result.opop"), 
                  col_names = c("pid", "fem", "group", "nev", "dob", "mom",
                                "pop", "nesibm", "nesibp", "lborn", 
                                "marid", "mstat", "dod", "fmult"))


# plot an age pyramid at a certain date

plot_pop_pyramid <- function(date, age_width = 5) {
  pop |>
    filter(dob <= date & (dod == 0 | dod > date)) |>
    mutate(age = floor((date - dob) / 12),
           age_group = factor(floor(age / 5) * 5),
           sex = factor(fem, levels = 0:1, labels = c("Male", "Female"))) |>
    select(sex, age, age_group) |> 
    group_by(sex, age_group) |>
    summarize(n = n()) |>
    ungroup() |>
    mutate(n = ifelse(sex == "Female", -n, n)) |>
    ggplot(aes(x = factor(age_group), y  = n, fill = sex))+
    geom_col()+
    coord_flip()
}

year_range <- 1200:3600

total_pop <- map_vec(year_range, function(x) {
  pop |> filter(dob <= x & (dod == 0 | dod > x)) |> nrow()
})

tibble(year_range, total_pop) |>
  ggplot(aes(x = year_range, y = total_pop))+
  geom_line()

asfr_sim <- rsocsim::estimate_fertility_rates(opop = pop,
  final_sim_year = 300, #[Jan-Dec]
  year_min = 100, # Closed [
  year_max = 300, # Open )
  year_group = 10, 
  age_min_fert = 10, # Closed [
  age_max_fert = 55, # Open )
  age_group = 5) #[,)

ggplot(asfr_sim, aes(x = age, y = socsim, group = year, color = year))+
  geom_line()

asfr_sim |>
  group_by(year) |>
    summarize(tfr = sum(5 * socsim))
