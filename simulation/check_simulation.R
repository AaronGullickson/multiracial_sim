# Load stuff -------------------------------------------------------------

library(here)
source(here("utils", "check_packages.R"))
sim_folder <- "baseline"
sim_name <- "sim_results_baseline.sup_42_"
base_folder = here("simulation", "sims", sim_folder, sim_name)

pop <- read_table(here(base_folder, "result.opop"), 
                  col_names = c("pid", "fem", "group", "nev", "dob", "mom",
                                "pop", "nesibm", "nesibp", "lborn", 
                                "marid", "mstat", "dod", "fmult"))

# have to add the junk variable due to trailing zeroes
mar <- read_table(here(base_folder, "result.omar"),
                  col_names = c("mid", "wpid", "hpid", "dstart", "dend",
                                "rend", "wprior", "hprior", "junk"))

#### Population pyramids ---------------------------------------------------

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

plot_pop_pyramid(1200)
plot_pop_pyramid(1800)
plot_pop_pyramid(3000)

#### Total Population Growth ---------------------------------------------

year_range <- 1200:3000

total_pop <- map_vec(year_range, function(x) {
  pop |> filter(dob <= x & (dod == 0 | dod > x)) |> nrow()
})

tibble(year_range, total_pop) |>
  mutate(year = (year_range - 1200) / 12) |>
  ggplot(aes(x = year, y = total_pop))+
  geom_line()+
  geom_vline(xintercept = 50, linetype = 2)

#### Fertility Rates ------------------------------------------------------

asfr_sim <- rsocsim::estimate_fertility_rates(opop = pop,
  final_sim_year = 150, #[Jan-Dec]
  year_min = 0, # Closed [
  year_max = 150, # Open )
  year_group = 10, 
  age_min_fert = 10, # Closed [
  age_max_fert = 55, # Open )
  age_group = 5) #[,)

ggplot(asfr_sim, aes(x = age, y = socsim, group = year, color = year))+
  geom_line()

asfr_sim |>
  group_by(year) |>
    summarize(tfr = sum(5 * socsim))

