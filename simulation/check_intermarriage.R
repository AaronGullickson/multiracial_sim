library(here)
source(here("utils", "check_packages.R"))

get_marriages <- function(base_folder) {

  pop <- read_table(here(base_folder, "result.opop"), 
                  col_names = c("pid", "fem", "group", "nev", "dob", "mom",
                                "pop", "nesibm", "nesibp", "lborn", 
                                "marid", "mstat", "dod", "fmult"))

# have to add the junk variable due to trailing zeroes
  mar <- read_table(here(base_folder, "result.omar"),
                    col_names = c("mid", "wpid", "hpid", "dstart", "dend",
                                  "rend", "wprior", "hprior", "junk"))

  husband <- pop |>
    filter(fem == 0) |>
    select(pid, group) |>
    rename(hpid = pid, hgroup = group)
                                
  wife <- pop |>
    filter(fem == 1) |>
    select(pid, group) |>
    rename(wpid = pid, wgroup = group)
                                
  marriages <- mar |>
    select(mid, wpid, hpid, dstart) |>
    left_join(husband) |>
    left_join(wife) |>
    filter(dstart < 3600) |>
    mutate(year = floor((dstart - 1200) / 12),
    time_period = cut(year, 
                      breaks = c(0, 50, 150, 160, 170, 180, 190, 200), 
                      right = FALSE)) |>
    select(mid, year, time_period, hgroup, wgroup)
  
  return(marriages)
}

base_folder <- here("simulation", "sims")
marriages_even_high <- get_marriages(here(base_folder, "group2_even_high",
                                     "sim_results_group2_high.sup_42_")) |>
  mutate(group = "even, high")

marriages_even_low <- get_marriages(here(base_folder, "group2_even_low",
                                     "sim_results_group2_low.sup_42_"))  |>
  mutate(group = "even, low")

marriages_uneven_high <- get_marriages(here(base_folder, "group2_uneven_high",
                                     "sim_results_group2_high.sup_42_"))  |>
  mutate(group = "uneven, high")

marriages_uneven_low <- get_marriages(here(base_folder, "group2_uneven_low",
                                     "sim_results_group2_low.sup_42_"))  |>
  mutate(group = "uneven, low")

marriages_highly_uneven_high <- get_marriages(here(base_folder, "group2_highly_uneven_high",
                                     "sim_results_group2_high.sup_42_")) |>
  mutate(group = "highly uneven, high")

marriages_highly_uneven_low <- get_marriages(here(base_folder, "group2_highly_uneven_low",
                                     "sim_results_group2_low.sup_42_")) |>
  mutate(group = "highly uneven, low")

marriages <- bind_rows(marriages_even_high, marriages_even_low,
            marriages_uneven_high, marriages_uneven_low,
            marriages_highly_uneven_high, marriages_highly_uneven_low)

tab_lor <- marriages |>
  group_by(time_period, group) |>
  summarize(n12 = sum(hgroup == 1 & wgroup ==2),
            n21 = sum(hgroup == 2 & wgroup ==1),
            n11 = sum(hgroup == 1 & wgroup ==1),
            n22 = sum(hgroup == 2 & wgroup ==2),
            lor = log((n12 * n21) / (n11 * n22))) |>
  select(time_period, group, lor) |>
  filter(lor > -Inf)

ggplot(tab_lor, aes(x = time_period, y = lor, group = group, color = group))+
  geom_line()+
  geom_point()

ggplot(tab_lor, aes(x = time_period, y = exp(lor), group = group, color = group))+
  geom_line()+
  geom_point()
