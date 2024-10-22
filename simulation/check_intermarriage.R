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

#### Intermarriage --------------------------------------------------------

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
  mutate(year = floor((dstart - 1200) / 12),
         decade = floor(year / 10) * 10) |>
  select(mid, year, decade, hgroup, wgroup)

tab <- table(marriages$hgroup, marriages$wgroup, marriages$decade)

# according to my paper, pre-civil rights period, this should be in the -12 to 
# -16 ballpark
lor <- log((tab[1,2,] * tab[2,1,])/(tab[1,1,] * tab[2,2,]))


