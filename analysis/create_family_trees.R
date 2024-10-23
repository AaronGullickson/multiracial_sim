library(here)
source(here("utils", "check_packages.R"))
sim_folder <- "group2_even_low"
sim_name <- "sim_results_group2_low.sup_42_"
base_folder = here("simulation", "sims", sim_folder, sim_name)

pop <- read_table(here(base_folder, "result.opop"), 
                  col_names = c("pid", "fem", "group", "nev", "dob", "mom",
                                "dad", "nesibm", "nesibp", "lborn", 
                                "marid", "mstat", "dod", "fmult"))

mar <- read_table(here(base_folder, "result.omar"),
                  col_names = c("mid", "wpid", "hpid", "dstart", "dend",
                                "rend", "wprior", "hprior", "junk"))


# the zero ids for mom and dad in the data should probably be NAs
pop <- pop |>
  mutate(mom = ifelse(mom == 0, NA, mom),
         dad = ifelse(dad == 0, NA, dad))

# you can iterate to further ancestors with:
#pop$mom_mom <- pop$mom[pop$mom]
#pop$mom_mom_mom <- pop$mom[pop$mom_mom]

# but this would be extremely tedious over the number of generations we have
# so we need some way to quickly iterate this

# try to figure out genealogy of one random person toward the tail end of the 
# sim and that might help us see how to code this efficiently for everyone

# take case 124470

# first create a dataset that gives us pairing information for each current
# person, based on mom and dad
moms <- pop |>
  filter(fem == 1) |>
  select(pid, group) |>
  rename(mom = pid, mom_group = group)

dads <- pop |>
  filter(fem == 0) |>
  select(pid, group) |>
  rename(dad = pid, dad_group = group)
                                
pairings <- pop |>
  left_join(moms) |>
  left_join(dads) |>
  select(pid, fem, group, dob, dod, mom, dad, mom_group, dad_group) |>
  mutate(intermar = mom_group != dad_group)

# create a function to grab all the next generation of pairings from a 
# pairing record
get_next_ancestor_pairings <- function(record) {
  next_ancestors <- pairings[c(record$mom, record$dad),]
  # using the slightly more awkward coding above which depends on 
  # actual position because the more elegant code below does not 
  # correctly add people twice when they are duplicated in mom or dad
  # vector due to inbreeding somewhere in the line. Might be a safer
  # way to do this as I don't like depending on position.
  #next_ancestors <- pairings |> 
  #  filter(pid %in% record$mom | pid %in% record$dad)
  return(next_ancestors)
}

# create a function based on a pid value to collect all ancestors and 
# calculate summary stats
get_ancestry_summary <- function(id) {
  current_ancestors <- pairings |> filter(pid == id)
  gen <- 1
  current_ancestors$gen <- gen
  ancestors <- current_ancestors
  while(nrow(current_ancestors) > 0) {
    gen <- gen + 1
    next_ancestors <- get_next_ancestor_pairings(current_ancestors)
    next_ancestors$gen <- gen
    ancestors <- ancestors |> bind_rows(next_ancestors)
    current_ancestors <- next_ancestors |>
      filter(!is.na(mom))
  }

  orig_ancestors <- ancestors |>
  filter(is.na(mom)) |>
  mutate(ancestry_fraction = 1/(2^(gen-1)),
         ancestry_group1 = ifelse(group == 1, ancestry_fraction, 0)) |>
  select(pid, fem, group, gen, starts_with("ancestry_"))

  ancestors_intermar <- ancestors |>
    filter(intermar)

  return(tibble(pid = id, 
                ancestry_fraction = sum(orig_ancestors$ancestry_fraction),
                ancestry_group1 = sum(orig_ancestors$ancestry_group1),
                ancestry_group2 = 1 - ancestry_group1,
                nearest_gen_locus = ifelse(nrow(ancestors_intermar) == 0, 
                                           NA, 
                                           min(ancestors_intermar$gen))))

}

#get_ancestry_summary(124469)

# get all non-founders
descendants <- pop |> filter(!is.na(mom))

# test time and correctness
test <- descendants[sample(1:nrow(temp), 200, replace = TRUE),]
system.time(
x <- map(test$pid, get_ancestry_summary) |>
  bind_rows())

ancestry_summary <- map(pop$pid, get_ancestry_summary) |>
  bind_rows()
