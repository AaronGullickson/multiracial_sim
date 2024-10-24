library(here)
source(here("utils", "check_packages.R"))
sim_folder <- "group2_even_high"
sim_name <- "sim_results_group2_high.sup_42_"
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

ancestry_summary <- map(descendants$pid, get_ancestry_summary) |>
  bind_rows()

analytical_data <- descendants |>
  left_join(ancestry_summary) |>
  mutate(sex = factor(fem, levels = 0:1, labels = c("Male", "Female")),
         dob = dob / 12, 
         dod = dod /12,
         mixedness = ifelse(ancestry_group1 >= ancestry_group2,
                            ancestry_group1, ancestry_group2),
         biracial = !is.na(nearest_gen_locus) & nearest_gen_locus == 1) |>
  select(pid, sex, group, dob, dod, starts_with("ancestry_"), 
         nearest_gen_locus, biracial, mixedness)

ggplot(analytical_data, aes(x = ancestry_group1))+
  geom_histogram()

ggplot(analytical_data, aes(x = nearest_gen_locus))+
  geom_histogram()

analytical_data |>
  mutate(decade = floor(dob / 10) * 10) |>
  group_by(decade) |>
  summarize(nearest_gen_locus = mean(nearest_gen_locus, na.rm = TRUE)) |>
  ggplot(aes(x = decade, y = nearest_gen_locus))+
  geom_point()+
  geom_line()

analytical_data |>
  mutate(decade = floor(dob / 10) * 10) |>
  ggplot(aes(x = nearest_gen_locus))+
  geom_histogram()+
  facet_wrap(~decade)

fractions <- tibble(nearest_gen_locus = 1:9,
                    mixedness = 1-1/(2^(1:9)))

actual_fractions <- analytical_data |>
  group_by(nearest_gen_locus) |>
  summarize(mixedness = mean(mixedness))

analytical_data |>
  filter(!is.na(nearest_gen_locus)) |>
  ggplot(aes(x = nearest_gen_locus, y = mixedness))+
  scale_x_continuous(breaks = 1:9)+
  geom_jitter(alpha = 0.1)+
  geom_point(data = fractions, color = "red", size = 3)+
  geom_point(data = actual_fractions, color = "blue", size = 3)+
  theme_bw()

analytical_data |>
  mutate(decade = floor(dob / 10) * 10) |>
  group_by(decade) |>
  summarize(p_biracial = mean(biracial)) |>
  ggplot(aes(x = decade, y = p_biracial))+
  geom_point()+
  geom_smooth(se = FALSE)+
  theme_bw()

# first gen as share of multiracial pop
analytical_data |>
  filter(!is.na(nearest_gen_locus)) |>
  mutate(decade = floor(dob / 10) * 10) |>
  group_by(decade) |>
  summarize(p_biracial = mean(biracial)) |>
  ggplot(aes(x = decade, y = p_biracial))+
  geom_point()+
  geom_smooth(se = FALSE)+
  theme_bw()

# first gen as share of multiracial pop, close up on stable pop
analytical_data |>
  #filter(dob > 250 & !is.na(nearest_gen_locus)) |>
  mutate(decade = floor(dob / 10) * 10) |>
  group_by(decade) |>
  summarize(p_biracial = mean(biracial)) |>
  ggplot(aes(x = decade, y = p_biracial))+
  geom_point()+
  geom_smooth(se = FALSE)+
  theme_bw()
