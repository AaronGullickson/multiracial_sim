# functions.R

# Demographic summary functions ---------------------------------------------

# functions shared across scripts are placed here
plot_pop_pyramid <- function(pop, date, age_width = 5) {
  dat_pyramid <- pop |>
    filter(dob <= date & (dod == 0 | dod > date)) |>
    mutate(age = floor((date - dob) / 12),
           age_group = cut(age, seq(from = 0, 
                                    by = age_width, 
                                    length.out = ceiling(max(age) / age_width)+2), 
                           right = FALSE),
           #age_group = factor(floor(age / 5) * 5),
           sex = factor(fem, levels = 0:1, labels = c("Male", "Female"))) |>
    select(sex, age, age_group) |> 
    group_by(sex, age_group) |>
    summarize(n = n()) |>
    ungroup() |>
    mutate(n = ifelse(sex == "Female", -n, n)) 
  
  max_value <- max(abs(range(dat_pyramid$n)))
  pop_breaks <- pretty(c(-max_value, max_value))
  
  ggplot(dat_pyramid, aes(x = factor(age_group), y  = n, fill = sex))+
    geom_col()+
    scale_y_continuous(breaks = pop_breaks, labels = abs(pop_breaks))+
    labs(y = NULL, x = "age group")+
    coord_flip()+
    theme_minimal()
}

get_marriages <- function(pop, mar) {

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
    select(mid, dstart, hgroup, wgroup)
  
  return(marriages)
}

calculate_lor <- function(marriages) {
  marriages |>
    group_by(time_period) |>
    summarize(n12 = sum(hgroup == 1 & wgroup ==2),
              n21 = sum(hgroup == 2 & wgroup ==1),
              n11 = sum(hgroup == 1 & wgroup ==1),
              n22 = sum(hgroup == 2 & wgroup ==2),
              lor = log((n12 * n21) / (n11 * n22))) |>
    select(time_period, lor) |>
    filter(lor > -Inf)
}

# Genealogical functions --------------------------------------------------

# create a dataset taht gives us pairing information of parents for each 
# person in the population.
get_parent_info <- function(pop) {
  
  # the zero ids for mom and dad in the data should  be NAs to clearly
  # identify when we have hit the founder population
  pop <- pop |>
    mutate(mom = ifelse(mom == 0, NA, mom),
          dad = ifelse(dad == 0, NA, dad))


  # create a dataset that gives us mom and dad information for each current
  # person, based on mom and dad
  moms <- pop |>
    filter(fem == 1) |>
    select(pid, group) |>
    rename(mom = pid, mom_group = group)

  dads <- pop |>
    filter(fem == 0) |>
    select(pid, group) |>
    rename(dad = pid, dad_group = group)
                                
  pop |>
    left_join(moms) |>
    left_join(dads) |>
    select(pid, fem, group, dob, dod, mom, dad, mom_group, dad_group) |>
    mutate(intermar = mom_group != dad_group)
}

# A function to grab all the next generation of parent info's from a single
# parent info record
get_next_ancestor_pairings <- function(record, parent_info) {
  next_ancestors <- parent_info[c(record$mom, record$dad),]
  # using the slightly more awkward coding above which depends on 
  # actual position because the more elegant code below does not 
  # correctly add people twice when they are duplicated in mom or dad
  # vector due to inbreeding somewhere in the line. Might be a safer
  # way to do this as I don't like depending on position.
  #next_ancestors <- pairings |> 
  #  filter(pid %in% record$mom | pid %in% record$dad)
  return(next_ancestors)
}

get_all_ancestors <- function(id, parent_info) {
  current_ancestors <- parent_info |> filter(pid == id)
  gen <- 1
  current_ancestors$gen <- gen
  ancestors <- current_ancestors
  while(nrow(current_ancestors) > 0) {
    gen <- gen + 1
    next_ancestors <- get_next_ancestor_pairings(current_ancestors, parent_info)
    next_ancestors$gen <- gen
    ancestors <- ancestors |> bind_rows(next_ancestors)
    current_ancestors <- next_ancestors |>
      filter(!is.na(mom))
  }
  
  return(ancestors)
}

# a function based on a pid value to collect all ancestors and calculate summary 
# stats
get_ancestry_summary <- function(id, parent_info) {
  ancestors <- get_all_ancestors(id, parent_info)
  
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
