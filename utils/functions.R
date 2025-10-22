# functions.R

# Demographic summary functions ---------------------------------------------

get_pop_alive <- function(pop, date) {
  pop |>
    filter(dob <= date & (is.na(dod) | dod > date))
}

# functions shared across scripts are placed here
plot_pop_pyramid <- function(pop, date, age_width = 5) {
  dat_pyramid <- pop |>
    get_pop_alive(date) |>
    mutate(age = floor(date - dob),
           age_group = cut(age, seq(from = 0, 
                                    by = age_width, 
                                    length.out = ceiling(max(age) / age_width)+2), 
                           right = FALSE)) |>
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

get_life_expectancy <- function(rates) {
  
  # get it started
  life_table <- rates |>
    rename(nmx = mort_rate) |>
    mutate(x = as.numeric(str_extract_all(age, "\\d+", simplify = TRUE)[,1]),
           n = as.numeric(str_extract_all(age, "\\d+", simplify = TRUE)[,2])-x,
           nax = n / 2) |>
    select(x, n, nax, nmx)
    
  #add open-ended row
  life_table <- life_table |>
    bind_rows(tibble(x = life_table$x[nrow(life_table)] + 
                       life_table$n[nrow(life_table)],
                     n = NA,
                     nax = 1/life_table$nmx[nrow(life_table)],
                     nmx = life_table$nmx[nrow(life_table)]))
  
  # now do the rest
  life_table <- life_table |>
    mutate(nqx = c(1 - exp(-nmx[-nrow(life_table)] * n[-nrow(life_table)]), 1),
           lx_end = cumprod(1-nqx),
           lx_start = c(1, lx_end[-nrow(life_table)]),
           ndx = lx_start-lx_end,
           nLx = ifelse(is.na(n), ndx * nax, lx_end * n + ndx * nax))
  
  return(sum(life_table$nLx))
}

calculate_lor <- function(marriages) {
  marriages |>
    group_by(decade_mar) |>
    summarize(n11 = sum(hgroup == 1 & wgroup ==1),
              n22 = sum(hgroup == 2 & wgroup ==2),
              n33 = sum(hgroup == 3 & wgroup ==3),
              n12 = sum(hgroup == 1 & wgroup ==2),
              n21 = sum(hgroup == 2 & wgroup ==1),
              n13 = sum(hgroup == 1 & wgroup ==3),
              n31 = sum(hgroup == 3 & wgroup ==1),
              n32 = sum(hgroup == 3 & wgroup ==2),
              n23 = sum(hgroup == 2 & wgroup ==3),
              lor12 = log((n12 * n21) / (n11 * n22)),
              lor13 = log((n13 * n31) / (n11 * n33)),
              lor23 = log((n32 * n23) / (n33 * n22))) |>
    select(decade_mar, starts_with("lor")) |>
    pivot_longer(cols = starts_with("lor"), names_to = "intermar", 
                 values_to = "lor", names_prefix = "lor") |>
    mutate(lor = ifelse(is.na(lor) | lor == -Inf | lor == Inf, NA, lor))
}

# Data cleaning functions -------------------------------------------------

code_population <- function(pop, ancestry) {
  
  last_year <- (max(pop$dob) - 1200) / 12
  
  pop |>
    left_join(ancestry) |>
    mutate(sex = factor(fem, levels = 0:1, labels = c("Male", "Female")),
           group = factor(group),
           mom = ifelse(mom == 0, NA, mom),
           dad = ifelse(pop == 0, NA, pop),
           dob = (dob - 1200) / 12,
           decade_birth = floor(dob / 10) * 10 + 5,
           decade_birth = ifelse(decade_birth < 0 | decade_birth > last_year,
                                 NA, decade_birth),
           dod = ifelse(dod == 0, NA, (dod - 1200) / 12),
           founder = is.na(mom) & is.na(dad),
           mixedness = 1 - (ancestry_group1^2+ancestry_group2^2)) |>
    select(pid, sex, group, founder, dob, decade_birth, dod, mom, dad, 
           ancestry_group1, ancestry_group2, nearest_gen_locus, mixedness)
}

get_marriages <- function(pop, mar) {
  
  husband <- pop |>
    filter(sex == "Male") |>
    select(pid, group, dob) |>
    rename(hpid = pid, hgroup = group, hdob = dob)
  
  wife <- pop |>
    filter(sex == "Female") |>
    select(pid, group, dob) |>
    rename(wpid = pid, wgroup = group, wdob = dob)
  
  marriages <- mar |>
    select(mid, wpid, hpid, dstart) |>
    left_join(husband) |>
    left_join(wife) |>
    mutate(year = (dstart - 1200) / 12,
           hage = year - hdob,
           wage = year - wdob) |>
    select(mid, year, hgroup, wgroup, hage, wage)
  
  return(marriages)
}

# Genealogical functions --------------------------------------------------

# create a dataset taht gives us pairing information of parents for each 
# person in the population.
get_parent_info <- function(pop) {

  # create a dataset that gives us mom and dad information for each current
  # person, based on mom and dad
  moms <- pop |>
    filter(sex == "Female") |>
    select(pid, group) |>
    rename(mom = pid, mom_group = group)

  dads <- pop |>
    filter(sex == "Male") |>
    select(pid, group) |>
    rename(dad = pid, dad_group = group)
                                
  pop |>
    left_join(moms) |>
    left_join(dads) |>
    select(pid, sex, group, dob, dod, mom, dad, mom_group, dad_group) |>
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
     select(pid, sex, group, gen, starts_with("ancestry_"))
 
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


## another approach to this is to get all the ancestors of everybody at once
## this is faster but requires a lot of RAM because it generates a huge dataset
get_ancestors <- function(pop) {
  parent_info <- get_parent_info(pop)
  current_ancestors <- parent_info |> filter(!is.na(mom))
  gen <- 1
  current_ancestors$gen <- gen
  current_ancestors$pid_orig <- current_ancestors$pid
  ancestors <- current_ancestors
  while(nrow(current_ancestors) > 0) {
    gen <- gen + 1
    next_ancestors <- parent_info[c(current_ancestors$mom, current_ancestors$dad),]
    next_ancestors$gen <- gen
    next_ancestors$pid_orig <- rep(current_ancestors$pid_orig, 2)
    ancestors <- ancestors |> bind_rows(next_ancestors)
    current_ancestors <- next_ancestors |>
      filter(!is.na(mom))
  }
  
  return(ancestors)
}

add_ancestry_info <- function(pop) {

  ancestors <- get_ancestors(pop)
  
  # get OG ancestors ancestry breakdown
  ancestry <- ancestors |> 
    filter(is.na(mom)) |>
    mutate(ancestry_fraction = 1/(2^(gen-1)),
           ancestry_group1 = ifelse(group == 1, ancestry_fraction, 0),
           ancestry_group2 = ifelse(group == 2, ancestry_fraction, 0)) |>
    group_by(pid_orig) |>
    summarize(ancestry_fraction = sum(ancestry_fraction),
              ancestry_group1 = sum(ancestry_group1),
              ancestry_group2 = sum(ancestry_group2)) |>
    ungroup() |>
    mutate(mixedness = 1 - (ancestry_group1^2 + ancestry_group2^2)) |>
    rename(pid = pid_orig)
  
  # get nearest generational locus
  gen_locus <- ancestors |>
    filter(intermar) |>
    group_by(pid_orig) |>
    summarize(n = n(),
              nearest_gen_locus = ifelse(n == 0, NA, min(gen))) |>
    ungroup() |>
    rename(pid = pid_orig) |>
    select(pid, nearest_gen_locus)
  
  pop <- pop |>
    left_join(ancestry) |>
    left_join(gen_locus)
  
  return(pop)
}
