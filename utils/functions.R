# functions.R

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
