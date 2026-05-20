# get implied life expectancy and TFR for simulations
library(tidyverse)

# remember that all rates are monthly in socsim, so need to be multiplied by 12


# Life Tables -------------------------------------------------------------

calc_le <- function(life_table) {
  
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
           nLx = ifelse(is.na(n), ndx * nax, lx_end * n + ndx * nax),
           ex = rev(cumsum(rev(nLx))))
  
  return(life_table)
}


life_table_women <- read_delim("simulation/parameter_files/basic_rates", 
                               delim = " ", skip = 61, n_max = 111, 
                               col_names = c("x", "grp", "nmx", "fluff")) |>
  mutate(x = x -1, n = 1, nax = 0.5, nmx = nmx * 12) |>
  select(x, n, nax, nmx) |>
  calc_le()

life_table_men <- read_delim("simulation/parameter_files/basic_rates", 
                               delim = " ", skip = 174, n_max = 111, 
                               col_names = c("x", "grp", "nmx", "fluff")) |>
  mutate(x = x -1, n = 1, nax = 0.5, nmx = nmx * 12) |>
  select(x, n, nax, nmx) |>
  calc_le()

# TFR -------------------------------------------------------------

asfr <- read_delim("simulation/parameter_files/fertility_rates", 
          delim = " ", skip = 7, n_max = 46, 
          col_names = c("x", "grp", "asfr"))

fert_multiplier <- 1.15
tfr <- sum(asfr$asfr) * 12 * fert_multiplier
