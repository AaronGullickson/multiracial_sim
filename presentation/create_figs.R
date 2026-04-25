# create figures for presentation


# Setup -------------------------------------------------------------------


library(here)
source(here("utils","check_packages.R"))
source(here("utils","functions.R"))
load(here("data", "data_constructed", "analytical_data.RData"))

# Create palettes ---------------------------------------------------------

palette_scenario <- c(
  "absorbing state"  = "#E7298A",   # distinct purple (standalone)
  "high peak, resilient" = "#2C7FB8",  # teal (pair 1)
  "high peak, decay" = "#66C2A5",  # lighter teal
  "low peak, resilient" = "#D95F02",  # orange (pair 2)
  "low peak, decay" = "#FDB462"  # lighter orange
)

palette_group <- c(
  "A"  = "#000080",
  "B"  = "#FF6347",
  "AB" = "#FFD700"
)

palette_intermar <- c(
  "B-AB"  = "#00A6A6",  # bright teal
  "A-AB"  = "#F18F01",  # strong orange
  "A-B" = "#5F0F40"   # deep plum
)


# Create theme ------------------------------------------------------------

theme_poster <- function(base_size = 24) {
  theme_bw(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "#F1F8F3", color = NA),
      panel.background = element_rect(fill = "#F1F8F3", color = NA),
      strip.background = element_rect(fill = "#E4F1E8"),
      legend.background = element_rect(fill = "#F1F8F3", color = NA),
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )
}


# Modify data -------------------------------------------------------------

sims_pop <- sims_pop |>
  mutate(
    group = factor(group, levels = 1:3, labels = c("A", "B", "AB")),
    pop_share = factor(pop_share, levels = c("uneven", "even"), 
                       labels = c("uneven (85/15)", "even (50/50)")),
    scenario = str_replace(scenario, "_", " peak, "),
    scenario = str_replace(scenario, "total peak, rigid", "absorbing state"),
    scenario = factor(
      scenario,
      levels = c("absorbing state", "high peak, resilient", "high peak, decay",
                 "low peak, resilient", "low peak, decay")))

total_pop_group <- map(300:500, function(x) {
  sims_pop |>
    get_pop_alive(x) |>
    group_by(group, pop_share, mar_growth, scenario) |>
    summarize(year = x, population = n(), .groups = "drop") |>
    group_by(pop_share, mar_growth, scenario) |>
    mutate(share = population / sum(population)) |>
    arrange(pop_share, mar_growth, scenario, group)
}) |>
  bind_rows()

total_pop_ancestry <- map(300:500, function(x) {
  sims_pop |>
    get_pop_alive(x) |>
    group_by(pop_share, mar_growth, scenario) |>
    summarize(year = x, 
              share_mixed = mean(ancestry_group1 < 1 & ancestry_group2 < 1), 
              .groups = "drop") |>
    arrange(pop_share, mar_growth, scenario)
}) |>
  bind_rows()


# AB growth figure - splash -----------------------------------------------

total_pop_group |>
  filter(group == "AB") |>
  mutate(year = year - 300) |>
  ggplot(aes(x = year, y = share, group = scenario, color = scenario))+
  geom_line(linewidth = 3, alpha = 0.9)+
  facet_grid(pop_share~mar_growth)+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = palette_scenario)+
  labs(y = "share identifying as mixed (AB)",
       x = "year of simulation",
       color = "classification\nscenario")+
  theme_poster(32)

ggsave(
  here("_products", "presentation","growth_splash.pdf"),
  height = 9.88,
  width = 18.53,
  units = "in",
  device = cairo_pdf
)


 # Marriage figure ---------------------------------------------------------

mar_summary <- sims_mar |>
  filter(year >= 300) |>
  mutate(year = year - 300) |>
  mutate(year5 = floor(year/5)*5+2.5) |>
  group_by(year5, pop_share, mar_growth, scenario) |>
  summarize(n11 = sum(hgroup == 1 & wgroup == 1),
            n22 = sum(hgroup == 2 & wgroup == 2),
            n33 = sum(hgroup == 3 & wgroup == 3),
            n12 = sum(hgroup == 1 & wgroup == 2),
            n21 = sum(hgroup == 2 & wgroup == 1),
            n13 = sum(hgroup == 1 & wgroup == 3),
            n31 = sum(hgroup == 3 & wgroup == 1),
            n23 = sum(hgroup == 2 & wgroup == 3),
            n32 = sum(hgroup == 3 & wgroup == 2),
            lor12 = log(n12) + log(n21) - log(n11) - log(n22),
            lor13 = log(n13) + log(n31) - log(n11) - log(n33),
            lor23 = log(n23) + log(n32) - log(n22) - log(n33),
            out_all = (n12+n21+n13+n31+n23+n32)/(n11+n22+n33+n12+n21+n13+n31+n23+n32),
            out1 = (n12+n21+n13+n31)/(2*n11+n12+n21+n13+n31),
            out2 = (n12+n21+n23+n32)/(2*n22+n12+n21+n23+n32),
            out3 = (n13+n23+n23+n32)/(2*n33+n13+n23+n23+n32)) |>
  mutate(lor12 = if_else(is.na(lor12) | lor12 == Inf | lor12 == -Inf, NA, lor12),
         lor13 = if_else(is.na(lor13) | lor13 == Inf | lor13 == -Inf, NA, lor13),
         lor23 = if_else(is.na(lor23) | lor23 == Inf | lor23 == -Inf, NA, lor23),
         mar_growth = factor(mar_growth, levels = c("plateau", "progress")),
         pop_share = factor(pop_share, levels = c("even", "uneven")),
         scenario = str_replace(scenario, "_", " peak, "),
         scenario = str_replace(scenario, "total peak, rigid", "absorbing state"),
         scenario = factor(
           scenario,
           levels = c("absorbing state", "high peak, resilient", "high peak, decay",
                      "low peak, resilient", "low peak, decay"))) |>
  select(year5, pop_share, mar_growth, scenario, starts_with("lor"), 
         starts_with("out"))

mar_summary |>
  pivot_longer(cols = starts_with("lor"), names_prefix = "lor", 
               names_to = "type", values_to = "lor") |>
  mutate(type = case_when(type == "12" ~ "A-B",
                          type == "13" ~ "A-AB",
                          type == "23" ~ "B-AB"),
         type = factor(type, levels = c("A-B", "A-AB", "B-AB")),
         pop_share = factor(pop_share, levels = c("uneven", "even"), 
                            labels = c("uneven (85/15)", "even (50/50)"))) |>
  ggplot(aes(year5, lor, group = type, color = type, shape = scenario))+
  geom_hline(yintercept = c(-10, 0), linetype = 2)+
  geom_point(alpha = 0.3)+
  geom_smooth(se = FALSE, method = "gam")+
  facet_grid(pop_share~mar_growth)+
  scale_color_manual(values = palette_intermar)+
  scale_shape_manual(values = c(19, 15, 17, 18, 3))+
  #scale_y_continuous(limits = c(-10, 0.1))+
  labs(y = "log odds ratio of intergroup union",
       x = "simulation year of marriage",
       color = "group\ncombination")+
  theme_poster(18)

ggsave(
  here("_products", "presentation","mar_scenarios.pdf"),
  height = 4.97,
  width = 9.31,
  units = "in",
  bg = "#F1F8F3",
  device = cairo_pdf
)


# Classification figure ---------------------------------------------------

calculate_probs <- function(scenario_name) {
  parameters <- range_read(
    "1ad-fJUCjRy_zslI2MMce8UaolepG536-IZr1yNNIuZ8",
    range = paste(scenario_name, "A8:L9", sep="!")
  ) |>
    select(starts_with("inherit"))
  prop_a <- seq(from = 0.01, to= 0.99, by = 0.01)
  y_a <- parameters$inherit_g1_intercept+
    parameters$inherit_g1_slope*prop_a+
    parameters$inherit_g1_slope_sq*(prop_a^2)+
    parameters$inherit_g1_slope_cube*(prop_a^3)
  y_b <- parameters$inherit_g2_intercept+
    parameters$inherit_g2_slope*prop_a+
    parameters$inherit_g2_slope_sq*(prop_a^2)+
    parameters$inherit_g2_slope_cube*(prop_a^3)
  y_ab <- 0 * prop_a
  denom <- (exp(y_a)+exp(y_b)+exp(y_ab))
  p_a <- exp(y_a)/denom
  p_b <- exp(y_b)/denom
  p_ab <- exp(y_ab)/denom
  
  tibble(group = factor(rep(c("A", "B", "AB"), each = length(prop_a)),
                        levels = c("A", "B", "AB")),
         prop_a = rep(prop_a, 3), 
         prob = c(p_a, p_b, p_ab))
}

gs4_deauth()

# high resilient
gp_hr <- calculate_probs("uneven_plateau_high_resilient") |>
  mutate(scenario_peak = "high peak", scenario_decline = "resilient")

# high decay
gp_hd <- calculate_probs("uneven_plateau_high_decay") |>
  mutate(scenario_peak = "high peak", scenario_decline = "decay")

# low resilient
gp_lr <- calculate_probs("uneven_plateau_low_resilient") |>
  mutate(scenario_peak = "low peak", scenario_decline = "resilient")

# low decay
gp_ld <- calculate_probs("uneven_plateau_low_decay") |>
  mutate(scenario_peak = "low peak", scenario_decline = "decay")

bind_rows(gp_hr, gp_hd, gp_lr, gp_ld) |>
  mutate(scenario_decline = factor(scenario_decline, 
                                   levels = c("resilient", "decay"))) |>
  ggplot(aes(x = prop_a, y = prob, group = group, color = group))+
  geom_line(linewidth = 2)+
  facet_grid(scenario_decline~scenario_peak)+
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
  scale_color_manual(values = palette_group)+
  labs(x = "proportion group A ancestry", 
       y = "probability of group assignment")+
  theme_poster(18)

ggsave(
  here("_products", "presentation","classification_scenarios.pdf"),
  height = 4.97,
  width = 9.31,
  units = "in",
  bg = "#F1F8F3",
  device = cairo_pdf
)

# Mixed ancestry figure ---------------------------------------------------

total_pop_ancestry |>
  mutate(year = year - 300) |>
  ggplot(aes(x = year, y = share_mixed, group = scenario, color = scenario))+
  geom_line(linewidth = 2, alpha = 0.6)+
  facet_grid(pop_share~mar_growth)+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = palette_scenario)+
  labs(y = "share of population with mixed ancestry",
       x = "year of simulation",
       color = "classification\nscenario")+
  theme_poster()

ggsave(
  here("_products", "presentation","ancestry_growth.pdf"),
  height = 7.37,
  width = 13.81,
  units = "in",
  bg = "#F1F8F3",
  device = cairo_pdf
)

# ancestry percent figure -------------------------------------------------

sims_pop |>
  # get everyone living at the end
  filter(is.na(dod)) |>
  # leave out absorbing state
  filter(scenario != "absorbing state") |>
  # only those who have mixed ancestry
  filter(ancestry_group1 < 1 & ancestry_group2 < 1) |>
  group_by(pop_share, mar_growth, scenario) |>
  summarize(p_identify_mixed = mean(group == "AB"), .groups = "drop") |>
  ggplot(aes(x = fct_rev(scenario), y = p_identify_mixed, group = mar_growth,
             color = mar_growth))+
  geom_segment(aes(yend = 0), position = position_dodge(width = 0.6))+
  geom_point(position = position_dodge(width = 0.6))+
  #geom_hline(yintercept = c(0.4, 0.75), linetype = 3)+
  facet_wrap(~pop_share)+
  coord_flip()+
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
  scale_color_manual(values = c("#1B4332", "#C9184A"))+
  labs(y = "Size of mixed (AB) population as fraction of all individuals with mixed ancestry",
       x = NULL,
       color = "partnering\ntrend")+
  theme_poster()

ggsave(
  here("_products", "presentation","identifying_share.pdf"),
  height = 7.37,
  width = 13.81,
  units = "in",
  bg = "#F1F8F3",
  device = cairo_pdf
)

# Pop shares over time figure ----------------------------------------------

total_pop_group |>
  filter(mar_growth == "progress") |>
  mutate(year = year - 300) |>
  ggplot(aes(x = year, y = share, fill = group))+
  facet_grid(scenario~pop_share)+
  geom_area(color = NA)+
  scale_fill_manual(values = palette_group)+
  scale_y_continuous(labels = scales::percent)+
  labs(y = NULL, x = "year of simulation")+
  theme_poster()+
  theme(panel.grid = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom")

ggsave(
  here("_products", "presentation","group_share.pdf"),
  height = 17.72,
  width = 14.02,
  units = "in",
  bg = "#F1F8F3",
  device = cairo_pdf
)
