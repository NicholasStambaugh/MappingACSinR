library(tidycensus)
library(ggiraph)
library(tidyverse)
library(patchwork)
library(scales)
library(dplyr)
options(tigris_use_cache = TRUE)

vt_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "MI",
  county = c("Kent", "Barry", "Ionia", "Montcalm", "Mecosta"),
  year = 2020,
  geometry = TRUE
) %>%
  filter(estimate > 50000)

vt_map <- ggplot(vt_income, aes(fill = estimate)) + 
  geom_sf_interactive(aes(data_id = GEOID)) + 
  scale_fill_distiller(palette = "Greens",
                       direction = 1, 
                       guide = "none") + 
  theme_void()

vt_plot <- ggplot(vt_income, aes(x = estimate, y = reorder(NAME, estimate), 
                                 fill = estimate)) +
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point_interactive(color = "black", size = 4, shape = 21,
                         aes(data_id = GEOID)) +
  scale_fill_distiller(palette = "Greens", direction = 1,
                       labels = label_dollar()) + 
  scale_x_continuous(labels = label_dollar()) + 
  labs(title = "Household income by tract in Michigan",
       subtitle = "2016-2020 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)",
       fill = "ACS estimate") + 
  theme_minimal(base_size = 17) +
  theme(axis.text.y = element_text(size = 6, margin(0,0,10,0)))

girafe(ggobj = vt_map + vt_plot, width_svg = 16, height_svg = 8) %>%
  girafe_options(opts_hover(css = "fill:cyan;"))