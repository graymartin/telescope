# tar_load(country_list)
# 
# tar_load(rger_emissions_rcalc)
# tar_load(composite_categories)

r_table <- as.data.frame(table(select(rger_emissions_rcalc, country, category, year, gas)))
c_table <- as.data.frame(table(select(composite_categories, country, category, year, gas)))

r_table_summary <-
  r_table %>% 
  group_by(category, country) %>% 
  summarize(max_freq = max(Freq, na.rm = TRUE),
            min_freq = min(Freq, na.rm = TRUE))

composite_categories_complete <- 
  composite_categories %>% 
  drop_na(category, country, year, gas, unit) %>% 
  as.data.frame() %>% 
  complete(country, nesting(category, gas, unit), year, fill = list("value" = 0))

cc_table <- as.data.frame(table(select(composite_categories_complete, country, category, year, gas)))


cc_table_summary <-
  cc_table %>% 
  group_by(category, country) %>% 
  summarize(max_freq = max(Freq, na.rm = TRUE),
            min_freq = min(Freq, na.rm = TRUE))
