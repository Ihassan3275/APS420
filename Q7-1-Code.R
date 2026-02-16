map_data <- data %>%
  filter(grepl("Borrowed", INDICATOR_LABEL, ignore.case = TRUE)) %>%
  filter(grepl("mobile money account", INDICATOR_LABEL, ignore.case = TRUE)) %>%
  filter(AGE_LABEL == "15 years old and over") %>%
  group_by(REF_AREA_LABEL) %>%
  filter(TIME_PERIOD == max(TIME_PERIOD, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(iso_a3 = toupper(countrycode(REF_AREA_LABEL, "country.name", "iso3c")))


africa_map <- ne_countries(continent = "Africa", returnclass = "sf") %>%
  mutate(iso_a3 = toupper(iso_a3))

ggplot(africa_map %>% left_join(map_data, by = "iso_a3")) +
  geom_sf(aes(fill = OBS_VALUE), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  theme_void() +
  labs(title = "Mobile Money Borrowing: Africa Heatmap")
