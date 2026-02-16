# note, I loaded libraries previously so I did not need to rerun them - if you run this code, you will :)

ts_data <- data %>%
  filter(grepl("Borrowed", INDICATOR_LABEL, ignore.case = TRUE)) %>%
  filter(grepl("mobile money account", INDICATOR_LABEL, ignore.case = TRUE)) %>%
  filter(AGE_LABEL == "15 years old and over") %>%
  filter(REF_AREA_LABEL == "Kenya" | grepl("Sub-Saharan Africa", REF_AREA_LABEL)) %>%
  mutate(Region = ifelse(REF_AREA_LABEL == "Kenya", "Kenya", "Sub-Saharan Africa"))


ggplot(ts_data, aes(x = TIME_PERIOD, y = OBS_VALUE, color = Region, group = Region)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Kenya" = "firebrick", "Sub-Saharan Africa" = "dodgerblue4")) +
  theme_minimal() +
  labs(title = "Fintech Trajectory: Kenya vs. Regional Average",
       y = "% Borrowing via Mobile Money", x = "Year")
