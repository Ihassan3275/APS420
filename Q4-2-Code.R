library(tidyverse)

# GDP file
gdp_data <- read.csv("WB_WDI_NY_GDP_PCAP_CD.csv")

gdp_clean <- gdp_data %>%
  select(REF_AREA_LABEL, TIME_PERIOD, OBS_VALUE) %>%
  rename(
    Country = REF_AREA_LABEL,
    Year = TIME_PERIOD,
    GDPpc = OBS_VALUE
  ) %>%
  mutate(
    Year = as.numeric(Year),
    GDPpc = as.numeric(GDPpc)
  ) %>%
  filter(!is.na(GDPpc),
         Country %in% c("Bangladesh",
                        "Korea, Rep.",
                        "Malaysia"))

ggplot(gdp_clean, aes(x = Year, y = GDPpc, color = Country)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "GDP per Capita (Current US$, 1960–Present)",
       y = "GDP per Capita (US$)")

