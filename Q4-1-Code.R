library(tidyverse)

# life expectancy file
life_data <- read.csv("WB_WDI_SP_DYN_LE00_IN.csv")

life_clean <- life_data %>%
  select(REF_AREA_LABEL, TIME_PERIOD, OBS_VALUE) %>%
  rename(
    Country = REF_AREA_LABEL,
    Year = TIME_PERIOD,
    LifeExp = OBS_VALUE
  ) %>%
  mutate(
    Year = as.numeric(Year),
    LifeExp = as.numeric(LifeExp)
  ) %>%
  filter(!is.na(LifeExp),
         Country %in% c("Bangladesh",
                        "Korea, Rep.",
                        "Malaysia"))

ggplot(life_clean, aes(x = Year, y = LifeExp, color = Country)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Life Expectancy (1960–Present)",
       y = "Life Expectancy (years)")
