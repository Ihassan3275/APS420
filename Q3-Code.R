#Q1


library(tidyverse)

data <- read.csv("WB_WDI_SP_DYN_LE00_IN.csv")

# ignoring unnecessary columns here
data_clean <- data %>%
  select(-c(Indicator.Name, Indicator.Code)) %>%
  pivot_longer(
    cols = matches("^X?[0-9]{4}$"),
    names_to = "Year",
    values_to = "LifeExp"
  )

data_clean$Year <- gsub("X", "", data_clean$Year)
data_clean$Year <- as.numeric(data_clean$Year)

data_clean <- data_clean %>% filter(!is.na(LifeExp))

data_clean <- data_clean %>%
  filter(Country.Name != "Central African Republic")


#  yearly changes
data_diff <- data_clean %>%
  arrange(Country.Name, Year) %>%
  group_by(Country.Name) %>%
  mutate(YearlyChange = LifeExp - lag(LifeExp)) %>%
  ungroup()



top_improvements <- data_diff %>%
  filter(!is.na(YearlyChange)) %>%
  arrange(desc(YearlyChange)) %>%
  head(5)

print("Top 5 Largest 1-Year Improvements:")
print(top_improvements)



top_declines <- data_diff %>%
  filter(!is.na(YearlyChange)) %>%
  arrange(YearlyChange) %>%
  head(5)

print("Top 5 Largest 1-Year Declines:")
print(top_declines)


#     world and Sub-saharan africa rows
ssa_world <- data_clean %>%
  filter(Country.Name %in% c("Sub-Saharan Africa", "World"))

ggplot(ssa_world, aes(x = Year, y = LifeExp, color = Country.Name)) +
  geom_line(size = 1) +
  labs(title = "Life Expectancy: Sub-Saharan Africa vs World",
       y = "Life Expectancy (years)",
       x = "Year") +
  theme_minimal()

ssa <- ssa_world %>% filter(Country.Name == "Sub-Saharan Africa")
world <- ssa_world %>% filter(Country.Name == "World")

diff_data <- merge(ssa, world, by = "Year")
diff_data$Difference <- diff_data$LifeExp.y - diff_data$LifeExp.x

ggplot(diff_data, aes(x = Year, y = Difference)) +
  geom_bar(stat = "identity") +
  labs(title = "Gap in Life Expectancy (World - Sub-Saharan Africa)",
       y = "Difference (years)",
       x = "Year") +
  theme_minimal()
