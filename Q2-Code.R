library(tidyverse)
library(reshape2)
data <- read.csv("WB_WDI_IT_NET_BBND_P2.csv")

####

lldc_africa <- c(
  "Burkina Faso","Burundi","Central African Republic","Chad",
  "Ethiopia","Lesotho","Malawi","Mali","Niger",
  "Rwanda","South Sudan","Uganda","Zambia","Zimbabwe",
  "Botswana","Eswatini"
)


#  filter African countries here
africa_data <- data %>%
  filter(Region == "Sub-Saharan Africa")

africa_data$LLDC_Status <- ifelse(
  africa_data$Country.Name %in% lldc_africa,
  "LLDC",
  "Non-LLDC"
)


africa_long <- melt(africa_data,
                    id.vars = c("Country.Name", "LLDC_Status"),
                    variable.name = "Year",
                    value.name = "Broadband")

africa_long <- africa_long %>%
  filter(grepl("^[0-9]+$", Year))

africa_long$Year <- as.numeric(as.character(africa_long$Year))
africa_long$Broadband <- as.numeric(africa_long$Broadband)

latest_data <- africa_long %>%
  group_by(Country.Name) %>%
  filter(Year == max(Year[!is.na(Broadband)])) %>%
  ungroup()

# (t-test)
t_test_result <- t.test(Broadband ~ LLDC_Status, data = latest_data)
print(t_test_result)

# Boxplot gen
boxplot(Broadband ~ LLDC_Status,
        data = latest_data,
        main = "Fixed Broadband Subscriptions (Latest Year)",
        ylab = "Subscriptions per 100 people",
        xlab = "Country Type")


time_trend <- africa_long %>%
  group_by(Year, LLDC_Status) %>%
  summarise(mean_broadband = mean(Broadband, na.rm = TRUE))

ggplot(time_trend, aes(x = Year, y = mean_broadband, color = LLDC_Status)) +
  geom_line(size = 1) +
  labs(title = "Evolution of Fixed Broadband Subscriptions in Africa",
       y = "Mean Subscriptions per 100 people") +
  theme_minimal()
