library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)

innovation <- read_csv("WEF_GCIHH_GCI_C_12.csv")
institutions <- read_csv("WEF_GCIHH_GCI_A_01.csv")
policy_stability <- read_csv("WEF_GCI_EOSQ434.csv")
legal_efficiency <- read_csv("WEF_GCI_EOSQ040.csv")
nt_barriers <- read_csv("WEF_GCI_EOSQ096.csv")

# common data year is 2017 for all datasets so I'm gonna focus on it
clean_data <- function(df, col_name) {
  df %>%
    filter(TIME_PERIOD == 2017) %>%
    filter(UNIT_MEASURE %in% c("1_TO_7", "IX")) %>% 
    select(Country = REF_AREA_LABEL, Value = OBS_VALUE) %>%
    rename(!!col_name := Value)
}

df_innov <- clean_data(innovation, "Innovation")
df_inst <- clean_data(institutions, "Institutions")
df_pol <- clean_data(policy_stability, "Policy_Stability")
df_leg <- clean_data(legal_efficiency, "Legal_Framework")
df_ntb <- clean_data(nt_barriers, "Non_Tariff_Barriers")

# merge
merged_df <- df_innov %>%
  inner_join(df_inst, by = "Country") %>%
  inner_join(df_pol, by = "Country") %>%
  inner_join(df_leg, by = "Country") %>%
  inner_join(df_ntb, by = "Country")


cor_matrix <- cor(merged_df %>% select_if(is.numeric))
print(cor_matrix)

create_plot <- function(data, x_var, title, corr_val) {
  ggplot(data, aes_string(x = x_var, y = "Innovation")) +
    geom_point(aes(color = Innovation), size = 3, alpha = 0.7) + 
    geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) +
    labs(title = paste0(title, "\n(r = ", round(corr_val, 2), ")"), 
         x = paste(x_var, "(1-7 Scale)"), 
         y = "Innovation (1-7 Scale)") +
    scale_color_viridis_c() +
    theme_minimal()
}

# plots
p1 <- create_plot(merged_df, "Institutions", "Innovation vs Institutions", cor(merged_df$Innovation, merged_df$Institutions))
p2 <- create_plot(merged_df, "Policy_Stability", "Innovation vs Policy Stability", cor(merged_df$Innovation, merged_df$Policy_Stability))
p3 <- create_plot(merged_df, "Legal_Framework", "Innovation vs Legal Framework", cor(merged_df$Innovation, merged_df$Legal_Framework))
p4 <- create_plot(merged_df, "Non_Tariff_Barriers", "Innovation vs Non-Tariff Barriers", cor(merged_df$Innovation, merged_df$Non_Tariff_Barriers))

grid.arrange(p1, p2, p3, p4, ncol = 2)
