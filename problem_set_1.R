setwd("C:/Users/joses/OneDrive/Documents/GitHub/problem-set-1-JSerr2")

df_industry <- read.csv("SAEMP25N by industry.csv")
df_total <- read.csv("SAEMP25N total.csv")

library(tidyverse)

## Question 1 ##
clean_industry <- df_industry %>% 
  filter(Description != "By industry")
  

## Fixing industry data ##
industry_long <- clean_industry %>% 
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    names_prefix = "X",
    values_to = "employment"
  )

panel_data_industry <- industry_long %>% 
  pivot_wider(
    id_cols = c(GeoName, year),
    names_from = Description,
    values_from = employment
  )

panel_data_industry <- panel_data_industry %>% 
  rename(state = GeoName) %>% 
  mutate(year = as.integer(year))


## fixing total employment data##
df_total_long <- df_total %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               names_prefix = "X",
               values_to = "total_employment") %>% 
  mutate(year = as.numeric(year))

df_total_long <- df_total_long %>% 
  rename(state = GeoName) %>% 
  mutate(year = as.integer(year))

df_total_long <- df_total_long %>% 
  select(-c("GeoFips"))

 
## merging data and getting shares of employment ##
merged_df <- panel_data_industry %>% 
  inner_join(df_total_long, by = c("state" = "state", "year" = "year"))

industry_columns <- colnames(merged_df)[3:(ncol(merged_df) - 1)]

for (col in industry_columns) {
  non_numeric <- merged_df %>% filter(!grepl("^[0-9.]+$", .data[[col]]))
  if (nrow(non_numeric) > 0) {
    print(paste("Non-numeric values found in column:", col))
    print(non_numeric)
  }
}

merged_df[industry_columns] <- merged_df[industry_columns] %>%
  mutate(across(everything(), ~ as.numeric(gsub("[^0-9.]", "", .))))

merged_df$total_employment <- as.numeric(merged_df$total_employment)

merged_df[industry_columns] <- merged_df[industry_columns] / merged_df$total_employment

final_data <- merged_df %>% 
  select(-total_employment)

## Exporting data ##

write.csv(final_data, "data.csv", row.names = FALSE)

## Question 2 ##

# Function
colnames(final_data) <- trimws(colnames(final_data))

find_top_states <- function(num_states, industry, year){
  industry <- trimws(industry)
  filtered_data <- final_data[final_data$year == year, ]
  if (!(industry %in% colnames(filtered_data))) {
    stop("The specified industry is not found in the data frame.")
  }
  relevant_data <- filtered_data[, c("state", industry)]
  sorted_data <- relevant_data[order(relevant_data[[industry]], decreasing = TRUE ), ]
  top_states <- head(sorted_data$state, num_states)
  
  return(top_states)
}

## 2a ##

top_5_manufacturing <- find_top_states(5, "Manufacturing", 2000)

## 2b ##

manufacturing_data <- final_data %>% 
  filter(state %in% top_5_manufacturing, year %in% c(2000, 2017)) %>% 
  select(state, year, Manufacturing)

## 2c ##

ggplot(manufacturing_data, 
       aes(x = year, y = Manufacturing, color = state)) +
  geom_line() +
  geom_point() +
  labs(title = "Manufacturing Employment Share (2000 vs 2017)",
       x = "Year", y = "Manufacturing Share") +
  theme_minimal()

## 2d ##

industries <- c("Manufacturing", "Farm employment", "Information")
num_states <- c(5, 10, 15)
years <- c(2000, 2000, 2017)

for (i in seq_along(industries)) {
  
  top_states <- find_top_states(num_states[i], industries[i], years[i])
  
  industry_data <- final_data %>%
    filter(state %in% top_states, year %in% c(2000, 2017)) %>%
    select(state, year, all_of(industries[i]))
  
  plot <- ggplot(industry_data, aes(x = year, y = .data[[industries[i]]], color = state)) +
    geom_line() +
    geom_point() +
    labs(title = paste(industries[i], "Employment Share (2000 vs 2017)"),
         x = "Year", y = paste(industries[i], "Share")) +
    theme_minimal()
  
  print(plot)
}

## Question 3 ##

plot_state_in_distribution <- function(state, industry) {
  industry <- trimws(industry)
  
  data_2000 <- final_data %>%
    filter(year == 2000) %>%
    select(state, !!rlang::sym(industry))
  
  data_2017 <- final_data %>%
    filter(year == 2017) %>%
    select(state, !!rlang::sym(industry))
  
  data_diff <- data_2017 %>%
    inner_join(data_2000, by = "state", suffix = c("_2017", "_2000")) %>%
    mutate(change = .data[[paste0(industry, "_2017")]] - .data[[paste0(industry, "_2000")]])
  
  state_value_2000 <- data_2000 %>% filter(state == !!state) %>% pull(!!rlang::sym(industry))
  state_value_2017 <- data_2017 %>% filter(state == !!state) %>% pull(!!rlang::sym(industry))
  state_value_diff <- data_diff %>% filter(state == !!state) %>% pull(change)
  
  plot_histogram <- function(data, x, state_value, title) {
    mean_value <- mean(data[[x]], na.rm = TRUE)
    
    ggplot(data, aes_string(x = x)) +
      geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black", alpha = 0.7) +
      geom_vline(aes(xintercept = mean_value), color = "red", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = state_value), color = "blue", linetype = "solid", size = 1) +
      annotate("text", x = mean_value, y = Inf, label = "Mean", color = "red", vjust = 2) +
      annotate("text", x = state_value, y = Inf, label = state, color = "blue", vjust = 4) +
      labs(title = title, x = "Employment Share", y = "Frequency") +
      theme_minimal()
  }
  
  plot_2000 <- plot_histogram(data_2000, industry, state_value_2000, 
                              paste(state, "-", industry, "in 2000"))
  plot_2017 <- plot_histogram(data_2017, industry, state_value_2017, 
                              paste(state, "-", industry, "in 2017"))
  plot_diff <- plot_histogram(data_diff, "change", state_value_diff, 
                              paste(state, "-", industry, "Change from 2000 to 2017"))
  print(plot_2000)
  print(plot_2017)
  print(plot_diff)
}

plot_state_in_distribution("California", "Information")



