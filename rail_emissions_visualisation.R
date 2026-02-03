#===========================================================
# Loading required packages
#=======================================================

install.packages(c(
  "readODS",     
  "tidyverse",   
  "janitor",    
  "stringr",     
  "treemapify",  
  "patchwork",   
  "scales"       
))

library(readODS)
library(tidyverse)
library(janitor)
library(stringr)
library(treemapify)
library(patchwork)
library(scales)


#===========================================================
#Loading the data
#=======================================================
data_dir <- "C:/Users/Senthil/Downloads/viz/data"
list.files(data_dir)   # shows what files are in that folder

f6103 <- file.path(data_dir,
                   "table-6103-estimates-of-normalised-passenger-carbon-dioxide-equivalent-emissions-by-operator.ods")

f6105 <- file.path(data_dir,
                   "table-6105-estimates-of-passenger-energy-consumption-and-carbon-dioxide-equivalent-emissions.ods")

stopifnot(file.exists(f6103), file.exists(f6105))  # ensures files are found


#===========================================================
# Reading raw ODS file 
#===========================================================

t6103_raw <- read_ods(f6103, sheet = 2) %>%
  remove_empty(which = c("rows","cols"))

t6105_raw <- read_ods(f6105, sheet = 2) %>%
  remove_empty(which = c("rows","cols"))


#===========================================================
# Cleaning the data
#===========================================================


hr_6103 <- which(
  str_detect(
    tolower(apply(t6103_raw, 1, paste, collapse = " ")),
    "time period"
  )
)[1]
stopifnot(!is.na(hr_6103))  


t6103 <- t6103_raw %>%
  slice(hr_6103:n()) %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  remove_empty(which = c("rows","cols"))


#===========================================================
# Convert the table 6103 tidy/long format for plotting 
#===========================================================

t6103_plot <- t6103 %>%
  rename(time_label = 1) %>%  # first column contains time periods
  pivot_longer(
    cols = -time_label,
    names_to = "operator",
    values_to = "value"
  ) %>%
  mutate(
    # Convert emissions values into numbers
    value = suppressWarnings(as.numeric(value)),
    
    # Remove ORR elemnt  like [p], [r], [b]
    time_label = str_remove_all(time_label, "\\s*\\[[prb]\\]"),
    
    # Create time label like 2021/22
    start_year = str_extract(time_label, "\\d{4}"),
    end_year   = str_extract(time_label, "(?<=Mar )\\d{4}"),
    fy_label   = paste0(start_year, "/", str_sub(end_year, 3, 4)),
    fy_label   = factor(fy_label, levels = unique(fy_label)),
    
    # Make operator names readable
    operator = as.factor(operator),
    operator_clean = operator %>%
      as.character() %>%
      str_remove("_grams_per_passenger_kilometre$") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  ) %>%
  select(fy_label, time_label, operator, operator_clean, value) %>%
  filter(!is.na(value))   # remove missing values


#============================================================
# Heatmap - Operator–time variation in CO₂e
#============================================================

p_heatmap <- ggplot(
  t6103_plot,
  aes(
    x = fy_label,
    y = fct_reorder(operator_clean, value, .fun = mean),
    fill = value
  )
) +
  geom_tile() +
  scale_fill_viridis_c(name = expression(CO[2]*"e")) +
  labs(
    title = expression("Normalised passenger " * CO[2] * "e by operator over time"),
    x = "Financial year",
    y = "Operator"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  )

print(p_heatmap)


#============================================================
# Violin - Distribution of CO₂e across operators
#============================================================
p_violin <- ggplot(t6103_plot, aes(x = fy_label, y = value)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, outlier.size = 0.6) +
  labs(
    title = expression("Distribution of operator " * CO[2] * "e across time"),
    x = "Financial year",
    y = expression(CO[2] * "e")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_violin)

#============================================================
# clen table 6105 energy and emissions 
#============================================================

t6105 <- t6105_raw %>%
  slice(4:n()) %>%                 # header row begins at row 4
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  remove_empty(which = c("rows","cols"))

t6105_plot <- t6105 %>%
  rename(
    time_label = time_period,
    elec_kwh_m = traction_electricity_consumption_million_kilowatt_hours,
    diesel_l_m = traction_diesel_consumption_million_litres,
    co2e       = na    # ORR column containing CO2e total
  ) %>%
  mutate(
    time_label = str_remove_all(time_label, "\\s*\\[[prb]\\]"),
    
    # [x] means missing in ORR tables → convert to NA then numeric
    elec_kwh_m = suppressWarnings(as.numeric(na_if(elec_kwh_m, "[x]"))),
    diesel_l_m = suppressWarnings(as.numeric(na_if(diesel_l_m, "[x]"))),
    co2e       = suppressWarnings(as.numeric(na_if(co2e, "[x]")))
  ) %>%
  filter(!is.na(co2e) & (!is.na(elec_kwh_m) | !is.na(diesel_l_m))) %>%
  mutate(
    elec_kwh_m = coalesce(elec_kwh_m, 0),
    diesel_l_m = coalesce(diesel_l_m, 0),
    
    # Standardised index: fair combination of different units
    energy_index = as.numeric(scale(elec_kwh_m)) + as.numeric(scale(diesel_l_m))
  )

#============================================================
# Bubble - Energy use vs emissions
#============================================================

p_bubble <- ggplot(t6105_plot, aes(x = energy_index, y = co2e)) +
  geom_point(aes(size = co2e), alpha = 0.8) +
  geom_smooth(se = FALSE) +
  labs(
    title = expression("Energy use vs " * CO[2] * "e emissions in UK passenger rail"),
    subtitle = "Standardised electricity + diesel index compared with total emissions",
    x = "Energy use index (standardised electricity + diesel)",
    y = expression(CO[2]*"e emissions"),
    size = expression(CO[2]*"e")
  ) +
  theme_minimal()

print(p_bubble)


#============================================================
# Treemap - Relative operator contributions
#============================================================

t6103_treemap <- t6103_plot %>%
  group_by(operator_clean) %>%
  summarise(
    avg_co2e = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_co2e))

p_treemap <- ggplot(
  t6103_treemap,
  aes(area = avg_co2e, fill = avg_co2e)
) +
  geom_treemap() +
  scale_fill_viridis_c(name = expression("Avg " * CO[2] * "e")) +
  labs(
    title = expression("Average normalised passenger " * CO[2] * "e intensity by operator"),
    subtitle = expression("Area represents mean " * CO[2] * "e intensity across all observed years")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

print(p_treemap)
