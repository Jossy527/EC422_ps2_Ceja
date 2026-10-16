library(tidyquant)
library(tidyverse)

unemp_raw <- tq_get(
  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
  get  = "economic.data",
  from = "1976-01-01"
)

head(unemp_raw)

dir.create("data", showWarnings = FALSE)

write_rds(unemp_raw, "data/unemp_raw.rds")

tail (unemp_raw)
view(unemp_raw)

unemp_raw = read_rds("data/unemp_raw.rds")

# 4.1. Wrangle your Data
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"    ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )

# 4.2. Wrangle your Data
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )
view(unemp_summary)

unemp_recent  <- unemp |>
  filter(date >= as.Date("2020-01-01"))
unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )
view(unemp_recent_summary)

# 4.3. Find the Extremes
unemp_top_increases <- unemp |> 
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)
view(unemp_top_increases)
unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)
view(unemp_top_decreases)

# 4.4. Find the Extremes Excluding 2020
unemp_no_covid <- unemp |>
  filter(year(date) != 2020) 
unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)
view(unemp_top_increases_no_covid)
unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)
view(unemp_top_decreases_no_covid)

# 5.1: Produce a Line Plot
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +        # or geom_col(), geom_point(), etc.
  labs(title = "Unemployment Rate by Region Over Time",
       x = "Date", y = "Unemployment Rate") +
  theme_minimal()

# 5.2: Produce Individual Line Plots
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(title = "Unemployment Rate by Region Over Time",
       x = "Date", y = "Unemployment Rate") +
  theme_minimal()

# 5.3: Produce Bar Chart
library(forcats)
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col() +
  labs(title = "Mean Unemployment Rate by Region",
       x = "Region", y = "Mean Unemployment Rate") +
  theme_minimal()

# 5.4: Your Choice
ggplot(unemp_no_covid, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Unemployment Rate by Region Over Time (Excluding 2020)",
       x = "Date", y = "Unemployment Rate") +
  theme_minimal()
