# r read initial datasets
temp = read.csv("data/temperature_change.csv", fileEncoding="latin1") |> clean_names()
ltemp = read.csv("data/land_temperatures.csv") |> clean_names()
carbon = read.csv("data/carbon_dioxide_emissions.csv") |> clean_names()
methane = read.csv("data/methane_emissions.csv") |> clean_names()
population = read.csv("data/population.csv") |> clean_names()

# r clean temp
temp2 = temp |>
  select(area, months, element, y1990:y2018) |>
  pivot_longer(cols = y1990:y2018,
               names_prefix = "y",
               names_to = "year",
               values_to = "value") |>
  transform(year = as.numeric(year)) |>
  filter(element == "Temperature change") |>
  rename("country"="area") |>
  summarize("temperature_change" = mean(value), .by = c(country, year))


# r clean ltemp
ltemp2 = ltemp |>
  drop_na() |>
  mutate(dt = substr(dt, 1, 4)) |>
  filter(dt >= 1990 & dt <= 2018) |>
  transform(year = as.numeric(dt)) |>
  summarize(average_temperature = mean(average_temperature),
            average_temperature_uncertainty = mean (average_temperature_uncertainty),
            .by = year)



# r clean carbon
carbon2 = carbon |>
  select(country, x2018:x1990) |>
  mutate(x1990 = na_if(x1990, "N/A")) |>
  drop_na() |>
  transform(x1990 = as.numeric(x1990)) |>
  pivot_longer(cols = x1990:x2018,
               names_prefix = "x",
               names_to = "year",
               values_to = "carbon_dioxide_emissions") |>
  transform(year = as.numeric(year)) |>
  arrange(country)


# r clean methane
methane2 = methane |>
  select(country, x2018:x1990) |>
  mutate(x1990 = na_if(x1990, "N/A")) |>
  drop_na() |>
  transform(x1990 = as.numeric(x1990)) |>
  pivot_longer(cols = x1990:x2018,
               names_prefix = "x",
               names_to = "year",
               values_to = "methane_emissions") |>
  transform(year = as.numeric(year)) |>
  arrange(country)


# r clean population
population2 = population |>
  mutate(country = country_territory,
         population = x2020_population) |>
  transform(population = as.numeric(population)) |>
  select(country, population)


# r make joined
joined = full_join(temp2, carbon2) |>
  full_join(ltemp2) |>
  full_join(population2) |>
  filter(country != "World")

write_csv(joined, "data/joined.csv")
