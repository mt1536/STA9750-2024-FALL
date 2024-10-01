library(nycflights13)
library(dplyr)
glimpse(flights)
glimpse(planes)

# Which plane (tailnum) flew the most times leaving NYC? Who manufactured it?

q5_table = inner_join (flights, planes, join_by (tailnum == tailnum))

q5_table |>
filter(origin %in% c("JFK", "LGA", "EWR")) |>
  group_by(tailnum) |> 
  summarise(n_departures = n()) |>
  filter(!is.na(tailnum)) |>
  slice_max(n_departures) |> 
  pull(manufacturer)









