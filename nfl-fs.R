library(arrow)
library(tidyverse)
library(tictoc)
library(nflplotR)

ds <- open_dataset("data", partitioning = "year")

tic()
epa_type <- ds |>
  filter(season == 2021) |>
  filter(season_type == "REG") |>
  select(posteam, epa, play_type) |>
  filter(!is.na(epa), play_type %in% c("run", "pass")) |>
  group_by(posteam, play_type) |>
  summarise(
    n=n(),
    mean_epa = mean(epa),
    median = median(epa)
  ) |>
  collect()

toc()

# data
# ├── 1999
# │   └── data.parquet
# ├── 2000
# │   └── data.parquet
# ├── 2001
# │   └── data.parquet
# ├── 2002
# │   └── data.parquet
# ├── 2003
# │   └── data.parquet
# ...........
# └── 2021
#     └── data.parquet
ds |>
  filter(season == 2021) |>
  filter(season_type == "REG") |>
  select(posteam, epa, play_type) |>
  filter(!is.na(epa), play_type %in% c("run", "pass")) |>
  group_by(posteam, play_type) |>
  to_duckdb() |>
  summarise(
    n=n(),
    mean_epa = mean(epa),
    median = median(epa)
  ) |>

tic()
epa_type |>
  ggplot(aes(x = mean_epa, y = fct_reorder(posteam, mean_epa), color = play_type)) +
  geom_line(aes(group =posteam), color = "grey", size = 2) +
  geom_point(size = 3) +
  theme(axis.text.y = element_nfl_logo())
toc()


ds |> names() |> length() * ds |> count() |> collect() |> pull()

all_df <- ds |> collect()

all_df

all_df |> lobstr::obj_size()

size_df <- lobstr::obj_size(all_df) |> unclass()

size_df |>
  prettyunits::pretty_bytes()

fs::dir_info("data-csv") |>
  summarise(size = sum(size))

fs::dir_info("data", recurse = TRUE) |>
  summarise(size = sum(size))

388/2190
