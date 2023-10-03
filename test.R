library(tidyverse)
library(readxl)

pols = read_csv("./data/fivethirtyeight_datasets/pols-month.csv")

snp = 
  df <- read_csv("./data/fivethirtyeight_datasets/snp.csv", col_types = cols(date = col_date(format = "%m/%d/%y"), close = col_double())) |> #modified for correct date)
  separate(date, into = c("year", "month", "day"), convert = TRUE) |>
  mutate(year = ifelse(year > 2023, year - 100,year))
str(snp)


Mr_TrashWheel = 
  read_excel("./data/202207 Trash Wheel Collection Data.xlsx",sheet = "Mr. Trash Wheel", col_names = TRUE) |>
  select(c(1:14)) |> #ignore the column without data
  janitor::clean_names() |> #clean the dataset
  drop_na(dumpster) |> #remove data without dumpster number
  rename (volume = volume_cubic_yards) |>
  rename (weight = weight_tons) |>
  mutate (homes_powered, homes_powered = weight*500/30) |>
  mutate(year = as.numeric(year)) |>
  mutate(TrashWheel = "MrTrashWheel")


Prof_TrashWheel = 
  read_excel("./data/202207 Trash Wheel Collection Data.xlsx",sheet = "Professor Trash Wheel", col_names = TRUE) |>
  janitor::clean_names() |> #clean the dataset
  drop_na(dumpster) |> #remove data without dumpster number
  rename (volume = volume_cubic_yards) |>
  rename (weight = weight_tons) |>
  mutate (homes_powered, homes_powered = weight*500/30) |>
  mutate(year = as.numeric(year)) |>
  mutate(TrashWheel = "ProfTrashWheel")

Gwynnda_TrashWheel = 
  read_excel("./data/202207 Trash Wheel Collection Data.xlsx",sheet = "Gwynnda Trash Wheel", col_names = TRUE) |>
  janitor::clean_names() |> #clean the dataset
  drop_na(dumpster) |> #remove data without dumpster number
  rename (volume = volume_cubic_yards) |>
  rename (weight = weight_tons) |>
  mutate (homes_powered, homes_powered = weight*500/30) |>
  mutate(year = as.numeric(year))|>
  mutate(TrashWheel = "GwynndaTrashWheel")

TrashWheel_new = bind_rows (Mr_TrashWheel,Prof_TrashWheel,Gwynnda_TrashWheel)

baseline = 
  read_csv("./data/data_mci/MCI_baseline.csv", skip = 1) |>
  janitor::clean_names() |> #clean the dataset
  mutate (sex = ifelse(sex == 1, "Male", "Female")) |>
  mutate (apoe4 = ifelse(apoe4 == 1, "APOE4 carrier", "APOE4 non-carrier")) |>
  mutate (age_at_onset = as.numeric(age_at_onset)) 

#remove participants who are MCI free
baseline_clean = 
  baseline |>
  filter(age_at_onset > current_age | is.na(age_at_onset))

amyloid = 
  read_csv("./data/data_mci/mci_amyloid.csv", skip = 1) |>
  janitor::clean_names() |> #clean the dataset 
  rename(id = study_id)

amyloid_clean = 
  amyloid |>
  drop_na() 

combined_full = 
  full_join(baseline,amyloid, by = "id") #total participants in this study

combine_both = 
  inner_join(baseline,amyloid,by = "id") #participants who show in both baseline and amyloid dataset

combined_baseline =
  left_join(baseline,amyloid,by = "id") #participants who show in baseline dataset

combined_amyloid =
  right_join(baseline,amyloid,by = "id") #participants who show in amyloid dataset

combined_clean = 
  inner_join(baseline_clean,amyloid_clean) #participants without missing value


filter(TrashWheel_new, TrashWheel == "ProfTrashWheel")

filter(TrashWheel_new, TrashWheel == "GwynndaTrashWheel", year == "2021", month == "July") |> 
pull(cigarette_butts) |> 
sum()

