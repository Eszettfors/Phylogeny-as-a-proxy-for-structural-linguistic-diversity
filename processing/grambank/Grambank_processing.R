library(lingtypology)
library(tidyverse)


### this script reads the grambankdata and creates two dataframed with each language as a row and each attribute as a column
# one long with each language-parameter-value as a row
# one wide with each langauge as a row and paramters a columns


# read data
gb = readRDS("data/raw/grambank_database.rds")


# get grambank linguistic data
gb_tab = gb$tables
values_tab = gb_tab$ValueTable
param_tab = gb_tab$ParameterTable
lang_tab = gb_tab$LanguageTable


head(values_tab)
head(param_tab)
head(lang_tab)


# join tabs into a single dataframe
df = values_tab %>% 
  select(Language_ID, Parameter_ID, Value, Code_ID) %>%
  left_join(
    param_tab %>%
      select(ID, Name) %>%
      rename("Parameter_name" = Name),
    join_by("Parameter_ID" == "ID")
  ) %>%
  left_join(
    lang_tab %>%
      select(ID, Name, Glottocode, Macroarea, Family_name, Latitude, Longitude, ISO639P3code) %>%
      rename("Language_name" = Name),
    join_by("Language_ID" == "ID")
  ) 

# renames columns
df = df %>%
  rename("language" = Language_name, "glottocode" = Glottocode, "macroarea" = Macroarea, "family" = Family_name, "ISO6393" = ISO639P3code)


# remove NA value
df = df %>%
  filter(!is.na(Value))

# write
write_csv(df, "data/processed/grambank_data.csv")



