library(tidyverse)

# this script access PHOIBLE Online, subsets to only one Inventory per language and write the file
phoible = read_rds("data/raw/phoible_database.rds")

# language tables
langs = phoible$tables$LanguageTable
vals = phoible$tables$ValueTable
param = phoible$tables$ParameterTable

langs %>% distinct(ID)

# join tables
inventories = langs %>%
  right_join(vals %>% select(!ID), join_by("ID" == "Language_ID")) %>%
  rename("lang_id" = ID) %>%
  left_join(param %>% select(!Name), join_by("Parameter_ID" == "ID"))

# group by Language_ID# group by inventory and count size, remove duplicates with smallest size and extract inventoryID to keep
contribs_to_use = inventories %>%
  group_by(Glottocode, Contribution_ID, .groups = "drop") %>%
  summarize(size = n()) %>%
  slice_min(size) %>% pull(Contribution_ID)

# filter to only keep the inventory with smallest sizes
inventories = inventories %>%
  filter(Contribution_ID %in% contribs_to_use)


inventories = inventories %>%
  rename("language" = Name, "macroarea" = Macroarea, "glottocode" = Glottocode, "ISO6393" = ISO639P3code, "phoneme" = Value)

# languages without glottocodes?
inventories %>%
  filter(is.na(glottocode)) %>%
  distinct(lang_id) # modernaramaicnortheastern, #djindewal

# filter
inventories = inventories %>%
  filter(!is.na(glottocode))


write_csv(inventories, "data/processed/phoible_data.csv")
