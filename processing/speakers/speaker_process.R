library(tidyverse)

# this script read the ethnologue joshua data and summarizes the number of speakers per language

speakers = read_csv("data/raw/speaker_data.csv")

speakers %>%
  filter(is.na(glotto_code))

# remove langauges without glotto_code
speakers = speakers %>%
  filter(!is.na(glotto_code))
  
speakers = speakers %>%
  group_by(glotto_code) %>%
  summarize(language = first(language),
            glotto_code = unique(glotto_code), 
            speakers = sum(speakers))

speakers = speakers %>%
  rename("glottocode" = glotto_code)

colSums(is.na(speakers))


write_csv(speakers, "data/processed/speaker_data.csv")
