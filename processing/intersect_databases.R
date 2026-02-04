library(tidyverse)

# this script reads all processed databases and intersects them

phoible = read_csv("data/processed/phoible_data.csv")
asjp = read_csv("data/processed/asjp_wide.csv")
grambank = read_csv("data/processed/grambank_data.csv")
glotto = read_csv("data/processed/glottolog.csv")

colSums(is.na(phoible))
phoible = phoible %>%
  distinct(glottocode) # 2184

asjp = asjp %>%
  distinct(glottocode) # 6127

grambank = grambank %>%
  distinct(glottocode) # 2467


intersection = asjp %>%
  inner_join(phoible, join_by(glottocode)) %>%
  inner_join(grambank, join_by(glottocode)) %>% 
  inner_join(glotto, join_by(glottocode))


nrow(intersection) # 1066 languages
colSums(is.na(intersection))

write_csv(intersection, "Data/processed/intersected_languages.csv")


