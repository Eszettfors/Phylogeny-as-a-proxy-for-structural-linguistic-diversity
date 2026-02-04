library(tidyverse)
library(patchwork)
library(xtable)

#this scripts read all datasets and analyses the coverage of them wrt languages, speakers and macroarea

phoible = read_csv("data/processed/phoible_data.csv")
asjp = read_csv("data/processed/asjp_wide.csv")
grambank = read_csv("data/processed/grambank_data.csv")
glotto = read_csv("data/processed/glottolog.csv")
intersect = read_csv("data/processed/intersected_languages.csv")
speakers = read_csv("data/processed/speaker_data.csv")

theme_set(theme_minimal(base_size = 14))

split_langs_speakers_counts = function(df, langs_speakers = "langs"){
  # if langs are associated with multiple macroareas, they are distributed across the macroareas where they are present
  
  # extract the macroareas to be splited
  splits = df %>%
    filter(grepl(";", macroarea)) %>%
    mutate(macroarea = str_split(macroarea, ";")) 
  
  
  # filter away from core df
  df_core = df %>%
    filter(!grepl(";", macroarea))
  
  # data frame to hold the contributed languages
  if (langs_speakers == "langs"){
    contrib_df = data.frame(langs = rep(0, length(unique(unlist(splits["macroarea"])))))
  } else if (langs_speakers == "speakers"){
    contrib_df = data.frame(speakers = rep(0, length(unique(unlist(splits["macroarea"])))))
  }
  rownames(contrib_df) = unique(unlist(splits["macroarea"]))
  
  
  
  # loop through each split and sum the contributions across macroareas
  for(i in 1:nrow(splits)){
    # divide the number of langs across the macro areas they appear in -> contribution of languages that is to be given to each macroarea
    to_add = splits[i, langs_speakers] / length(unlist(splits[i, "macroarea"]))

    for (macro in unlist(splits[i, "macroarea"])){
      # loop through all macro areas present and add the contribution
      contrib_df[macro, langs_speakers] = contrib_df[macro, langs_speakers] + as.numeric(to_add)
    }
  }
  
  # merge the core df with the contribution df
  contrib_df$macroarea = rownames(contrib_df)
  contrib_df %>%
    relocate(macroarea)

  df = rbind(df_core, contrib_df)
  
  return(df)
  
}



# phoible ######

colSums(is.na(phoible))

# n langs
phoible_langs = phoible %>%
  group_by(glottocode) %>%
  summarize(language = first(language))
print(nrow(phoible_langs)) # 2184

phoible_langs_macro = phoible_langs %>%
  left_join(glotto %>% 
               select(glottocode, macroarea, Latitude, Longitude), join_by(glottocode))

phoible_langs_macro %>%
  filter(!is.na(macroarea)) %>%
  nrow() # 2136 can be matched with macroareas 

# summarize per macroarea
phoible_langs_macro_distrib = phoible_langs_macro %>%
  filter(!is.na(macroarea)) %>%
  group_by(macroarea) %>%
  summarize(langs = n_distinct(glottocode))


phoible_langs_macro_distrib

# split langs across shared macroarea
phoible_langs_macro_distrib = phoible_langs_macro_distrib %>%
  split_langs_speakers_counts(langs_speakers = "langs")

# group and sum
phoible_langs_macro_distrib = phoible_langs_macro_distrib %>%
  group_by(macroarea) %>%
  summarize(langs = sum(langs))

# add label
phoible_langs_macro_distrib$dataset = "Phoible"

# n speakers
phoible_speakers = phoible %>%
  distinct(glottocode) %>%
  inner_join(speakers, join_by(glottocode)) 

nrow(phoible_speakers) #1790 langs can be matched with speaker numbers

phoible_speakers %>%
  summarize(speakers_tot = sum(speakers)) # 6414705792

# macroarea
phoible_speakers_macro = phoible_speakers %>%
  inner_join(glotto %>% 
              select(glottocode, macroarea), join_by(glottocode))

phoible_speakers_macro = phoible_speakers_macro %>%
  filter(!is.na(macroarea))


phoible_speakers_macro_distrib = phoible_speakers_macro %>%
  group_by(macroarea) %>%
  summarize(speakers = sum(speakers))

sum(phoible_speakers_macro_distrib$speakers) # 6414593328 speakers can be assigned to a macroarea


phoible_speakers_macro_distrib = phoible_speakers_macro_distrib %>%
  split_langs_speakers_counts(langs_speakers = "speakers")

# group and join
phoible_speakers_macro_distrib = phoible_speakers_macro_distrib %>%
  group_by(macroarea) %>%
  summarize(speakers = sum(speakers))

sum(phoible_speakers_macro_distrib$speakers) # 6414593328

phoible_speakers_macro_distrib$dataset = "Phoible"

# ASJP #####

colSums(is.na(asjp))

# n langs
asjp_langs = asjp %>%
  group_by(glottocode) %>%
  summarize(language = first(language))
print(nrow(asjp_langs)) # 6127

asjp_langs_macro = asjp_langs %>%
  left_join(glotto %>% 
               select(glottocode, macroarea), join_by(glottocode))

asjp_langs_macro = asjp_langs_macro %>%
  filter(!is.na(macroarea))

asjp_langs_macro %>%
  nrow() # 6017 langs can be matched with macroareas 

asjp_langs_macro_distrib = asjp_langs_macro %>%
  group_by(macroarea) %>%
  summarize(langs = n_distinct(glottocode))


asjp_langs_macro_distrib = asjp_langs_macro_distrib %>%
  split_langs_speakers_counts(langs_speakers = "langs")


asjp_langs_macro_distrib = asjp_langs_macro_distrib %>%
  group_by(macroarea) %>%
  summarize(langs = sum(langs)) %>%
  mutate(dataset = "ASJP")

sum(asjp_langs_macro_distrib$langs) # 6017

# n speakers
asjp_speakers = asjp %>%
  distinct(glottocode) %>%
  inner_join(speakers, join_by(glottocode)) 

nrow(asjp_speakers) #5572 langs can be matched with speaker numbers

asjp_speakers %>%
  summarize(speakers_tot = sum(speakers)) # 7439279572

# macroarea
asjp_speakers_macro = asjp_speakers %>%
  inner_join(glotto %>% 
               select(glottocode, macroarea), join_by(glottocode))


asjp_speakers_macro = asjp_speakers_macro %>%
  filter(!is.na(macroarea))

asjp_speakers_macro %>%
  summarize(speakers_tot = sum(speakers)) # 7439092381

asjp_speakers_macro_distrib = asjp_speakers_macro %>%
  group_by(macroarea) %>%
  summarize(speakers = sum(speakers)) 

asjp_speakers_macro_distrib = asjp_speakers_macro_distrib %>%
  split_langs_speakers_counts(langs_speakers = "speakers")

asjp_speakers_macro_distrib = asjp_speakers_macro_distrib %>%
  group_by(macroarea) %>%
  summarize(speakers = sum(speakers))

sum(asjp_speakers_macro_distrib$speakers) # 7439092381

asjp_speakers_macro_distrib$dataset = "ASJP"


# Grambank ####

colSums(is.na(grambank))

# n langs
grambank_langs = grambank %>%
  group_by(glottocode) %>%
  summarize(language = first(language))

print(nrow(grambank_langs)) # 2467

grambank_langs_macro = grambank_langs %>%
  left_join(glotto %>% 
               select(glottocode, macroarea), join_by(glottocode))

grambank_langs_macro = grambank_langs_macro %>%
  filter(!is.na(macroarea))

grambank_langs_macro %>%
  nrow() # 2402 can be matched with macroareas 


grambank_langs_macro_distrib = grambank_langs_macro %>%
  group_by(macroarea) %>%
  summarize(langs = n_distinct(glottocode)) 

grambank_langs_macro_distrib = grambank_langs_macro_distrib %>%
  split_langs_speakers_counts(langs_speakers = "langs")

grambank_langs_macro_distrib = grambank_langs_macro_distrib %>%
  group_by(macroarea) %>%
  summarize(langs = sum(langs))

grambank_langs_macro_distrib = grambank_langs_macro_distrib %>%
  mutate(dataset = "Grambank")

# n speakers
grambank_speakers = grambank %>%
  distinct(glottocode) %>%
  inner_join(speakers, join_by(glottocode)) 

nrow(grambank_speakers) #2148 langs can be matched with speaker numbers

grambank_speakers %>%
  summarize(speakers_tot = sum(speakers)) # 5065812986

# macroarea
grambank_speakers_macro = grambank_speakers %>%
  inner_join(glotto %>% 
               select(glottocode, macroarea), join_by(glottocode))

grambank_speakers_macro = grambank_speakers_macro %>%
  filter(!is.na(macroarea))

sum(grambank_speakers_macro$speakers) # 5065650574

grambank_speakers_macro_distrib = grambank_speakers_macro %>%
  group_by(macroarea) %>%
  summarize(speakers = sum(speakers))

grambank_speakers_macro_distrib = grambank_speakers_macro_distrib %>%
  split_langs_speakers_counts(langs_speakers = "speakers")

grambank_speakers_macro_distrib = grambank_speakers_macro_distrib %>%
  group_by(macroarea) %>%
  summarize(speakers = sum(speakers))

grambank_speakers_macro_distrib$dataset = "Grambank"

### intersection #######

colSums(is.na(intersect))

# n langs
intersect_langs = intersect %>%
  group_by(glottocode) %>%
  summarize(language = first(language))

print(nrow(intersect_langs)) # 1054

intersect_langs_macro = intersect_langs %>%
  left_join(glotto %>% 
               select(glottocode, macroarea), join_by(glottocode))

intersect_langs_macro = intersect_langs_macro %>%
  filter(!is.na(macroarea))

intersect_langs_macro %>%
  nrow() # 1026 can be matched with macroareas 


intersect_langs_macro_distrib = intersect_langs_macro %>%
  group_by(macroarea) %>%
  summarize(langs = n_distinct(glottocode)) 

intersect_langs_macro_distrib = intersect_langs_macro_distrib %>%
  split_langs_speakers_counts(langs_speakers = "langs")

intersect_langs_macro_distrib = intersect_langs_macro_distrib %>%
  group_by(macroarea) %>%
  summarize(langs = sum(langs))

intersect_langs_macro_distrib = intersect_langs_macro_distrib %>%
  mutate(dataset = "Intersection")

# n speakers
intersect_speakers = intersect %>%
  distinct(glottocode) %>%
  inner_join(speakers, join_by(glottocode)) 

nrow(intersect_speakers) #972 langs can be matched with speaker numbers

intersect_speakers %>%
  summarize(speakers_tot = sum(speakers)) # 4713477206

# macroarea
intersect_speakers_macro = intersect_speakers %>%
  inner_join(glotto %>%
               select(glottocode, macroarea), join_by(glottocode))


intersect_speakers_macro = intersect_speakers_macro %>%
  filter(!is.na(macroarea))

sum(intersect_speakers_macro$speakers) # 4713366275

intersect_speakers_macro_distrib = intersect_speakers_macro %>%
  group_by(macroarea) %>%
  summarize(speakers = sum(speakers))

intersect_speakers_macro_distrib = intersect_speakers_macro_distrib %>%
  split_langs_speakers_counts(langs_speakers = "speakers")

intersect_speakers_macro_distrib = intersect_speakers_macro_distrib %>%
  group_by(macroarea) %>%
  summarize(speakers = sum(speakers))

intersect_speakers_macro_distrib$dataset = "Intersection"


#### check sum
asjp_langs_macro_distrib
asjp_speakers_macro_distrib

phoible_langs_macro_distrib
phoible_speakers_macro_distrib

grambank_langs_macro_distrib
grambank_speakers_macro_distrib

intersect_langs_macro_distrib
intersect_speakers_macro_distrib

### Speakers ######

speakers = speakers %>%
  inner_join(glotto %>% select(glottocode, macroarea), join_by(glottocode))

speakers %>%
  summarize(speakers_tot = sum(speakers)) #7600461992

speakers = speakers %>%
  filter(!is.na(macroarea))

sum(speakers$speakers) # 7600274501

speakers_macro_distrib = speakers %>%
  group_by(macroarea) %>%
  summarize(speakers = sum(speakers))

speakers_macro_distrib = speakers_macro_distrib %>%
  split_langs_speakers_counts(langs_speakers = "speakers")

speakers_macro_distrib = speakers_macro_distrib %>%
  group_by(macroarea) %>%
  summarize(speakers = sum(speakers))

speakers_macro_distrib$dataset = "Ethnologue_Joshua"

##### Glottolog #####

glotto = glotto %>%
  filter(level == "language") %>%
  filter(category != "Bookkeeping") # 8231 languages

glotto_macro_distrib = glotto %>%
  filter(!is.na(macroarea)) %>%
  group_by(macroarea) %>%
  summarize(langs = n_distinct(glottocode)) %>%
  split_langs_speakers_counts()

glotto_macro_distrib = glotto_macro_distrib %>%
  group_by(macroarea) %>%
  summarize(langs = sum(langs))

glotto_macro_distrib$dataset = "Glottolog"


######## visualization ######

# barplots showing the total numbers of languages and speakers across languages

n_langs_asjp = nrow(asjp_langs)
n_langs_phoible = nrow(phoible_langs)
n_langs_grambank = nrow(grambank_langs)
n_langs_intersect = nrow(intersect_langs)
n_langs_glotto = nrow(glotto)

full_lang = data.frame(dataset = c("Glottolog", "ASJP", "Phoible", "Grambank", "Intersection"), 
                       langs = c(n_langs_glotto, n_langs_asjp, n_langs_phoible, n_langs_grambank ,n_langs_intersect))


n_speakers_asjp = sum(asjp_speakers$speakers)
n_speakers_phoible = sum(phoible_speakers$speakers)
n_speakers_grambank = sum(grambank_speakers$speakers)
n_speakers_intersect = sum(intersect_speakers$speakers)
n_speakers_ethno_joshua = sum(speakers$speakers)

full_speaker = data.frame(dataset = c("Ethno./Joshua", "ASJP", "Phoible", "Grambank", "Intersection"), 
                       speakers = c(n_speakers_ethno_joshua, n_speakers_asjp, n_speakers_phoible, n_speakers_grambank, n_speakers_intersect))


full_lang_macro = rbind(glotto_macro_distrib, asjp_langs_macro_distrib, grambank_langs_macro_distrib, phoible_langs_macro_distrib, intersect_langs_macro_distrib)

full_speaker_macro = rbind(speakers_macro_distrib, asjp_speakers_macro_distrib, grambank_speakers_macro_distrib, phoible_speakers_macro_distrib, intersect_speakers_macro_distrib)


bar_lang_coverage = full_lang %>%
  mutate(coverage = langs/max(langs) * 100) %>%
  ggplot(aes(y = langs, x = reorder(dataset, -langs), fill = dataset)) +
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  geom_text(aes(label = paste0(langs, " \n(", round(coverage,2), "%)")),
            vjust = -0.25,
            size = 3.7) + 
  theme(legend.position = "none") + 
  labs(x = "",
       y = "Number of varieties",
       title = "a)") + 
  lims(y = c(0, max(full_lang$langs) + 1000))

print(bar_lang_coverage)


bar_speaker_coverage = full_speaker %>%
  group_by(dataset) %>%
  summarize(speakers = sum(speakers)) %>%
  mutate(coverage = speakers/max(speakers) * 100) %>%
  ggplot(aes(y = speakers, x = reorder(dataset, -speakers), fill = dataset)) +
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  geom_text(aes(label = paste0(speakers, " \n(", round(coverage,2), "%)")),
            vjust = -0.25,
            size = 3.7) + 
  theme(legend.position = "none") + 
  labs(x = "",
       y = "Number of speakers",
       title = "b)") + 
  lims(y = c(0, max(full_speaker$speakers) + 5000000000))

print(bar_speaker_coverage)

bar_lang_macro = full_lang_macro %>%
  group_by(dataset) %>%
  mutate(percent = langs / sum(langs) * 100) %>%
  mutate(tot_langs = sum(langs)) %>%
  ggplot(aes(y = percent, x = reorder(dataset, -tot_langs), fill = macroarea)) + 
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  geom_text(
    aes(label = paste0(round(percent,2), "%")),
    position = position_stack(vjust = 0.5), 
    size = 3.7,
    color = "black" ) + 
  labs(title = "c)",
       x = "",
       y = "Percent (%)")

print(bar_lang_macro)

bar_speaker_macro = full_speaker_macro %>%
  group_by(dataset) %>%
  mutate(percent = speakers / sum(speakers) * 100) %>%
  mutate(tot_speakers = sum(speakers)) %>%
  ggplot(aes(y = percent, x = reorder(dataset, -tot_speakers), fill = macroarea)) + 
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  geom_text(
    aes(label = paste0(round(percent,2), "%")),
    position = position_stack(vjust = 0.5), 
    size = 4,
    color = "black" ) + 
  labs(title = "Distribution of speakers across macroareas in major cross-linguistic databases",
       x = "",
       y = "Percent (%)")
print(bar_speaker_macro)

tri_plot = (bar_lang_coverage + bar_speaker_coverage ) / (bar_lang_macro)

ggsave("analysis/plots/barplots.png", tri_plot, width = 12, height = 10, dpi = 300)

###### language families  ####

# subset to only spoken L1_language
glotto_l1 = glotto %>%
  filter(category == "Spoken_L1_Language")
nrow(glotto_l1)# 7675

phoible_langs = phoible_langs %>%
  left_join(glotto_l1 %>% select(!language), join_by(glottocode))

asjp_langs = asjp_langs %>%
  left_join(glotto_l1 %>% select(!language), join_by(glottocode))

grambank_langs = grambank_langs %>%
  left_join(glotto_l1 %>% select(!language), join_by(glottocode))

intersect_langs = intersect_langs %>%
  left_join(glotto_l1 %>% select(!language), join_by(glottocode))

# language families

get_n_lang_fam = function(df){
  # extract the first head of the classification as the top langauge families and counts the number
  n_fam = df %>%
    filter(!is.na(classification)) %>%
    rowwise() %>%
    mutate(classification = str_split(classification, "/")[[1]][1]) %>%
    distinct(classification) %>% nrow()
  return(n_fam)
}

get_n_lang_fam(glotto_l1)
get_n_lang_fam(phoible_langs)
get_n_lang_fam(asjp_langs)
get_n_lang_fam(grambank_langs)
get_n_lang_fam(intersect_langs)

# top 10 largest languages missing from the databases

langs_not_in_phoible = speakers %>%
  filter(!glottocode %in% phoible$glottocode) %>%
  slice_max(speakers, n = 5)  %>%
  rowwise() %>%
  mutate(Phoible = paste0(language, "; ", glottocode,"; ", speakers)) %>%
  select(Phoible)

langs_not_in_grambank = speakers %>%
  filter(!glottocode %in% grambank$glottocode) %>%
  slice_max(speakers, n = 5)  %>%
  rowwise() %>%
  mutate(Grambank = paste0(language, "; ", glottocode,"; ", speakers)) %>%
  select(Grambank)

langs_not_in_asjp = speakers %>%
  filter(!glottocode %in% asjp$glottocode) %>%
  slice_max(speakers, n = 5)  %>%
  rowwise() %>%
  mutate(ASJP = paste0(language, "; ", glottocode,"; ", speakers)) %>%
  select(ASJP)

# print table
langs_not_in_asjp %>%
  cbind(langs_not_in_grambank) %>%
  cbind(langs_not_in_phoible) %>%
  xtable()


