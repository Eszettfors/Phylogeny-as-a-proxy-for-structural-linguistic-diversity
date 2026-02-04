library(tidyverse)

# this script flattens the constructed similarity matrices, joins them, and exports them as a csv

lex_sim_matrix = readRDS("data/processed/lex_sim_matrix.rds")
morphosyntax_sim_mat = readRDS("data/processed/morphosyntax_sim_mat.rds")
phon_sim_matrix = readRDS("data/processed/phon_sim_matrix.rds")
phyl_sim_matrix = readRDS("data/processed/phyl_sim_mat.rds")



#### flatten
lex_sim_df = lex_sim_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "lang_1") %>%
  pivot_longer(cols = !lang_1, names_to = "lang_2", values_to = "lexical_similarity") %>%
  filter(lang_1 < lang_2) 

morphsyn_sim_df = morphosyntax_sim_mat %>%
  as.data.frame() %>%
  rownames_to_column(var = "lang_1") %>%
  pivot_longer(cols = !lang_1, names_to = "lang_2", values_to = "morphosyntactic_similarity") %>%
  filter(lang_1 < lang_2) 


phon_sim_df = phon_sim_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "lang_1") %>%
  pivot_longer(cols = !lang_1, names_to = "lang_2", values_to = "phoneme_inventory_similarity") %>%
  filter(lang_1 < lang_2) 


phyl_sim_df = phyl_sim_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "lang_1") %>%
  pivot_longer(cols = !lang_1, names_to = "lang_2", values_to = "phylogenetic_similarity") %>%
  filter(lang_1 < lang_2) 


#### join
full_df = lex_sim_df %>%
  inner_join(morphsyn_sim_df, join_by(lang_1, lang_2)) %>%
  inner_join(phon_sim_df, join_by(lang_1, lang_2)) %>%
  inner_join(phyl_sim_df, join_by(lang_1, lang_2))


# write
write_csv(full_df, "data/processed/all_similarities.csv")
  