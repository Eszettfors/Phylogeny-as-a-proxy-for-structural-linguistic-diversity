library(tidyverse)
library(vegan)
library(stringdist)

# this script calculates 4 similarity matrices, one for each database based on the intersected languages. It then flattes the matrices and exports the data
# as a csv

# read data
phoible = read_csv("data/processed/phoible_data.csv")
asjp = read_csv("data/processed/asjp_wide.csv")
grambank = read_csv("data/processed/grambank_data.csv")
glotto = read_csv("data/processed/glottolog.csv")
intersect = read_csv("data/processed/intersected_languages.csv")

#### lexical similarity requires at least 28 words -> subset
too_few_words = asjp %>%
  filter(words < 28) %>%
  pull(glottocode)# 621 languages

intersect %>%
  filter(glottocode %in% too_few_words) # 18 languages



# langs for which similarities will be calculated
intersect_langs = intersect %>%
  filter(!glottocode %in% too_few_words) %>%
  pull(glottocode)
length(intersect_langs) # 1048

#### phoneme inventory similarity ######

get_phon_similarity_matrix = function(langs){
  # takes a vector of glotto codes and returns a jaccard similarity matrix for all languages present in PHOIBLE
  # utilizes a binary presense/absense matrix for efficiency

  # get glottocode from phoible to the languages of interest -> only the glottocodes which exist in Phoible are considered
  langs_in_phoible = phoible %>%
    filter(glottocode %in% langs) %>%
    distinct(glottocode) %>%
    pull(glottocode)
  
  # generate a binary matrix of the phonemes in the languages of interest
  binary_matrix = phoible %>% 
    filter(glottocode %in% langs_in_phoible) %>%  # filter to relevant languages
    select(glottocode, phoneme) %>% # select the phonemes
    distinct() %>% # make sure no phonemes are double counted
    mutate(presence = 1) %>% # indicate that the phonemes exist in the inventory
    pivot_wider(names_from = phoneme, values_from = presence, values_fill = list(presence = 0)) %>% # create one column for each phoneme which exist in the data
    select(-glottocode) %>%
    as.matrix() # turn binary presense values into a matrix
  
  # generate a distance matrix. Looks at the cases where both languages have 1 = presense. Divides with the cases where at least one of the languages have 1 = presense
  distance_matrix = vegdist(binary_matrix, method = "jaccard") %>%
    as.matrix()
  sim_matrix = 1 - distance_matrix
  
  # name the matrix rows and columns according to glottocode
  colnames(sim_matrix) = langs_in_phoible
  rownames(sim_matrix) = langs_in_phoible
  
  return(sim_matrix)
}

# generate matrix
phon_sim_matrix = get_phon_similarity_matrix(intersect_langs)

# write
write_rds(phon_sim_matrix, "data/processed/phon_sim_matrix.rds")


##### lexical similarity

get_concept_vector = function(langs){
  # this function takes a glottocode and returns a vector with concept values from ASJP
  if (!langs %in% asjp$glottocode){
    stop(error)
  }
  vec = asjp %>%
    filter(glottocode == langs) %>%
    select(!c("lang_id", "ISO6393", "glottocode", "words", "completeness", "language", "macroarea", "family", "Latitude", "Longitude")) %>%
    t() %>%
    as.vector()
  return(vec)
}


get_mean_ldn_sim = function(v1, v2){
  # this function takes two language vectors with concepts and calculates the mean normalized levenshtein distance 
  # between them
  
  # get vector with normalized levenshtein distances
  ldn = stringdist(v1, v2, method = "lv") / pmax(nchar(v1), nchar(v2))
  
  # average ldn
  mean_ldn = mean(ldn, na.rm = TRUE)
  
  return(1 - mean_ldn)
}


get_ldn_sim_matrix = function(langs){
  # function takes a vector of glottocodes and returns a matrix with pairwise similarity
  
  sim_m = matrix(NA, ncol = length(langs), nrow = length(langs), dimnames = list(langs, langs))
  
  # populate upper triangle
  i = 1
  for (lang1 in langs){
    for (lang2 in langs[i:length(langs)]){
      concept_vec_1 = get_concept_vector(lang1)
      concept_vec_2 = get_concept_vector(lang2)
      
      sim = get_mean_ldn_sim(concept_vec_1, concept_vec_2)
      
      sim_m[lang1, lang2] = sim
      sim_m[lang2, lang1] = sim
      
    }
    i = i + 1
  }
  return(sim_m)
}

# generate matrix
lex_sim_matrix = get_ldn_sim_matrix(intersect_langs)

# write
write_rds(lex_sim_matrix, "data/processed/lex_sim_matrix.rds")


##### morphosyntactic similarity

grambank_wide = grambank %>%
  select(glottocode, Parameter_ID, Value) %>%
  pivot_wider(names_from = Parameter_ID, values_from = Value, values_fill = NA)

get_feature_vector = function(lang) {
  # takes a glottocode and returns a vector with feature values from grambank
  vec = grambank_wide %>% 
    filter(glottocode == lang) %>%
    select(!glottocode) %>%
    t() %>%
    as.vector()
  
  return(vec)
}

t1 = get_feature_vector("swed1254")
t2 = get_feature_vector("dani1285")
t3 = get_feature_vector("hung1274")

get_feature_overlap = function(l1, l2){
  # this function takes two language feature vectors of size n with categorical values.
  # it subsets the vectors to features for which both vectors are defined and returns the fraction
  # of overlapping values and the number of common features as a measure of reliability
  # if the number of overlapping features are smaller than 49 -> returns NA
  
  # create df of languages with features as columns
  df = t(data.frame(l1, l2))
  
  # subset to features without any missing values = both languages are defined
  df = df[, colSums(is.na(df)) == 0]
  
  #the number of features for which both languages are defined
  def_length = ncol(df)
  
  # check if the length is null or smaller than 49 -> return NA and def_length
  if (is.null(def_length) || def_length < 49) {
    return(c(NA, def_length))
  }
  else{
    # count instances of the language vectors overlapping. TRUE = same, FALSE = different
    tab = table(df[1,] == df[2,])
    
    # frac overlap = TRUE / (TRUE + FALSE)
    overlap = as.numeric(tab["TRUE"] / sum(tab))
    
    return(c(overlap, def_length))
  }
}
get_feature_overlap(t1,t2)



get_feature_overlap_matrix = function(langs){
  # takes a vector of glottocodes and outputs a matrix with feature overlap between them
  
  
  # retrive glottocodes present in grambank
  langs_in_gram = grambank_wide %>%
    filter(glottocode %in% langs) %>%
    pull(glottocode)
  
  # create an empty matrix of with every language as a row and column entry to hold similarity values
  n = length(langs_in_gram)
  sim_m = matrix(NA,
                 ncol = n,
                 nrow = n,
                 dimnames = list(langs_in_gram, langs_in_gram))
  
  # copy the empty matrix to hold the number of features for which any two languages are defined
  def_m = sim_m
  
  # loop through upper triangle and populate both upper and lower triangle
  i = 1
  for (lang1 in langs_in_gram){
    for (lang2 in langs_in_gram[i:length(langs_in_gram)]){
      feature_vec_1 = get_feature_vector(lang1)
      feature_vec_2 = get_feature_vector(lang2)
      
      distance_def = get_feature_overlap(feature_vec_1, feature_vec_2)
      
      sim_m[lang1, lang2] = distance_def[1]
      sim_m[lang2, lang1] = distance_def[1]
      
      def_m[lang1, lang2] = distance_def[2]
      def_m[lang2, lang1] = distance_def[2]
    }
    i = i + 1
  }
  return(list(sim_m, def_m))
}

morphosyntax_matrices = get_feature_overlap_matrix(intersect_langs)

morphosyntax_sim_mat = morphosyntax_matrices[[1]]
write_rds(morphosyntax_sim_mat, "data/processed/morphosyntax_sim_mat.rds")


###### phylogenetic similarity

get_phyl_similarity_matrix = function(langs){
  # takes a vector of glotto codes and returns a similarity matrix with 1 - jaccarddistance
  
  #langs in glotto
  langs_in_glotto = glotto %>% 
    filter(glottocode %in% langs) %>% pull(glottocode)
  
  
  # calculate distances. Spearate the classification and pivot wide. If a branch belongs to a language -> 1; if not: 0. The measure compares the cases of branches belonging to both langauage
  # and divides by the distinct cases where any of the languages have 1. If both languages have 0, it is not considered for the calculation
  dist_matrix = glotto %>%
    select(glottocode, classification) %>%
    filter(glottocode %in% langs_in_glotto) %>%
    separate_rows(classification, sep = "/") %>%
    pivot_wider(names_from = classification, values_from = classification, values_fill = NA) %>%
    select(-c(glottocode)) %>%
    mutate(across(everything(), ~ case_when(
      !is.na(.) ~ 1,
      TRUE ~ 0
    ))) %>%
    as.matrix() %>%
    vegdist(method = "jaccard") %>% 
    as.matrix()
  
  # convert to similarity
  sim_matrix = 1-dist_matrix
  
  # add ISO to names
  rownames(sim_matrix) = langs_in_glotto
  colnames(sim_matrix) = langs_in_glotto
  return(sim_matrix)
}
phyl_sim_mat = get_phyl_similarity_matrix(intersect_langs)

write_rds(phyl_sim_mat, "data/processed/phyl_sim_mat.rds")


