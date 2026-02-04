library(tidyverse)
library(corrplot)
library(patchwork)
library(xtable)
library(GGally)

# this script analysis the pairwise distances of the intersected languages

df_sim = read_csv("data/processed/all_similarities.csv")

theme_set(theme_minimal(base_size = 14))

# filter cases where lang_1 = lang_2
df_sim = df_sim %>%
  filter(lang_1 != lang_2)


# create a composite index excluding phylogenetic similarity

min_max_norm = function(vec){
  norm = (vec - min(vec)) / (max(vec) - min(vec))
  return(norm)
}

df_composite = df_sim %>%
  mutate(across(!c(lang_1, lang_2), ~as.numeric(scale(.x)))) %>%
  mutate(composite_index = rowMeans(across(!c(lang_1, lang_2, phylogenetic_similarity)), na.rm = TRUE)) %>%
  mutate(composite_index = min_max_norm(composite_index)) %>%
  select(lang_1, lang_2, composite_index)

boxplot(df_composite$composite_index)

# distributions #####

# join and remove NA
df_sim = df_composite %>%
  inner_join(df_sim, join_by(lang_1, lang_2)) %>%
  filter(!is.na(morphosyntactic_similarity))

box_plots = df_sim %>%
  rename("Composite Index" = composite_index, "Morphosyntactic Similarity" = morphosyntactic_similarity, "Lexical Similarity" = lexical_similarity,
         "Phoneme Inventory Similarity" = phoneme_inventory_similarity, "Tree-typology Similarity" = phylogenetic_similarity) %>%
  pivot_longer(cols = !c(lang_1, lang_2)) %>% 
  ggplot(aes(x = value, fill = name, y = name)) + 
  geom_boxplot() + 
  theme(axis.title.y = element_blank(),
        legend.position = "none") + 
  labs(y = element_blank(),
       fill = "Measure",
       title = "a)")



histograms = df_sim %>%
  rename("Composite Index" = composite_index, "Morphosyntactic Similarity" = morphosyntactic_similarity, "Lexical Similarity" = lexical_similarity,
         "Phoneme Inventory Similarity" = phoneme_inventory_similarity, "Tree-topology Similarity" = phylogenetic_similarity) %>%
  pivot_longer(cols = !c(lang_1, lang_2)) %>%
  ggplot(aes(x = value, fill = name)) + 
  geom_histogram(position = "identity", alpha = 0.6, bins = 30, color = "black") +
  theme(legend.position = c(0.8, 0.8)) + 
  labs(x = "Similarity Score",
       fill = "Measure",
       title = "b)")


box_hist = box_plots / histograms
print(box_hist)
ggsave("analysis/plots/box_hist.png", box_hist, width = 12, height = 8, dpi = 300)

### correlations #####

cor(df_sim %>% select(!c(lang_1, lang_2)), method = "pearson") %>%
  corrplot.mixed(lower = "number",
           upper = "circle",
           color = "black")

cor(df_sim %>% select(!c(lang_1, lang_2)), method = "pearson")


### scatterplots
corrs = ggpairs(data = df_sim %>% select(!c(lang_1, lang_2)) %>%
          rename("Composite Index" = composite_index, "Morphosyntactic Similarity" = morphosyntactic_similarity, "Lexical Similarity" = lexical_similarity,
                 "Phoneme Inventory Similarity" = phoneme_inventory_similarity, "Tree-topology Similarity" = phylogenetic_similarity))


hex_fn = function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_hex(bins = 30, ...) +
    scale_fill_viridis_c() +
    theme_minimal()
}


corrs = ggpairs(
  data = df_sim %>% select(!c(lang_1, lang_2)) %>%
    rename(
      "Composite Index" = composite_index,
      "Morphosyntactic Similarity" = morphosyntactic_similarity,
      "Lexical Similarity" = lexical_similarity,
      "Phoneme Inventory Similarity" = phoneme_inventory_similarity,
      "Tree-topology Similarity" = phylogenetic_similarity
    ),
  lower = list(continuous = wrap(hex_fn)),
  upper = list(continuous = wrap("cor", size = 4)),
  diag = list(continuous = wrap("densityDiag", alpha = 0.5, fill = "lightblue"))
)

print(corrs)

ggsave("analysis/plots/corrs.png", corrs, height = 12, width = 12, dpi = 300)


