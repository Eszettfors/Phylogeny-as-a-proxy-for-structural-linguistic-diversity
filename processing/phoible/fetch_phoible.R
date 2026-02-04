library(devtools)

if ("rcldf" %in% installed.packages()) {
  print("rcldf is installed")
} else{
  print("installing rcldf......")
  install_github("SimonGreenhill/rcldf", dependencies = TRUE)
}

library(rcldf)

#### this script downloads Phoible as an R object using the rcldf pacakge (Greenhill SJ 2025)
# and saves it as a .rds####

phoible = rcldf::cldf(mdpath = "https://zenodo.org/records/2677911/files/cldf-datasets/phoible-v2.0.1.zip")

saveRDS(phoible, "data/raw/phoible_database.rds")
