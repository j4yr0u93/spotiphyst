library(tidyverse)

#source data script

all_data <- read_csv("./data/all_music_data.csv")

ggplot(all_data, mapping = aes(x = valence, y = loudness, color = energy)) +
  scale_color_viridis_c() +
  geom_point()



