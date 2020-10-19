library(tidyverse)
data <- read_csv("data/chords_01_cleaned.csv")

major_chords <- data %>% filter(str_detect(chord, "^major$"))
minor_chords <- data %>% filter(str_detect(chord, "^minor$"))
