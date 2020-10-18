library(readr)
chords_0_scraping <- read_csv("data/chords_00_scraped.csv")

# final modifications and write to csv ----
write.csv(all_chord_data, "data/chords_0_scraped.csv", row.names = FALSE)