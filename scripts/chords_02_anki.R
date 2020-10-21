library(tidyverse)
data <- read_csv("data/chords_01_cleaned.csv")

major_chords <- data %>% filter(str_detect(chord, "^major$"))
minor_chords <- data %>% filter(str_detect(chord, "^minor$"))

figures <- lapply(unique(major_chords$key_chord_short), display_notes)
figures

names <- unique(major_chords$key_chord_short)

ggsave(
        filename = paste("images/", names[1], ".png", sep =""), 
        plot = figures[[1]], 
        device = "png", 
        width = 5,
        height = 5,
        units = "in"
        ) 

colnames(figures)


library(googlesheets4)
dat <- data.frame(x = c(1, 5, 3, 2, 4, 6))
ss <- gs4_create("gs4-formula-demo", sheets = dat)
ss
summaries <- tibble::tribble(
        ~desc, ~summaries,
        "max", "=max(A:A)",
        "sum", "=sum(A:A)",
        "min", "=min(A:A)",
        "sparkline", "=SPARKLINE(A:A, {\"color\", \"blue\"})"
)
# explicitly declare a column as `googlesheets4_formula`
summaries$summaries <- gs4_formula(summaries$summaries)
summaries
range_write(ss, data = summaries, range = "C1", reformat = FALSE)
miscellany <- tibble::tribble(
        ~desc, ~example,
        "hyperlink", "=HYPERLINK(\"http://www.google.com/\",\"Google\")",
        "image", "=IMAGE(\"https://www.google.com/images/srpr/logo3w.png\")"
)
miscellany$example <- gs4_formula(miscellany$example)
miscellany
sheet_write(miscellany, ss = ss)
        