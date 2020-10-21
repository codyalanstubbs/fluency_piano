library(tidyverse)
data <- read_csv("data/chords_01_cleaned.csv")

major_chords <- data %>% filter(str_detect(chord, "^major$"))
minor_chords <- data %>% filter(str_detect(chord, "^minor$"))

figures <- lapply(unique(major_chords$key_chord_short), display_notes)
figures

names <- unique(major_chords$key_chord_short)

print_pngs <- function(figures, names){
        for (i in 1:length(names)){
                ggsave(
                        filename = paste("images/", names[i], ".png", sep =""), 
                        plot = figures[[i]], 
                        device = "png", 
                        width = 5,
                        height = 5,
                        units = "in"
                )       
        }
}

print_pngs(figures, names)      


## Create a new workbook
wb <- createWorkbook("Ayanami")

## Add some worksheets
addWorksheet(wb, "Sheet 1")
addWorksheet(wb, "Sheet 2")
addWorksheet(wb, "Sheet 3")

## Insert images
insertImage(wb, "Sheet 1", img, startRow = 5, startCol = 3, width = 6, height = 5)


## Save workbook
## Not run: 
saveWorkbook(wb, "insertImageExample.xlsx", overwrite = TRUE)

## End(Not run)








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
        