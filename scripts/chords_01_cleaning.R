library(tidyverse)
data <- read_csv("data/chords_00_scraped.csv")

data$description[1] # \n can be used to create separate columns

new_desc_columns <-
        data %>%
        mutate(description = 
                       gsub("EXPLANATION: ", "_EXPLANATION: ", description)) %>% 
        mutate(description = 
                       gsub("THEORY: ", "_THEORY: ", description)) %>% 
        mutate(description = 
                       gsub("FINGERINGS: ", "_FINGERINGS: ", description)) %>% 
        separate(
                col = "description",
                sep = "_",
                into = c("description.overview",
                         "description.explanation",
                         "description.theory",
                         "description.fingerings") ) %>% 
        mutate(description.explanation = gsub("EXPLANATION: ", "", description.explanation)) %>% 
        mutate(description.theory = gsub("THEORY: ", "", description.theory))  %>% 
        mutate(description.fingerings = gsub("FINGERINGS: ", "", description.fingerings)) %>% 
        mutate(fingers = gsub("lh", "lh_", fingers)) %>% 
        mutate(fingers = gsub("rh ", ",rh", fingers)) %>%
        mutate(fingers = gsub("two-hands", "two-hands_", fingers)) %>%
        separate(
                col = fingers,
                remove = F,
                sep = "_",
                into = c("number.of.hands", "fingering.fingers")) %>% 
        select(-fingering.fingers) %>% 
        mutate(number.of.hands = gsub("^lh", "1", number.of.hands)) %>%
        mutate(number.of.hands = gsub("two-hands", "2", number.of.hands)) %>% 
        mutate(fingers = gsub("lh_", "", fingers)) %>% 
        mutate(fingers = gsub("rh", "", fingers)) %>%
        mutate(fingers = gsub("two-hands", NA, fingers)) %>% 
        separate(
                col = fingers,
                sep = ",",
                into = c("fingers.lh", "fingers.rh")) %>% 
        separate_rows(notes, sep = "/") %>% 
        filter(notes != "NA") %>% 
        mutate(chord = gsub("chord", "", chord)) %>% 
        separate(
                col = chord,
                into = c("key", "chord"),
                extra = "merge" ) 

chords <- if_else(new_desc_columns$chord == "", new_desc_columns$key, new_desc_columns$chord)

new_desc_columns <- 
        new_desc_columns %>%  
        mutate(chord = chords)

cleansed_data <- new_desc_columns

# final modifications and write to csv ----
write.csv(cleansed_data, "data/chords_01_cleaned.csv", row.names = FALSE)
