library(tidyverse)
data <- read_csv("data/chords_00_scraped.csv")

data$description[1] # \n can be used to create separate columns

new_desc_columns <-
        data %>%
        # Add a subscript to description to allow for separating into 4 colums
        # I originally tried .\r\n which worked for most but not all rows despite
        # the presence of it in the unaffected rows
        mutate(description = 
                       gsub("EXPLANATION: ", "_EXPLANATION: ", description)) %>% 
        mutate(description = 
                       gsub("THEORY: ", "_THEORY: ", description)) %>% 
        mutate(description = 
                       gsub("FINGERINGS: ", "_FINGERINGS: ", description)) %>% 
        # Separating into 4 columns
        separate(
                col = "description",
                sep = "_",
                into = c("description.overview",
                         "description.explanation",
                         "description.theory",
                         "description.fingerings") ) %>% 
        # Remove unneccesary strings that are now capture by the columns
        mutate(description.explanation = gsub("EXPLANATION: ", "", description.explanation)) %>% 
        mutate(description.theory = gsub("THEORY: ", "", description.theory))  %>% 
        mutate(description.fingerings = gsub("FINGERINGS: ", "", description.fingerings)) %>%
        # Add subscript and comma patterns for further separation of finger column
        mutate(fingers = gsub("lh", "lh_", fingers)) %>% 
        mutate(fingers = gsub("rh ", ",rh", fingers)) %>%
        mutate(fingers = gsub("two-hands", "two-hands_", fingers)) %>%
        # Separate into two columns
        separate(
                col = fingers,
                remove = F,
                sep = "_",
                into = c("number.of.hands", "fingering.fingers")) %>% 
        # Remove fingering.fingers since it has incomplete and redundant data
        select(-fingering.fingers) %>% 
        # Make number of hands represented by numbers - note it is still a character string
        mutate(number.of.hands = gsub("^lh", "1", number.of.hands)) %>%
        mutate(number.of.hands = gsub("two-hands", "2", number.of.hands)) %>% 
        # Remove irrelevant data from fingers
        mutate(fingers = gsub("lh_", "", fingers)) %>% 
        mutate(fingers = gsub("rh", "", fingers)) %>%
        mutate(fingers = gsub("two-hands", NA, fingers)) %>% 
        # Separate fingers into columns for left and right hands
        separate(
                col = fingers,
                sep = ",",
                into = c("fingers.lh", "fingers.rh")) %>% 
        # Some chords can have multiple versions so separate these into new rows
        separate_rows(notes, sep = "/") %>% 
        # Some chords had an extra row containing NA in chords so we remove them here
        filter(notes != "NA") %>% 
        # Remove unnecessary data in chord column
        mutate(chord = gsub("chord", "", chord)) %>% 
        # Separate the chord column into key(=first note) and chord(=description)
        separate(
                col = chord,
                sep = " ",
                into = c("key", "chord"),
                extra = "merge" ) 

# For keys with not chord description, then just make the chord value the same as the key
chords <- if_else(new_desc_columns$chord == "", new_desc_columns$key, new_desc_columns$chord)
new_desc_columns <- 
        new_desc_columns %>%  
        mutate(chord = chords) %>% 
        # Create a more wordy key_chord combination
        mutate(key_chord_long = paste(key, chord)) %>% 
        # Extract a short-form key_chord combination
        separate(
                col = description.overview,
                sep = " chord",
                into = c("key_chord_short", "remnants"),
                extra = "drop") %>% 
        # Get rid of irrelevant data
        select(-remnants) %>% 
        # Reorder columns for convenience
        select(key_chord_short, key_chord_long, everything())

# Create a separate data set for writing
cleansed_data <- new_desc_columns

# final modifications and write to csv ----
write.csv(cleansed_data, "data/chords_01_cleaned.csv", row.names = FALSE)
