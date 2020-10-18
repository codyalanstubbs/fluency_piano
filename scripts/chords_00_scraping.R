# Load the Library
library(RSelenium)
library(XML)
library(tidyverse)

eCaps <- list("chrome_binary" = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
# start the server and browser(you can use other browsers here)
rD <- rsDriver(browser=c("chrome"), chromever="86.0.4240.22", extraCapabilities = eCaps, port = 4604L)
driver <- rD$client

# retrieve all math textbooks----
# navigate to textbook subjects page
driver$navigate("https://www.pianochord.org/")

# find and retrieve all the links on the page
elements <- driver$findElements("css selector","li a")
links <- unlist(sapply(elements, function(x){x$getElementAttribute("href")}))

# filter out non-book links
links <- as.data.frame(links)
chord_links <- 
        links %>% slice(4:length(links))


# retrieve table of contents data from all textbooks ----
all_chord_cat_links <- data.frame() # create empty data frame for collecting

for (i in chord_links$links){
        # navigate to the books website
        driver$navigate(i)
        
        # find the elements for navigating to the Table of contents
        Sys.sleep(.5)
        elements <- driver$findElements(using = "css selector","a.in-line")
        chord_cat_links <- unlist(sapply(elements, function(x){x$getElementAttribute("href")})) %>% as.data.frame()
        Sys.sleep(2.5)
        
        # adding TOC text to collection data frame
        all_chord_cat_links <- rbind(all_chord_cat_links, chord_cat_links)
        
}

# all_chord_cat_links <- all_chord_cat_links %>% 
#         filter(
#                 ! `.` %in% 
#                         c("https://www.pianochord.org/c6-9.html",
#                           "https://www.pianochord.org/c-extended.html",
#                           "https://www.pianochord.org/cm9.html", 
#                           "https://www.pianochord.org/cmaj9.html"))

# retrieve table of contents data from all textbooks ----
all_chord_data <- data.frame() # create empty data frame for collecting

for (i in all_chord_cat_links$.){
        # navigate to the books website
        driver$navigate(i)
        
        elements <- driver$findElements(using = "css selector","div")
        div_text <- unlist(lapply(elements, function(x){x$getElementText()})) 
        str_pres <- str_detect(div_text, "Left hand") 

        if (TRUE %in% str_pres){
                # find the elements for navigating to the Table of contents
                
                elements <- driver$findElements(using = "css selector","div h1")
                chord <- unlist(lapply(elements, function(x){x$getElementText()}))
                
                Sys.sleep(.5)
                elements <- driver$findElements(using = "css selector","div p")
                description <- unlist(lapply(elements, function(x){x$getElementText()}))
                description <- description[2]
                Sys.sleep(.5)
                
                Sys.sleep(.5)
                elements <- driver$findElements(using = "css selector","span.notes")
                notes <- unlist(lapply(elements, function(x){x$getElementText()}))
                Sys.sleep(.5)
                
                Sys.sleep(.5)
                elements <- driver$findElements(using = "css selector","div.fingerings")
                fingers <- unlist(lapply(elements, function(x){x$getElementText()}))
                Sys.sleep(.5)
                
                chord_data <- 
                        data.frame(chord, description, notes, fingers, reference = i) %>% 
                        mutate(notes = gsub("Notes: ", "", notes)) %>% 
                        mutate(notes = gsub(" - ", " ", notes))  %>% 
                        mutate(fingers = gsub("-", " ", fingers)) %>%
                        mutate(fingers = gsub("Left hand:", "lh", fingers)) %>% 
                        mutate(fingers = gsub("Right hand:", "rh", fingers))
                
        } else{
                # find the elements for navigating to the Table of contents
                
                Sys.sleep(.5)
                elements <- driver$findElements(using = "css selector","div h1")
                chord <- unlist(lapply(elements, function(x){x$getElementText()}))
                Sys.sleep(.5)
                
                Sys.sleep(.5)
                elements <- driver$findElements(using = "css selector","div p")
                description <- unlist(lapply(elements, function(x){x$getElementText()}))
                description <- description[2]
                Sys.sleep(.5)
                
                Sys.sleep(.5)
                elements <- driver$findElements(using = "css selector","span.notes")
                notes <- unlist(lapply(elements, function(x){x$getElementText()}))
                Sys.sleep(.5)
                
                chord_data <- 
                        data.frame(chord, description, notes, fingers = "two-hands", reference = i) %>% 
                        mutate(notes = gsub("Notes: ", "", notes)) %>% 
                        mutate(notes = gsub(" - ", " ", notes))
        }
        
        # adding TOC text to collection data frame
        all_chord_data <- rbind(all_chord_data, chord_data)
        
}

#close the driver
driver$close()

#close the server
rD$server$stop()

# final modifications and write to csv ----
write.csv(all_chord_data, "data/chords_00_scraped.csv", row.names = FALSE)

