library(tidyverse)

k_data <- 
        read_csv("data/key-data-88 - keys.csv") %>% 
        mutate(viz.y = key.color) %>% 
        mutate(viz.y = gsub("white", "1", key.color)) %>%
        mutate(viz.y = gsub("black", "0.667", viz.y)) %>% 
        mutate(viz.y = as.numeric(viz.y)) %>% 
        separate_rows(notes, sep = " ") 

p <- k_data %>% 
        ggplot(aes(x = viz.x, y = viz.y, fill = key.color)) +
        geom_col(
                data = k_data %>% filter(key.color == "white"),
                position = position_dodge(), color = "black",
                width = 1
        ) +
        geom_col(
                data = k_data %>% filter(key.color == "black"),
                position = position_dodge(), color = "white",
                width = 0.75
        ) +
        geom_text(
                data = k_data %>% filter(key.color == "white"),
                color = "black",
                aes(label = notes),
                angle = 90, hjust = "left"
        ) +
        geom_text(
                data = k_data %>% filter(key.color == "black"),
                color = "white",
                aes(label = notes),
                angle = 90, hjust = "left"
        ) +
        scale_fill_manual(values = c("black", "white")) +
        scale_y_reverse(
                limits = c(1,0), 
                expand = c(0,0)
        ) +
        scale_x_continuous(
                breaks = 1:52,
                labels = b,
                position = "top"
        ) +
        theme_classic() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              axis.line.y = element_blank(),
              #axis.line.x = element_blank(),
              axis.title.x = element_blank(),
              legend.position = "none")

p
b <- k_data %>% filter(key.color == "white") %>% select(key.number)
b <- unique(b$key.number)
pb <- p + geom_col( # middle C
        aes(x = 24, y = 1),
        fill = "gold", color = "black",
        width = 1 )
pb$layers <- c(pb$layers[1], pb$layers[5], pb$layers[2], pb$layers[3], pb$layers[4])
pb


notes_lst <- c("A", "C#", "E")
chrd <- "A major"
display_notes <- function(chrd = NULL, notes_lst){
        # Load data for piano ----
        k_data <- 
                read_csv("data/key-data-88 - keys.csv") %>% 
                mutate(viz.y = key.color) %>% 
                mutate(viz.y = gsub("white", "1", key.color)) %>%
                mutate(viz.y = gsub("black", "0.667", viz.y)) %>% 
                mutate(viz.y = as.numeric(viz.y))  %>% 
                separate_rows(notes, sep = " ") %>% 
                filter(key.number >= 28, key.number <= 63) 
        
        p <- k_data %>% 
                filter(octave == 4) %>% 
                ggplot(aes(x = viz.x, y = viz.y, fill = key.color)) +
                geom_col(
                        data = k_data %>% filter(key.color == "white"),
                        position = position_dodge(), color = "black",
                        width = 1
                ) +
                geom_col(
                        data = k_data %>% filter(key.color == "black"),
                        position = position_dodge(), color = "white",
                        width = 0.75
                ) +
                scale_fill_manual(values = c("black", "white")) +
                scale_y_reverse(
                        limits = c(1,0), 
                        expand = c(0,0)
                ) +
                scale_x_continuous(
                        breaks = 1:52,
                        labels = b,
                        position = "top"
                ) +
                theme_classic() +
                theme(axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.y = element_blank(),
                      axis.line.y = element_blank(),
                      #axis.line.x = element_blank(),
                      axis.title.x = element_blank(),
                      legend.position = "none")
        
        #----
        if (chrd == NULL){
                visual_data <- k_data %>% 
                        filter(octave == 4) %>% 
                        filter(notes %in% notes_lst)
                
                p_w_chord <- # The piano build ####
                        p + 
                        geom_text(
                                data = k_data %>% filter(key.color == "white"),
                                color = "black",
                                aes(label = notes),
                                angle = 0, nudge_y = 0.1
                        ) +
                        geom_text(
                                data = k_data %>% filter(key.color == "black", str_detect(notes, "#$")),
                                color = "white",
                                aes(label = notes),
                                angle = 0, nudge_y = 0.125
                        ) +
                        geom_text(
                                data = k_data %>% filter(key.color == "black", str_detect(notes, "b$")),
                                color = "white",
                                aes(label = notes),
                                angle = 0, nudge_y = 0.075
                        ) +
                        geom_col( 
                                data = visual_data %>% filter(key.color == "black"),
                                aes(x = viz.x, y = viz.y),
                                fill = "purple", color = "black",
                                width = 0.75 ) + 
                        geom_col( 
                                data = visual_data %>% filter(key.color == "white"),
                                aes(x = viz.x, y = viz.y),
                                fill = "purple", color = "black",
                                width = 1 )
                # Rearrange the plot layers to optimize visibility ####
                p_w_chord$layers <- c(p_w_chord$layers[1], p_w_chord$layers[7], p_w_chord$layers[2], p_w_chord$layers[6], p_w_chord$layers[3], p_w_chord$layers[4], p_w_chord$layers[5])
                
                # Print the figure
                p_w_chord
                
        } else{
                chord_data <- read_csv("data/chords_01_cleaned.csv") %>% 
                        filter(key_chord_short %in% chrd) %>% 
                        select(key_chord_short, notes) %>% 
                        filter(notes %in% chord_data$notes)
                
                visual_data <- k_data %>% 
                        filter(octave == 4) %>% 
                        filter(notes %in% chord_data$notes)
                
                p_w_chord <- # The piano build ####
                p + 
                        geom_text(
                                data = k_data %>% filter(key.color == "white"),
                                color = "black",
                                aes(label = notes),
                                angle = 0, nudge_y = 0.1
                        ) +
                        geom_text(
                                data = k_data %>% filter(key.color == "black", str_detect(notes, "#$")),
                                color = "white",
                                aes(label = notes),
                                angle = 0, nudge_y = 0.125
                        ) +
                        geom_text(
                                data = k_data %>% filter(key.color == "black", str_detect(notes, "b$")),
                                color = "white",
                                aes(label = notes),
                                angle = 0, nudge_y = 0.075
                        ) +
                        geom_col( 
                                data = visual_data %>% filter(key.color == "black"),
                                aes(x = viz.x, y = viz.y),
                                fill = "purple", color = "black",
                                width = 0.75 ) + 
                        geom_col( 
                                data = visual_data %>% filter(key.color == "white"),
                                aes(x = viz.x, y = viz.y),
                                fill = "purple", color = "black",
                                width = 1 )
                
                # Rearrange the plot layers to optimize visibility ####
                p_w_chord$layers <- c(p_w_chord$layers[1], p_w_chord$layers[7], p_w_chord$layers[2], p_w_chord$layers[6], p_w_chord$layers[3], p_w_chord$layers[4], p_w_chord$layers[5])
                
                # Print the figure
                p_w_chord
                
                
        }
}