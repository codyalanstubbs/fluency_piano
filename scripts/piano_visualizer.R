library(tidyverse)

k_data <- 
        read_csv("key-data-88 - keys.csv") %>% 
        mutate(viz.y = key.color) %>% 
        mutate(viz.y = gsub("white", "1", key.color)) %>%
        mutate(viz.y = gsub("black", "0.667", viz.y)) %>% 
        mutate(viz.y = as.numeric(viz.y)) 

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
