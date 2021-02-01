library(tidyverse)
library(ggplot2)
library(ggforce)
library(ggtext)
library(ggrepel)
library(extrafont)
library(glue)
#font_import()
loadfonts(device = "win")

setwd("~/Files/DS_projects/data")

raw_data <- read_delim('laliga_xg_xga.txt', delim = ',')
view(raw_data)

xGpg_avg <- mean(raw_data$xG_per90)
xGApg_avg <- mean(raw_data$xGA_per90)

bad_box <- data.frame(xmin = -Inf, xmax = xGpg_avg, 
                      ymin = -Inf, ymax = xGApg_avg)

chance_creation_box <- data.frame(xmin = xGpg_avg, xmax = Inf, 
                                  ymin = -Inf, ymax = xGApg_avg)

midfield_progress_box <- data.frame(xmin = -Inf, xmax = xGpg_avg, 
                                    ymin = xGApg_avg, ymax = Inf)

dual_box <- data.frame(xmin = xGpg_avg, xmax = Inf, 
                       ymin = xGApg_avg, ymax = Inf)

xG_xGA_laliga_2021_plot <- ggplot(data = raw_data, aes(x = xG_per90, 
                                                y = xGA_per90)) + 
  geom_rect(data = chance_creation_box, aes(x = NULL, y = NULL, 
                                            xmin = xmin, xmax = xmax, 
                                            ymin = ymin, ymax = ymax), 
    fill = 'green', alpha = 0.1) + 
  geom_rect(data= bad_box, aes(x = NULL, y = NULL, 
                               xmin = xmin, xmax = xmax,
                               ymin = ymin, ymax = ymax),
            fill = 'yellow', alpha = 0.1) +
  geom_rect(data= midfield_progress_box, aes(x = NULL, y = NULL, 
                               xmin = xmin, xmax = xmax,
                               ymin = ymin, ymax = ymax),
            fill = 'red', alpha = 0.1) + 
  geom_rect(data= dual_box, aes(x = NULL, y = NULL, 
                               xmin = xmin, xmax = xmax,
                               ymin = ymin, ymax = ymax),
            fill = 'yellow', alpha = 0.1) +
  geom_hline(aes(yintercept = xGApg_avg), colour = 'grey20', size= 1.2) +
  geom_vline(aes(xintercept = xGpg_avg), colour = 'grey20', size= 1.2) +
  geom_point(size= 6) +
  geom_text_repel(aes(label = Squad), size = 6, nudge_y = 0.03, force = 6,
                  min.segment.length = 0, segment.size = 1, fontface = 'bold',
                  segment.color = '#000000', box.padding = unit(5, 'mm'),
                  family = 'Comic Sans MS',
                  max.overlaps = getOption("ggrepel.max.overlaps", default = 15)) +
  scale_x_continuous(limit = c(0.6, 2.3),
                     labels = seq(0, 2.5, 0.5),
                     breaks = seq(0, 2.5, 0.5)) +
  scale_y_reverse(limit = c(1.70, 0.70),
                  labels = seq(0, 2, 0.5),
                  breaks = seq(0, 2, 0.5)) +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 1.7, x = 0.6, hjust = 0, color = 'red', size = 5,
           label = 'Bad Attack | Bad Defense') +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 1.7, x = 1.85, hjust = 0, color = '#7f7f00', size = 5,
           label = 'Good Attack | Bad Defense') +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 0.7, x = 0.6, hjust = 0, color = '#7f7f00', size = 5,
           label = 'Bad Attack | Good Defense') +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 0.7, x = 1.85, hjust = 0, color = '#228B22', size = 5,
           label = 'Good Attack | Good Defense') +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 0.78, x = 1.25, hjust = 0, color = 'grey20', size = 5,
           label = glue('Average: {round(xGpg_avg,2)} xG per Game')) +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 1.25, x = 1.8, hjust = 0, color = 'grey20', size = 5,
           label = glue('Average: {round(xGApg_avg,2)} xGA per Game')) +
  labs(title = 'Quality of Shots Taken (xG) vs Quality of Shots Conceded (xGA)',
       subtitle = 'La Liga 2020 - 2021',
       x = 'xG per Game',
       y = 'xGA per Game') + 
  theme_minimal() + 
  theme(text = element_text(size = 30, family = 'Comic Sans MS'),
        plot.title = element_markdown(size = 20), 
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        panel.grid.major = element_line(size = 2),
        panel.grid.minor = element_line(size = 2))
  
xG_xGA_laliga_2021_plot
