library(tidyverse)
library(ggplot2)
library(ggforce)
library(ggtext)
library(extrafont)
library(glue)
library(ggimage)
library(png)
library(grid)
#font_import()
loadfonts(device = "win")

theme_set(theme_cowplot())

setwd("~/Files/DS_projects/Project_1/data")

raw_data <- read_delim('laliga_xg_xga.txt', delim = ',')

xGpg_avg <- mean(raw_data$xG_per90)
xGApg_avg <- mean(raw_data$xGA_per90)

raw_data$images <- c('atletico_madrid.png', 'real_madrid.png', 'barcelona.png', 
                     'sevilla.png', 'villareal.png', 'real_sociedad.png', 
                     'granada.png', 'cadiz.png', 'celta_de_vigo.png', 
                     'betis.png', 'levante.png', 'athletic_bilbao.png', 
                     'getafe.png', 'valencia.png', 'eibar.png', 'valladolid.png',
                     'alaves.png', 'elche.png', 'osasuna.png', 'huesca.png')
view(raw_data)

setwd("~/Files/DS_projects/Project_1/data/logos_imgs")

img <- readPNG('laliga.png')
laliga_logo <- rasterGrob(img, interpolate = TRUE)

bad_box <- data.frame(xmin = -Inf, xmax = xGpg_avg, 
                      ymin = -Inf, ymax = xGApg_avg)

chance_creation_box <- data.frame(xmin = xGpg_avg, xmax = Inf, 
                                  ymin = -Inf, ymax = xGApg_avg)

midfield_progress_box <- data.frame(xmin = -Inf, xmax = xGpg_avg, 
                                    ymin = xGApg_avg, ymax = Inf)

dual_box <- data.frame(xmin = xGpg_avg, xmax = Inf, 
                       ymin = xGApg_avg, ymax = Inf)

#FIRST PLOT
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
  geom_image(aes(image= images), size= 0.09, asp = 1, by = 'height') +
  annotation_custom(laliga_logo, 1.8, 2.2, -1.6, -1.4) + 
  scale_x_continuous(limit = c(0.6, 2.3),
                     labels = seq(0, 2.5, 0.5),
                     breaks = seq(0, 2.5, 0.5)) +
  scale_y_reverse(limit = c(1.70, 0.70),
                  labels = seq(0, 2, 0.25),
                  breaks = seq(0, 2, 0.25)) +
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
           y = 0.81, x = 1.25, hjust = 0, color = 'grey20', size = 5,
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

#SECOND PLOT

Gpg_avg <- mean(raw_data$GF_per90)
GApg_avg <- mean(raw_data$GA_per90)

max(raw_data$GA_per90)
min(raw_data$GA_per90)

bad_box_2 <- data.frame(xmin = -Inf, xmax = Gpg_avg, 
                      ymin = -Inf, ymax = GApg_avg)

chance_creation_box_2 <- data.frame(xmin = Gpg_avg, xmax = Inf, 
                                  ymin = -Inf, ymax = GApg_avg)

midfield_progress_box_2 <- data.frame(xmin = -Inf, xmax = Gpg_avg, 
                                    ymin = GApg_avg, ymax = Inf)

dual_box_2 <- data.frame(xmin = Gpg_avg, xmax = Inf, 
                       ymin = GApg_avg, ymax = Inf)

setwd("~/Files/DS_projects/Project_1/data/logos_imgs")

G_GA_laliga_2021_plot <- ggplot(data = raw_data, aes(x = GF_per90, 
                                                     y = GA_per90)) + 
  geom_rect(data = chance_creation_box_2, aes(x = NULL, y = NULL, 
                                            xmin = xmin, xmax = xmax, 
                                            ymin = ymin, ymax = ymax), 
            fill = 'green', alpha = 0.1) + 
  geom_rect(data= bad_box_2, aes(x = NULL, y = NULL, 
                               xmin = xmin, xmax = xmax,
                               ymin = ymin, ymax = ymax),
            fill = 'yellow', alpha = 0.1) +
  geom_rect(data= midfield_progress_box_2, aes(x = NULL, y = NULL, 
                                             xmin = xmin, xmax = xmax,
                                             ymin = ymin, ymax = ymax),
            fill = 'red', alpha = 0.1) + 
  geom_rect(data= dual_box_2, aes(x = NULL, y = NULL, 
                                xmin = xmin, xmax = xmax,
                                ymin = ymin, ymax = ymax),
            fill = 'yellow', alpha = 0.1) +
  geom_hline(aes(yintercept = GApg_avg), colour = 'grey20', size= 1.2) +
  geom_vline(aes(xintercept = Gpg_avg), colour = 'grey20', size= 1.2) +
  geom_image(aes(image= images), size= 0.09, asp = 1, by = 'height') +
  annotation_custom(laliga_logo, 1.9, 2.4, -1.68, -1.38) + 
  scale_x_continuous(limit = c(0.7, 2.3),
                     labels = seq(0, 2.5, 0.5),
                     breaks = seq(0, 2.5, 0.5)) +
  scale_y_reverse(limit = c(1.75, 0.25),
                  labels = seq(0, 2, 0.25),
                  breaks = seq(0, 2, 0.25)) +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 1.74, x = 0.7, hjust = 0, color = 'red', size = 5,
           label = 'Bad Attack | Bad Defense') +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 1.74, x = 1.85, hjust = 0, color = '#7f7f00', size = 5,
           label = 'Good Attack | Bad Defense') +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 0.25, x = 0.7, hjust = 0, color = '#7f7f00', size = 5,
           label = 'Bad Attack | Good Defense') +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 0.25, x = 1.85, hjust = 0, color = '#228B22', size = 5,
           label = 'Good Attack | Good Defense') +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 0.5, x = 1.25, hjust = 0, color = 'grey20', size = 5,
           label = glue('Average: {round(Gpg_avg,2)} Goals per Game')) +
  annotate(geom = 'text', family = 'Comic Sans MS', fontface = 'bold',
           y = 1.25, x = 1.6, hjust = 0, color = 'grey20', size = 5,
           label = glue('Average: {round(GApg_avg,2)} Goals Against per Game')) +
  labs(title = 'Goals Taken (GF) vs Goals Conceded (GA)',
       subtitle = 'La Liga 2020 - 2021',
       x = 'G per Game',
       y = 'GA per Game') + 
  theme_minimal() + 
  theme(text = element_text(size = 30, family = 'Comic Sans MS'),
        plot.title = element_markdown(size = 20), 
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        panel.grid.major = element_line(size = 2),
        panel.grid.minor = element_line(size = 2))

G_GA_laliga_2021_plot

#ggsave('R_plot_1', plot = xG_xGA_laliga_2021_plot, path = '~/Files/DS_projects',
#       width = 11, height = 8, device = 'tiff', dpi = 700)
#ggsave('R_plot_2', plot = G_GA_laliga_2021_plot, path = '~/Files/DS_projects',
#       width = 11, height = 8, device = 'tiff', dpi = 700)

