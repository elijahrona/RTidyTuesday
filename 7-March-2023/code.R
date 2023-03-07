library(tidyverse)
library(geodata)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load('2023-03-07')
numbats <- tuesdata$numbats
rm(tuesdata)
numbats <- numbats[,1:12]
numbats <- numbats[,-3]
numbats <- na.omit(numbats)

numbats_function <- function(x) {
  numbats%>%
    mutate(month = factor(month, levels = c('Jan','Feb','Mar','Apr', 'May',
                                            'Jun','Jul','Aug','Sep', 'Oct',
                                            'Nov','Dec'))) %>%
    mutate(wday = factor(wday, levels = c('Mon','Tue','Wed','Thu', 'Fri',
                                            'Sat','Sun'))) %>%
    mutate(variable = as.factor(.data[[x]])) %>%
    group_by(variable) %>%
    summarise(count = length(variable)) %>%
    ggplot(aes(x = variable, y = count)) + 
    geom_bar(color = '#3b1e02', stat = "identity", fill="#3b1e02") +
    theme(panel.background = element_rect(fill = "#eba55e",
                                          colour = "#eba55e",
                                          size = 0.5, linetype = "solid"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#eba55e")) +
    xlab(label = "") +
    ylab(label = "") +
    guides(fill=FALSE) 
}

year <- numbats_function("year") +
  coord_flip() +
  geom_text(aes(label = count, hjust = 1), size = 4, color = "#eba55e") +
  theme(axis.text = element_text(color="#3b1e02", size = 12),
        axis.ticks.x =  element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y =  element_blank())
year

month <- numbats_function("month") +
  geom_text(aes(label = count, vjust = 1), size = 4, color = "#eba55e") +
  theme(axis.text = element_text(color="#3b1e02", size = 12),
        axis.ticks.y =  element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x =  element_blank())
month

wday <- numbats_function("wday") +
  geom_text(aes(label = count, vjust = 1), size = 4, color = "#eba55e") +
  theme(axis.text = element_text(color="#3b1e02", size = 12),
        axis.ticks.y =  element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x =  element_blank())
wday

hour <- numbats_function("hour") +
  coord_flip() +
  geom_text(aes(label = count, hjust = 1), size = 4, color = "#eba55e") +
  theme(axis.text = element_text(color="#3b1e02", size = 12),
        axis.ticks.x =  element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y =  element_blank())+
  xlab(label = "Hour of the Day")
hour


Australia_map <- geodata::gadm("Australia",
                        level = 1,
                        path = tempdir()) |>
  sf::st_as_sf()

numbat_map <- Australia_map %>% ggplot() +
  geom_sf(fill = "#3b1e02") +
  geom_point(data = numbats, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 16, fill = "black") +
  coord_sf(xlim = c(110, 155), ylim = c(-45, -9), expand = FALSE) +
  theme(panel.background = element_rect(fill = "#eba55e",
                                        colour = "#eba55e",
                                        size = 0.5, linetype = "solid"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#eba55e"),
        axis.ticks.x =  element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y =  element_blank(),
        axis.text.y = element_blank()) +
  xlab(label = "") +
  ylab(label = "")
numbat_map

#Combine plots with Microsoft Excel or use the patchwork library
p1 <- hour + numbat_map
p2 <- wday + month 
p3 <- p1 / p2
p4 <- p3 + year
