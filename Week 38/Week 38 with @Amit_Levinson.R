library(tidyverse)
library(png)
library(gridGraphics)

df_park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")

# park_visits$year <- as.integer(park_visits$year,na.omit = T)
# park_visits$visitors <- as.integer(park_visits$visitors,na.omit = T)
# park_visits$unit_type <-  as.factor(park_visits$unit_type)

# cleaning the data
df_national_parks <- # try to add prefixes for data.frame so look them up quickly in Rstudio
  df_park_visits %>% 
  filter(year != "Total") %>% 
  mutate_at(vars(year, visitors), as.integer) %>% 
  mutate(unit_type = as.factor(unit_type)) %>% 
  filter(unit_type == "National Park") %>% 
  group_by(year) %>% 
  # mutate(average_per_year = mean(visitors)) %>% # summarize() is suffice
  # select(year, average_per_year) %>%
  summarise(visitors_parks = mean(visitors)) %>% 
  mutate(visitors_parks = visitors_parks/1000)
  # na.omit # use map(df, ~any(is.na(.x))) to check NAs for each column

# preparing icons for later insertion; 
# img1 <- readPNG("sun.png")
# fig1 <- rasterGrob(img1)
# 
# img2 <- readPNG("cloud.png")
# fig2 <- rasterGrob(img2)
# 
# img2a <- readPNG("cloud.png")
# fig2a <- rasterGrob(img2a)
# 
# img3 <- readPNG("stickman.png")
# fig3 <- rasterGrob(img3)

png <- list.files("Week 38", "\\.png$")[-4] # find all PNGs needed
pngs <- here::here("Week 38", png) # their directories
icons <- map(pngs, readPNG) %>% map(rasterGrob) %>% set_names(png) # set reference to list

#plotting
p <- ggplot(df_national_parks, mapping = aes(x = year, y = visitors_parks)) +
  # geom_step(color = "green4") +
  # geom_linerange(data = df_national_parks, aes(x = year, ymin = 0, ymax = visitors_parks), 
  #                color = 'green4') +
  geom_bar(stat = "identity", color = "green4", fill = NA) +
  ggimage::geom_image(data = mutate(df_national_parks, img = pngs[2]), 
                      aes(image = img), by = "height") +
  geom_text(aes(x = 1920, y = 900, label = year), size = 20, color = "lightskyblue3") +
  scale_y_continuous(breaks = c(0, 400, 800, 1200)) +
  #adding labels:
  labs(x = "", y = "", title = "Visitors in National Parks",
       subtitle = "Average per year (Thousands)", caption = "data from: data.world | AmitL")+
  
  #changing background for a nice clear sky
  theme(panel.background = element_rect(fill = 'lightskyblue2', color = 'lightblue', size = 0.5),
        # panel.grid.major = element_line(color = 'white', linetype = 'dashed'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(size = 0.1, linetype = "solid", colour = "black"), 
        axis.text.x = element_blank(), 
        axis.ticks = element_blank()) +

  #playing with some icons
  # annotation_custom(fig2, xmin=1975, xmax=1985, ymin=1100, ymax=1400) +
  annotation_custom(icons$cloud.png, xmin = 1975, xmax = 1985, ymin = 1100, ymax = 1400) + 
  # annotation_custom(fig2a, xmin=1925, xmax=1935, ymin=900, ymax=1200) + 
  annotation_custom(icons$cloud.png, xmin = 1925, xmax = 1935, ymin = 900, ymax = 1200) + 
  # annotation_custom(fig1, xmin=1899, xmax=1920, ymin=1200, ymax=1400)+ 
  annotation_custom(icons$sun.png, xmin = 1899, xmax = 1920, ymin = 1200, ymax = 1400)
  # annotation_custom(fig3, xmin=1915, xmax=1925, ymin=50, ymax=200)
  # annotation_custom(icons$stickman, xmin = 1915, xmax = 1925, ymin = 50, ymax = 200)

# ggsave("National Parks.png", width = 10, height = 5)

library(gganimate)
anim1 <- 
  p + transition_time(year) + 
  shadow_mark(exclude_layer = 2:3) + 
  ease_aes('bounce-in-out')

anim2 <- 
  p + transition_time(year) + 
  shadow_mark(future = TRUE, exclude_layer = 2:3) + 
  ease_aes('bounce-in-out')

anim_save(here::here("Week 38", "national_park.gif"), 
          animate(anim1, fps = 8, end_pause = 10))
anim_save(here::here("Week 38", "national_park_hiking.gif"), 
          animate(anim2, fps = 8, end_pause = 10))

