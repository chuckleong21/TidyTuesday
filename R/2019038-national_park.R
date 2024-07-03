library(tidyverse)
library(png)
library(gridGraphics)

tt <- tidytuesdayR::tt_load(2019, 38)
df_park_visits <- tt$national_parks


# cleaning the data
df_national_parks <- # try to add prefixes for data.frame so look them up quickly in Rstudio
  df_park_visits %>% 
  filter(year != "Total") %>% 
  mutate_at(vars(year, visitors), as.integer) %>% 
  mutate(unit_type = as.factor(unit_type)) %>% 
  filter(unit_type == "National Park") %>% 
  group_by(year) %>% 
  summarise(visitors_parks = mean(visitors)) %>% 
  mutate(visitors_parks = visitors_parks/1000)

png_path <- path.chain::full_path_chain()
pngs <- png_path$static$image
icons <- map(c(pngs$cloud.png, pngs$stickman.png, pngs$sun.png), readPNG) %>% 
        map(rasterGrob) %>% 
        set_names(c("cloud.png", "stickman.png", "sun.png")) # set reference to list

#plotting
p <- ggplot(df_national_parks, mapping = aes(x = year, y = visitors_parks)) +
  geom_bar(stat = "identity", color = "green4", fill = NA) +
  ggimage::geom_image(data = mutate(df_national_parks, img = here::here("static/image", png[2])), 
                      aes(image = img), by = "height") +
  geom_text(aes(x = 1920, y = 900, label = year), size = 20, color = "lightskyblue3") +
  scale_y_continuous(breaks = c(0, 400, 800, 1200)) +
  #adding labels:
  labs(x = "", y = "", title = "Visitors in National Parks",
       subtitle = "Average per year (Thousands)", caption = "data from: data.world | AmitL")+
  
  #changing background for a nice clear sky
  theme(panel.background = element_rect(fill = 'lightskyblue2', color = 'lightblue', linewidth = 0.5),
        # panel.grid.major = element_line(color = 'white', linetype = 'dashed'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(linewidth = 0.1, linetype = "solid", colour = "black"), 
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

anim <- 
  p + transition_time(year) + 
  shadow_mark(past = TRUE, exclude_layer = 2:3) + 
  ease_aes('bounce-in-out')


anim_save(here::here("plot", "2019038001.gif"), 
          animate(anim, fps = 8, end_pause = 10))

