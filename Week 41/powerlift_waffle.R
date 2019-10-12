library(tidyverse)
library(janitor)
library(waffle)
library(ggtext)

df_lifts_raw <- read_csv(here::here("Week 41", "ipf_lifts.csv"))

df_lifts_waffle <- 
        df_lifts_raw %>% 
        mutate(year = lubridate::year(date)) %>% 
        tabyl(year, sex) %>% 
        adorn_percentages() %>% 
        gather(sex, n, 2:3) %>% 
        mutate(n = round(n * 100))

# sprintf("%03d") pads enough zeros for numbers so they are strings of length 3
waffle_png <- here::here("Week 41", paste0("waffle_", sprintf("%03d", seq_len(t)), ".png"))

title_waffle <- "Women are taking part in powerlifting"
sub_waffle <- "<span style='color:#AB82FF'>Women</span> first participated powerlifting 
                        competitions in 1980, <br></br>before then it was <span style='color:#4876FF'>men</span> 
                        dominated the scene. More females <br></br>took part in powerlifting events 
                        ever since. In 2019 42% lifters<br></br>are females, the proportion increases 
                       more than 10% since 1980."

# waffle charts
waffles <- 
        map(unique(df_lifts_waffle$year), ~{
                waffle <- 
                        df_lifts_waffle %>% 
                        filter(year == .x) %>% 
                        ggplot(aes(fill = sex, values = n)) + 
                        geom_waffle(color = "white", size = .25, n_rows = 10) + 
                        geom_text(aes(label = year), x = 5.5, y = 5.5, size = 40, 
                                  color = "#4169E1", family = "Impact") + 
                        scale_x_discrete() + 
                        scale_y_continuous(labels = function(x) x *10) + 
                        scale_fill_manual(name = "", values = c("mediumpurple1", "royalblue1")) +
                        theme_minimal(base_size = 16, base_family = "Impact") +
                        labs(
                                x = "1 square = 1%", y = "",
                                title = title_waffle, 
                                subtitle = sub_waffle, 
                                caption = "Data: Open Powerlifting | Graphic: @chucc900"
                        ) + 
                        theme(
                                plot.margin = margin(20, 20, 20, 20),
                                plot.title = element_markdown(size = 28),
                                plot.subtitle = element_markdown(size = 24),
                                panel.grid = element_blank(), 
                                axis.text.y = element_blank(), 
                                legend.position = "None", 
                                legend.key.size = unit(30, "pt")
                        )
                if(.x == 1979) {
                        waffle + annotate("text", x = 5.5, y = 2.5, size = 12, family = "Impact",
                                          label = "IPF added women's competition")
                } else waffle
})

# save them. map2() returns a list of NULL
walk2(waffle_png, waffles, ggsave, width = 25, height = 15, units = "cm")


setwd(here::here("Week 41"))
system("convert -delay 40 waffle_*.png ipf_waffle.gif")
invisible(file.remove(list.files()[grepl("waffle_.*\\.png", list.files())]))
