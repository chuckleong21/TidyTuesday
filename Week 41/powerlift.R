# this script requires extra fonts ImpactaLL
# pkgs & data ----------
library(tidyverse)
library(janitor)
library(waffle)
library(ggbeeswarm)
library(ggtext)
library(grid)
library(cowplot)

df_lifts_raw <- read_csv(here::here("Week 41", "ipf_lifts.csv"))

# waffle -----
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
                        over the decade in 2010s. <br></br>By 2019 42% lifters are females, the proportion increases 
                       <br></br>more than 10% since 1980."

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

# line -----
df_lifts_inc <- 
        df_lifts_raw %>% 
        select(date, sex, age_class) %>% 
        filter(age_class != "5-12") %>% 
        drop_na() %>% 
        mutate(year = lubridate::year(date)) %>% 
        count(year, sex, age_class) %>% 
        mutate(n_lag1 = lag(n), 
               n_lag1 = coalesce(n_lag1, n), 
               incr = n_lag1 - n) %>% 
        ungroup() %>% 
        group_by(sex, age_class) %>% 
        mutate(avgincr = mean(incr))

title_line <- "Less younger competeors over the years"
sub_line <- 
        "Most powerlifters competed their first match around the age between 17 and 22. <br></br>However, 
         over the years, the number of competors in younger age class has <br></br>been decreasing after
         80s for <span style='color:#4876FF'>men</span> and 90s for <span style='color:#AB82FF'>women</span>.
        More participants <br></br>compete around the age 30s as the graph shows."

lines <- df_lifts_inc %>% 
        ggplot(aes(year, incr, color = sex)) + 
        geom_line(size = 1.2) + 
        geom_hline(aes(yintercept = avgincr, color = sex), linetype = 2) + 
        scale_color_manual(name = "", values = c("#AB82FF", "#4876FF")) +
        facet_wrap(~age_class, nrow = 5) + 
        theme_minimal(base_family = "Impact", base_size = 16) + 
        theme(plot.title = element_text(size = 28), 
              plot.subtitle = element_markdown(size = 24), 
              plot.margin = margin(20, 20, 20, 20),
              legend.position = "none", 
              axis.text.y = element_text(size = 10), 
              axis.title.y = element_text(size = 13), 
              axis.text.x = element_text(size = 11)) + 
        labs(
                x = "", y = "Increase in participants", 
                title = title_line, 
                subtitle = sub_line, 
                caption = "Data: Open Powerlifting | Graphic: @chucc900"
             )

ggsave(here::here("Week 41", "ipf_ageclass.png"), lines,
       width = 30.9, height = 20, units = "cm")

# beeswarm ------
df_lifts_filter <- 
        df_lifts_raw %>% 
        filter(event != "SB", 
               age > 10, 
               age_class != "5-12",
               !place %in% c("DQ", "DD"), 
               equipment != "Wraps") %>% 
        mutate(age = ifelse(is.na(age) && !is.na(age_class), 
                            take_median(age_class), age))

df_lifts_beeswarm <- 
        df_lifts_filter %>% 
        select(sex, age_class, contains("kg"), equipment, date) %>% 
        gather(perform, max_weight_kg, best3squat_kg:best3deadlift_kg) %>% 
        mutate(perform = str_extract(perform, "squat|bench|deadlift")) %>% 
        drop_na() %>% 
        mutate(age_class = as.numeric(factor(age_class)))

df_lifts_smooth <- 
        df_lifts_beeswarm %>% 
        group_by(sex, age_class, equipment, perform) %>% 
        summarize(max_wt_kg = max(max_weight_kg)) %>% 
        ungroup() %>% 
        mutate(age_class = as.numeric(factor(age_class)))

xlbl <- df_lifts_filter %>% 
        drop_na() %>% 
        pull(age_class) %>% 
        unique() %>% 
        sort()

title_beeswarm <- "Gear up for better performance!"
sub_beeswarm <- "The following graph shows how age and equipment affect atheletes' performances on 
benches, squats and deadlifts. Athletes equipped with single-plys are likely to 
lift heavier weights than those compete with bare hands. This advantage persists 
for older athletes, even though their sport performance slowly decreases."

beeswarms <- 
        ggplot() +
        geom_quasirandom(data = df_lifts_beeswarm, 
                         aes(age_class, max_weight_kg, color = sex),
                         alpha = .5, size = 2) + 
        geom_smooth(data = df_lifts_smooth, 
                    aes(x = age_class, y = max_wt_kg, color = sex, group = sex),
                    method = "loess", se = F) + 
        scale_color_manual(
                name = "", values = c("#AB82FF", "#4876FF"), 
                labels = c("Women", "Men"),
                guide = guide_legend(reverse = TRUE, direction = "horizontal")
                ) + 
        scale_x_continuous(
                breaks = seq_len(length(xlbl)),
                labels = xlbl, 
                limits = c(.5, 15.5)
                ) +
        labs(
                x = "Age Class", y = "Maximum Weights Lifted", 
                title = title_beeswarm, 
                subtitle = sub_beeswarm, 
                caption = "Data: Open Powerlifting | Graphic: @chucc900"
        ) +
        facet_grid(perform ~ equipment, switch = "y") + 
        theme_minimal(base_size = 16, base_family = "Impact") + 
        theme(
                plot.margin = margin(20, 20, 20, 20), 
                plot.title = element_text(size = 28), 
                plot.subtitle = element_text(size = 24),
                strip.placement = "outside", 
                strip.text.y = element_text(size = 12), 
                legend.position = c(.9, .65), 
                legend.text = element_text(size = 14), 
                axis.text.x = element_text(size = 12, angle = 30)
              )

# add caption under legend
legend_txt <- "The smooth lines fitted maximum weights \never lifted merely for displaying trends"
# legend_caption <- textGrob(legend_txt, hjust = 0, gp = gpar(fontsize = 9, fontfamily = "ImpactaLL"))
legend_caption <- textGrob(legend_txt, hjust = 0, x = .65, y = .73,
                           gp = gpar(fontsize = 9, fontfamily = "ImpactaLL"))

# YazidKurdi's approach for footnote of legend
annotation_custom2 <- 
        function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
        {
                layer(data = data, stat = StatIdentity, position = PositionIdentity, 
                      geom = ggplot2:::GeomCustomAnn,
                      inherit.aes = TRUE, params = list(grob = grob, 
                                                        xmin = xmin, xmax = xmax, 
                                                        ymin = ymin, ymax = ymax))
        }

p <- beeswarms + annotation_custom2(data = df_lifts_beeswarm %>%
                               filter(equipment == "Single-ply", perform == "deadlift"), 
                       grob = legend_caption)

ggsave(here::here("Week 41", "ipf_age_effect.png"), p,
       height = 20.9, width = 33.4, units = "cm")

# the cowplot approach | not as flexible as annotation_custom2()
# p <- ggdraw(beeswarms) + 
#         draw_grob(legend_caption, hjust = -.325, vjust = .03) 
