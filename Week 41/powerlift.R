library(tidyverse)
library(ggbeeswarm)
library(ggtext)
library(grid)
library(cowplot)

df_lifts_raw <- read_csv(here::here("Week 41", "ipf_lifts.csv"))

take_median <- function(x) {
        map_dbl(strsplit(x, "-"), ~median(as.numeric(.x)))
}

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

df_lifts_inc %>% 
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

ggsave(here::here("Week 41", "ipf_ageclass.png"), width = 30.9, height = 20, units = "cm")

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
                benches, squats and deadlifts. Athletes equipped with single-plys are likely to lift 
                heavier weights than those compete with bare hands."

p <- ggplot() +
        geom_quasirandom(data = df_lifts_beeswarm, 
                         aes(age_class, max_weight_kg, color = sex),
                         alpha = .5, size = 2) + 
        geom_smooth(data = df_lifts_smooth, 
                    aes(x = age_class, y = max_wt_kg, color = sex, group = sex),
                    method = "loess", se = F) + 
        scale_color_manual(
                name = "", values = c("#AB82FF", "#4876FF"), 
                labels = c("Women", "Men"),
                guide = guide_legend(reverse = TRUE)
                ) + 
        scale_x_continuous(
                breaks = seq_len(length(xlbl)),
                labels = xlbl, 
                limits = c(.5, 15.5)
                ) +
        labs(
                x = "Age Class", y = "Maximum Weights Lifted", 
                title = 
        ) +
        facet_grid(perform ~ equipment, switch = "y") + 
        theme_minimal(base_size = 16, base_family = "Impact") + 
        theme(
                strip.placement = "outside", 
                strip.text.y = element_text(size = 12), 
                legend.position = c(.9, .65), 
                legend.text = element_text(size = 14), 
                axis.text.x = element_text(size = 12, angle = 30)
              )

legend_txt <- "The smooth lines fitted maximum weights \never lifted merely for displaying trends"
legend_caption <- textGrob(legend_txt, hjust = 0, gp = gpar(fontsize = 7.5, fontfamily = "Impact"))

ggdraw(p) + 
        draw_grob(legend_caption, x = .24, y = .08, hjust = -0.1) + 
        ggsave(here::here("Week 41", "ipf_age_effect.png"), 
               height = 20, width = 30.9, units = "cm")
