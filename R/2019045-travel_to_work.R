library(tidyverse)
library(ggtext)

tt <- tidytuesdayR::tt_load(2019, 45)
indicator <- c(rep("age", 6), rep("sex", 2), 
               rep("race", 6), rep("children", 4), 
               rep("income", 10), rep("education", 5))

df_commute_demo <- read_csv(here::here("static", "data", "2019045-table_3.csv")) %>% 
        slice(-c(7:8, 11:12, 19:20, 25:26, 37:38)) %>% 
        mutate(indicator = indicator) %>% 
        rowid_to_column(var = "id") %>% 
        rename(category = "age") %>% 
        select(-other_percent) %>% 
        pivot_longer(cols = contains("percent"), names_to = "mode", 
                     names_pattern = "(.*)_",
                     values_to = "percent") %>% 
        mutate_at(vars(total_workers, percent), as.numeric) %>% 
        mutate(mode_clr = ifelse(mode == "walk", "#252627", "#cdcdcd")) 

# prepare labels: https://www.r-graph-gallery.com/296-add-labels-to-circular-barplot.html

color_d3 <- ggsci::pal_d3()(6) # color scheme
names(color_d3) <- unique(indicator)
# color_d3 <- color_d3[c(1, 4, 6, 5, 3, 2)]
demo_lbls <- df_commute_demo %>% 
        group_by(category) %>% 
        mutate(lbly = sum(percent)) %>% 
        ungroup() %>% 
        distinct(id, lbly) %>% 
        # slice(seq(2, nrow(df_commute_demo), 2)) %>% 
        mutate(angle = 90 - 360 * (id - .5) / n(), 
               hjust = ifelse(angle < -90, 1, 0), 
               angle = ifelse(angle < -90, angle + 180, angle), 
               txtcolor = rep(color_d3, c(6, 2, 6, 4, 10, 5)))

df_commute_demo <- df_commute_demo %>% 
        left_join(demo_lbls, by = 'id')

subtitle_span <- imap_chr(color_d3, 
                          ~glue::glue("<span style='color:{.x}'>{.y}</span>"))
subtitle_span <- paste(paste(subtitle_span[1:5], collapse = ", "), 
                       "and", subtitle_span[6], collapse = ", ")

title_main <- "Traveling to work: Walking vs Bycicling"
subtitle <- paste0("U.S. Census Bureau conducted a survey on people 
                   walking/bycicling to work between 2008 and 2012.<br></br>
                   The survey asked about 6 demographic indicators: ", 
                   subtitle_span, ".<br></br>",
                   "The graph below shows that **young people** are preferred 
                   to <span style='color:#252627'>walk(outer)</span>/
                   <span style='color:#cdcdcd'>bycicle(inner)</span> to work.
                   <br></br>", "This fact is reflected into education and 
                   income level based on the assumptions for the young.")
caption <- "data: ACS Survey | graphic: chuckleong21@"

(plot_demg <- df_commute_demo %>% 
                group_by(indicator) %>% 
                mutate(x = fct_reorder(as.factor(id), lbly)) %>% 
                ggplot(aes(x = id, 
                           y = percent, 
                           group = indicator, 
                           color = I(mode_clr), 
                           fill = indicator)) + 
                geom_col() + 
                geom_text(aes(x = id, y = lbly + .5, color = txtcolor,
                              label = category, hjust = hjust, angle = angle), 
                          size = 3, alpha = .6, 
                          fontface = "bold", 
                          inherit.aes = FALSE) +
                scale_y_continuous(limits = c(-1, 12), 
                                   labels = function(x) paste0(x, "%"), 
                                   expand = c(0, 0)) + 
                scale_fill_manual(values = color_d3) + 
                coord_polar(start = 0) + 
                guides(fill = "none", color = "none") + 
                theme_minimal(base_family = "xkcd Script") + 
                theme(
                        panel.grid = element_blank(),
                        axis.text = element_blank(), 
                        axis.title = element_blank(), 
                        plot.title = element_text(size = 24, 
                                                  margin = margin(0, 0, 15, 0)),
                        plot.subtitle = element_markdown(size = 14, lineheight = 1.2), 
                        plot.caption = element_text(size = 12)
                ) +
                labs(x = "", y = "",
                     title = title_main, 
                     subtitle = subtitle, 
                     caption = caption)
)

