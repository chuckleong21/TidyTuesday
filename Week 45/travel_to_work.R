# load data & pkgs ------
library(tidyverse)
library(ggtext)
source(here::here("tidy_tuesday.R"))

df_commute_demg <- tidy_tuesday("table_3.csv")

# commuters' demographic ------
df_commute_demg %<>% 
        slice(-c(7:8, 11:12, 19:20, 25:26, 37:38)) %>% 
        mutate(
                indicator = c(rep("age", 6), rep("sex", 2), 
                              rep("race", 6), rep("children", 4), 
                              rep("income", 10), rep("education", 5)), 
        ) %>% 
        rowid_to_column(var = "id") %>% 
        rename(category = "age") %>% 
        select(indicator, everything(), -other_percent) %>% 
        pivot_longer(cols = contains("percent"), names_to = "mode", 
                     names_pattern = "(.*)_",
                     values_to = "percent") %>% 
        mutate_at(vars(total_workers, percent), as.numeric)

# reorder colors: use ggsci::pal_d3()
df_commute_demg_grouped <- group_by(df_commute_demg, indicator)
color_order <- 
        df_commute_demg_grouped %>% 
        group_indices() %>% 
        unique()
group_name <- 
        df_commute_demg_grouped %>% 
        group_keys %>% 
        reduce(c) %>% 
        .[color_order]        
color_d3 <- ggsci::pal_d3()(length(color_order))
names(color_d3) <- group_name
color_mode <- c("walk" = "#252627", "bike" = "#cdcdcd")

# prepare labels
# https://www.r-graph-gallery.com/296-add-labels-to-circular-barplot.html
df_commute_demg_lbls <- 
        df_commute_demg %>% 
        group_by(category) %>% 
        mutate(lbly = sum(percent)) %>% 
        ungroup() %>% 
        slice(seq(2, nrow(df_commute_demg), 2)) %>% 
        mutate(angle = 90 - 360 * (id - .5) / n(), 
               hjust = ifelse(angle < -90, 1, 0), 
               angle = ifelse(angle < -90, angle + 180, angle))
nrep <- group_by(df_commute_demg_lbls, indicator) %>% 
        group_size() %>% 
        .[color_order]
txtcolor <- rep(color_d3, nrep) # color labels
 
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
caption <- "data: ACS Survey | graphic: Chuck Leong"

(plot_demg <- df_commute_demg %>% 
        ggplot(aes(as.factor(id), percent, group = indicator, color = mode,
                   fill = indicator)) + 
        geom_col() + 
        scale_y_continuous(limits = c(-1, 12), 
                           labels = function(x) paste0(x, "%")) + 
        scale_color_manual(values = color_mode) + 
        scale_fill_manual(values = color_d3) + 
        coord_polar(start = 0) + 
        guides(fill = "none", color = "none") + 
        theme_minimal(base_family = "IBM Plex Serif") + 
        theme(
                legend.position = c(.75, .75),
                legend.key.size = unit(.25, "inches"),
                legend.text = element_text(size = 10),
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
             caption = caption)  +
        geom_text(data = df_commute_demg_lbls, 
                  aes(x = id, y = lbly + .5, 
                      label = category, hjust = hjust, angle = angle), 
                  color = I(txtcolor), size = 3, alpha = .6, 
                  family = "IBM Plex Serif", fontface = "bold", 
                  inherit.aes = FALSE)
)

# save image via Save Image As... via Rstudio