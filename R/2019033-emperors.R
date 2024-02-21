
# library --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tidytuesdayR)
library(ggalluvial)
library(ggrepel)

theme_plex <- function(base_size = 11,
                       strip_text_size = 12,
                       strip_text_margin = 5,
                       subtitle_size = 13,
                       subtitle_margin = 10,
                       plot_title_size = 16,
                       plot_title_margin = 10,
                       ...) {
        ret <- ggplot2::theme_minimal(base_family = "IBMPlexSans",
                                      base_size = base_size, ...)
        ret$strip.text <- ggplot2::element_text(
                hjust = 0, size = strip_text_size,
                margin = ggplot2::margin(b = strip_text_margin),
                family = "IBMPlexSans-Medium"
        )
        ret$plot.subtitle <- ggplot2::element_text(
                hjust = 0, size = subtitle_size,
                margin = ggplot2::margin(b = subtitle_margin),
                family = "IBMPlexSans"
        )
        ret$plot.title <- ggplot2::element_text(
                hjust = 0, size = plot_title_size,
                margin = ggplot2::margin(b = plot_title_margin),
                family = "IBMPlexSans-Bold"
        )
        ret
}

theme_set(theme_plex())


# load --------------------------------------------------------------------

tt <- tt_load(2019, 33)
emperors <- tt$emperors


# wrangle -----------------------------------------------------------------

before_bc <- c("Augustus", "Tiberius", "Claudius", "Galba")

emperors_years <- emperors %>% 
        mutate_if(is.Date, list(year = year)) %>% 
        mutate_at(vars(reign_start_year), 
                  ~ifelse(name == "Augustus", -(.), .)) %>% 
        mutate(birth_year = ifelse(name %in% before_bc, -birth_year, birth_year))
emperors_missing_years <- emperors_years %>% 
        filter(is.na(birth)) %>% 
        mutate(birth_year = case_when(name == "Florian" ~ 232,
                                      name == "Numerian" ~ 254, 
                                      name == "Carinus" ~ 257,
                                      name == "Severus II" ~ 250,
                                      name == "Vetranio" ~ 325))

plot_data <- emperors_years %>% 
        filter(!is.na(birth)) %>% 
        bind_rows(emperors_missing_years) %>% 
        select(name, dynasty, contains("year"))

# annotations and arrows

curve_data <- tibble(curve_x = c(-40, 430), 
                     curve_xend = c(-26, 418), 
                     curve_y = c(3, 64), 
                     curve_yend = c(1, 67))


# plot --------------------------------------------------------------------

my_palette <- colorRampPalette(c("#F5F646", "#F95E7F"))(8) %>% 
        set_names(unique(plot_data$dynasty))
plot_title <- str_to_title("A tip of the iceberg of the Roman Emperors")
plot_subtitle <- str_wrap(
        "This graph is reproduced from the work by Jake Kaupp.", 100)
plot_cation <- "Data: Wikipedia via @geokaramanis | Reference from @jkaupp"

timeline <- ggplot(plot_data, aes(y = 0)) +
        geom_segment(aes(x = reign_start_year, 
                         xend = reign_end_year, 
                         yend = 0, color = dynasty), 
                     size = 4) +
        scale_x_continuous(breaks = c(-62, 0, 100, 200, 300, 400), 
                           labels = c("62BC", "AD", "100AD", "200AD", "300AD", "400AD")) +
        expand_limits(x = c(-62, 450)) +
        scale_color_manual("Dynasty", values = my_palette, breaks = names(my_palette)) +
        labs(x = NULL, 
             y = NULL, 
             caption = plot_cation) +
        theme_classic() +
        theme(axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank(),
              panel.background = element_blank(),
              legend.position = "none")

# segment bars
main <- ggplot(plot_data, aes(y = reorder(name, reign_start_year))) +
        geom_segment(aes(x = birth_year, xend = death_year, yend = name), 
                     size = 2, color = "grey90") +
        geom_segment(aes(x = reign_start_year, xend = reign_end_year, y = name, 
                         yend = name, color = dynasty), size = 2) +
        geom_text(aes(x = death_year, label = name), 
                  hjust = 0, size = 2, nudge_x = 2.5) +
        geom_curve(data = curve_data, 
                   aes(x = curve_x, xend = curve_xend, 
                       y = curve_y, yend = curve_yend), color = "grey25",
                   arrow = arrow(length = unit(0.07, "inch")), curvature = .7) +
        annotate("text", x = -40, y = 3.8, label = "Longest in reign of 40 years", size = 2.4) +
        annotate("text", x = 428, y = 63.6, label = "The youngest emperor", size = 2.4) +
        scale_color_manual("Dynasty", values = my_palette, 
                           breaks = names(my_palette)) +
        scale_x_continuous(breaks = c(-62, 0, 100, 200, 300, 400), 
                           labels = c("62BC", "AD", "100AD", "200AD", "300AD", "400AD")) +
        expand_limits(x = c(-62, 450)) +
        labs(x = NULL, y = NULL,
             title = plot_title,
             subtitle = plot_subtitle) +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major = element_blank(),
              legend.position = c(.8, 0.5),
              legend.background = element_rect(fill = "white", colour = "white")
        )

patchwork::wrap_plots(main, timeline, ncol = 1, heights = c(0.9, 0.1)) %>% 
        ggsave(filename = "2019033-001.png", plot = ., device = "png", path = "plot")


