# get a local copy of the data for this week to save time on getCurl
# pkgs, functions, data ------
library(tidyverse)
library(patchwork)
library(ggforce)
library(ggridges)
library(magick)
library(cowplot)
library(htmltools)


df_epacars <- read_csv(here::here("static", "data", "2019042-epacars.csv"))


# data wrangling -------
co2up <- c("Mercedes-Benz", "Porsche")
co2down <- c("Dodge", "Mitsubishi")
co2bounce <- c("Ford", "Chevrolet")

df_epacars_co2 <- 
        df_epacars %>% 
        select(year, make, co2TailpipeGpm, VClass) %>% 
        filter(between(year, 1989, 2019)) %>% 
        mutate(VType = case_when(str_detect(VClass, "Cars") ~ "Sedan", 
                                 str_detect(VClass, "Wagons") ~ "Wagon", 
                                 str_detect(VClass, "Trucks") ~ "Truck", 
                                 str_detect(VClass, "Vehicle") ~ "SUV", 
                                 str_detect(VClass, "Vans") ~ "Van", 
                                 TRUE ~ "Others"), 
               color = case_when(make %in% co2up ~ "#D72F28", 
                                 make %in% co2down ~ "#110F0C",
                                 make %in% co2bounce ~ "#EECB3C",
                                 TRUE ~ "#F4F4F4")
        )

df_epacars_co2prop <- 
        df_epacars_co2 %>% 
        group_by(make) %>% 
        mutate(history = diff(range(year))) %>% 
        ungroup() %>% 
        filter(history == 30) %>% 
        distinct() %>% 
        group_by(year, make) %>% 
        mutate(co2_annual_by_make = sum(co2TailpipeGpm)) %>% 
        ungroup() %>% 
        group_by(year) %>% 
        mutate(co2_annual = sum(co2TailpipeGpm)) %>% 
        ungroup() %>% 
        mutate(prop = co2_annual_by_make / co2_annual)

# plotting trend & facet -----
make_plot <- function(.filter) {
        trend <- df_epacars_co2prop %>% 
                filter(make == .filter) %>% 
                ggplot(aes(year, prop, group = make)) + 
                geom_line(aes(color = I(color))) + 
                xlim(1987.5, 2020.5) +
                scale_y_continuous(breaks = function(x) c(min(x), max(x)), 
                                   labels = scales::percent) + 
                labs(x = "", y = "contrib.") +
                theme_classic() +
                theme(axis.text = element_text(size = 10))
        type <- df_epacars_co2 %>% 
                filter(make == .filter, between(year, 1989, 2019)) %>% 
                group_by(year, make, VType) %>% 
                count() %>% 
                ggplot(aes(x = year, y = n, color = VType, group = VType)) + 
                geom_line(show.legend = FALSE) + 
                scale_y_continuous(breaks = function(x) seq(0, max(x) %/% 10 * 10, length.out = 3)) +
                facet_wrap(~VType) + 
                theme_classic() + 
                labs(x = "", y = "") +
                theme(axis.text.x = element_blank(),
                      axis.line.x = element_blank(), 
                      axis.ticks.x = element_blank(), 
                      axis.text.y = element_text(size = 14))
        wrap_plots(trend, type, heights = c(0.1, 0.9), ncol = 1, byrow = FALSE)
}

# plotting ridges
ridge <- df_epacars_co2 %>% 
        ggplot(aes(co2TailpipeGpm, VType, col = VType)) + 
        geom_density_ridges(fill = "transparent", show.legend = FALSE, scale = 1) + 
        theme_classic()

ridge_data <- 
        ridge %>% 
        ggplot_build() %>% 
        pluck("data", 1) %>% 
        group_by(group) %>% 
        filter(density == max(density)) %>% 
        ungroup()

ridge_data$group <- levels(factor(df_epacars_co2$VType))

ridge_data <- left_join(df_epacars_co2, ridge_data, by = c("VType" = "group"))
ridge_data$markdescr <- "These cars are powered by electrcity"
ridge_anno_lbl <- latex2exp::TeX("$\\textbf{$\\overset{Most\\,Common\\,${CO_2}$}{level\\,in\\,the\\,group}$}")

ridge <- ridge_data %>% 
        ggplot(aes(co2TailpipeGpm, reorder(VType, x), fill = VType)) + 
        coord_flip() + 
        geom_density_ridges(alpha = .3, show.legend = FALSE, scale = 1) +
        geom_jitter(aes(col = VType), show.legend = FALSE, height = .1, width = .1, alpha = .1) +
        geom_mark_circle(aes(color = VType, filter = co2TailpipeGpm == 0 & VType == "Truck",
                             description = markdescr), fill = "transparent", show.legend = FALSE) +
        geom_curve(x = min(ridge_data$x), xend = min(ridge_data$x) - 20,
                   y = .95, yend = .8, curvature = 0,
                   arrow = arrow(length = unit(.07, "inches"), ends = "first", type = "closed")) +
        annotate("text", x = 310, y = .75, label = ridge_anno_lbl, size = 3.5) + 
        scale_y_discrete(expand = c(.1, .2)) +
        geom_point(aes(x), show.legend = FALSE, size = 1.5, col = "black") +
        geom_line(aes(x, group = 1), show.legend = FALSE) +
        theme_classic() +
        theme(axis.text = element_text(size = 14)) +
        labs(y = "", x = expression(Tailpipe~CO[2]~(g/Mile)))

# wrap them together -------
p <- wrap_plots(
        wrap_plots(map(c(co2down, co2up, co2bounce), make_plot), byrow = FALSE, ncol = 3), 
        ridge, ncol = 1, heights = c(.7, .3)
        )

# save the plots ----- 
ggsave(here::here("Week 42", "plot.png"), p, height = 32.9, width = 34.9, units = "cm")

# draw the saved plot -----
main <- image_read(here::here("Week 42", "plot.png")) %>% 
        image_ggplot()
title_richtext <- paste0(
        "**A story about ",
        span("C", style = 'color:black'), 
        span("O", style = 'color:red'), 
        "<span style = 'color:#EECB3C'><sub>2</sub></span>", 
        " from cars**"
)

passage1 <- "A consolidation of plots on the left consists three major parts: <br></br>
            the overall trend for annual CO<sub>2</sub> proportional contributions,<br></br> 
            above the faceted plots for productions by the following 6 brands,<br></br> 
            from top to bottom, **Dodge**, **Mitsubishi**, **<span style=\"color:#D72F28\">Mercedes-Benz</span>**, 
            **<span style=\"color:#D72F28\">Porsche</span>**<br></br>
            **<span style=\"color:#EECB3C\">Ford</span>**, and **<span style=\"color:#EECB3C\">Chevrolet</span>**.
            Additionally a ridge plot for displaying CO<sub>2</sub> <br></br>distribution by car types is at the bottom."

passage2 <- "Through the visualization, we know that the **downword** trend and <br></br>
            **<span style=\"color:#D72F28\">upward**</span> trend in CO<sub>2</sub> contribution 
            by these cars from the mentioned <br></br>manufacturers, are most likely linked to the 
            notable change in <br></br>model counts of sedan. Even sedan cars are ranked second last<br></br> 
            for tailpipe CO<sub>2</sub> emission, it surely is the biggest contributor by count."

passage3 <- "Another interesting finding here is trucks have a higher CO<sub>2</sub> <br></br>emission.
            As we can see from line plots on the right, **<span style=\"color:#EECB3C\">Ford</span>** 
            and <br></br>**<span style=\"color:#EECB3C\">Chevrolet</span>** cars' contributions to 
            CO<sub>2</sub> bounced up after the steady <br></br>decreases. Then a similar pattern in 
            the faceted plots for these two <br></br>brands of cars both showed an increased production
            in trucks after <br></br>2010. This particular finding conforms with ridge plot below:<br></br>
            trucks' CO<sub>2</sub> emission is second highest ranked by common levels, <br></br>right after vans."

caption <- "data: www.fueleconomy.gov | graphic: @chucc900"

# update plot with text ------ 
draw_richtext <- function (text, x = 0.5, y = 0.5, size = 14, hjust = 0.5, vjust = 0.5, ...) {
        ggtext::geom_richtext(data = data.frame(text, x, y), aes(x = x, y = y, label = text), 
                              size = (size/.pt), inherit.aes = FALSE, hjust = hjust, vjust = vjust, ...)
}

p <- ggdraw(main, xlim = c(0, 1.9), ylim = c(-0.01, 1.05)) + 
        draw_richtext(title_richtext, x = 1.35, y = .87, label.color = NA, family = "Optima", size = 22) +
        draw_richtext(text = passage1, x = 1.05, y = .73, label.color = NA, family = "Optima", hjust = 0) +
        draw_richtext(text = passage2, x = 1.05, y = .53, label.color = NA, family = "Optima", hjust = 0) +
        draw_richtext(text = passage3, x = 1.05, y = .30, label.color = NA, family = "Optima", hjust = 0) +
        draw_text(text = caption, x = .25, y = 0.05, size = 10, family = "Optima")

# the final outcome ------
ggsave(here::here("Week 42", "cars.png"), p, height = 21.3, width = 33.9, units = "cm")
