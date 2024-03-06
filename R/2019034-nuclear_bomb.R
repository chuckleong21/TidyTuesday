# setup -----------------------------------------------------------------

library(tidyverse)
library(highcharter)

# scale_*_radioactive -----------------------------------------------------

#' Function to extract drsimonj colors as hex codes
#'
#' @param ... Character names of radioactive_colors 
#'
radioactive_cols <- function(...) {
        radioactive_colors <- c(`red`               = "#BC1A10", 
                                `orange`            = "#FFC901", 
                                `gorse`             = "#F7FF56", 
                                `charteuse`         = "#94FC13", 
                                `turqoise`          = "#4BE3AC", 
                                `caribbean green`   = "#00BDAA", 
                                `electric violet`   = "#951DED")
        cols <- c(...)
        
        if(is.null(cols)) 
                return(radioactive_colors)
        
        radioactive_colors[cols]
}


#' Return function to interpolate a radioactive color palette
#'
#' @param palette Character name of palette in radioactive_colors
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
radioactive_pal <- function(palette = "main", reverse = FALSE, ...) {
        
        radioactive_palettes <- list(
                `main`  = radioactive_cols("gorse", "turqoise", "electric violet"),
                `mixed` = radioactive_cols("gorse", "charteuse", "turqoise", 
                                           "caribbean green", "electric violet"),
                `hot`   = radioactive_cols("red", "orange")
        )
        pal <- radioactive_palettes[[palette]]
        
        if(reverse) pal <- rev(pal)
        
        colorRampPalette(pal, ...)
}

#' Color scale constructor for radioactive colors
#'
#' @param palette Character name of palette in radioactive_colors
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to \code{discrete_scale()} or
#'            \code{scale_color_gradientn()}, used respectively when discrete is
#'            TRUE or FALSE
#'
scale_color_radioactive <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
        pal <- radioactive_pal(palette = palette, reverse = reverse)
        
        if(discrete) {
                ggplot2::discrete_scale("color", paste0("radioactive_", palette), 
                                        palette = pal, ...)
        } else {
                ggplot2::scale_color_gradientn(colours = pal(256), ...)
        }
}

#' Fill scale constructor for radioactive colors
#'
#' @param palette Character name of palette in radioactive_colors
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'   scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'   
scale_fill_radioactive <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
        pal <- radioactive_pal(palette = palette, reverse = reverse)
        
        if(discrete) {
                ggplot2::discrete_scale("fill", paste0("radioactive_", palette), 
                                        palette = pal, ...)
        } else {
                ggplot2::scale_fill_gradientn(colours = pal(256), ...)
        }
}

# data --------------------------------------------------------------------


tt <- tidytuesdayR::tt_load(2019, 34)

nuclear_explosions <- tt$nuclear_explosions
nuclear_explosions <- nuclear_explosions %>% 
        filter(!is.na(purpose)) %>% 
        mutate(
                #! country color are coerced into alphabetical orders 
                country = case_when(
                        country == "CHINA"  ~ "China",
                        country == "FRANCE" ~ "France",
                        country == "INDIA"  ~ "India",
                        country == "PAKIST" ~ "Pakistan",
                        country == "UK"     ~ "United Kingdom",
                        country == "USA"    ~ "United States of America",
                        country == "USSR"   ~ "Soviet Union",
                        
                )
        )


# packed bubble -----------------------------------------------------------

packed_bubble_outer_col <- c("#4BE3AC", "#7BF345", "#19C9AA", "#3187C0", "#951DED", "#B5FD29", "#F7FF56")
hc_theme_radioactive <- hc_theme_merge(
        hc_theme_null(), 
        hc_theme(
                #! this change the colors of the outer bubbles
                colors = packed_bubble_outer_col, 
                chart = list(backgroundColor = "#000000"), 
                title = list(style = list(color = "white")), 
                subtitle = list(style = list(color = "white")), 
                legend = list(itemStyle = list(color = "grey"), itemHoverStyle = list(color = "white"))
        )
)

hc_packedbubble <- nuclear_explosions %>% 
        mutate(purpose = ifelse(str_detect(purpose, "WR"), "WR", purpose)) %>%  
        count(country, purpose) %>% 
        mutate(
                #! this only changes the colors of the inner bubbles
                color = case_when(
                        country == "China"                   ~ "#3cb589",
                        country == "France"                  ~ "#62c237",
                        country == "India"                   ~ "#19C9AA",
                        country == "Pakistan"                ~ "#276c99",
                        country == "Soviet Union"            ~ "#7717bd",
                        country == "United Kingdom"          ~ "#90ca20",
                        country == "United States of America"~ "#c5cc44"
                )
        ) %>%
        arrange(desc(purpose)) %>% 
        hchart(
                # declare chart type
                type = "packedbubble", 
                # aesthestic arguments take directly from that in highcharterJS
                hcaes(name = purpose, value = n, group = country, color = color)
        ) %>% 
        hc_plotOptions(
                packedbubble = list(
                        minSize = "20%",
                        maxsize = "100%",
                        zMin = 0, 
                        layoutAlgorithm = list(
                                gravitationalConstant = 0.05,
                                splitSeries =  TRUE, # split bubbles into groups
                                seriesInteraction = FALSE, # subItems cannot be dragged into other groups
                                dragBetweenSeries = FALSE, # SubItems are always back to its parent
                                parentNodeLimit = FALSE
                        ), 
                        dataLabels = list(
                                enabled = TRUE,
                                format = '{point.name}', 
                                # filter `purpose == 'WR'`
                                filter = list(property ='x', operator =  '===', value = 0),
                                style = list(color = 'black', textOutline = 'none', fontWeight = 'normal')
                        )
                        
                )
        ) %>% 
        hc_title(text = 'Distributions in Nuclear Explosions Across the Globe') %>% 
        hc_subtitle(text = 'Between 1945 and 1998, most of the nuclear explosions were for weapon development programmes') %>% 
        hc_tooltip(useHTML = TRUE, pointFormat = '{point.value} {point.name}') %>% 
        hc_add_theme(hc_theme_radioactive)

# htmlwidgets::saveWidget(hc_packedbubble, "plot/2019034.html")

# barplot -----------------------------------------------------------------


explosions_depth <- nuclear_explosions %>% 
        mutate(
                type = ifelse(grepl("SHAFT|MINE|U[G|W]|TUNNEL|GALLERY", type), "U", "A"), 
                purpose = fct_lump_min(purpose, min = 100)
        ) %>% 
        select(year, country, type) %>% 
        count(year, country, type) %>% 
        mutate(n = ifelse(type == "U", -n, n))

annotations <- tibble(x1 = c(1963, 1996, NA, NA), y1 = c(5, 0, NA, NA),
                      x2 = c(1963, 1996, NA, NA), y2 = c(110, 105, NA, NA),
                      x3 = c(1964, 1994.5, NA, NA), y3 = c(110, 105, NA, NA),
                      xtext = c(1968, 1989, 1946, 1946), ytext = c(104, 102, 115, -55),
                      text = c("**Partial Test Ban Treaty**<br>prohibiting tests in atmosphere,
                               <br>outer space, and underwater",
                               "**Comprehensive Test Ban Treaty**<br>prohibiting all tests",
                               "**Atmopsheric**", "**Underground**")
)

manual_pal <- set_names(
        packed_bubble_outer_col, 
        c("China", "France", "India", "Pakistan", "Soviet Union", "United Kingdom", "United States of America")
)
ggplot() + 
        geom_segment(data = annotations, aes(x = x1, y = y1, xend = x2, yend = y2), 
                     col = "white") +
        geom_segment(data = annotations, aes(x = x1, xend = x3, y = y3, yend = y3), 
                     col = "white") +
        ggtext::geom_richtext(data = annotations, aes(x = xtext, y = ytext, label = text), 
                      size = 5.5, fill = NA, color = "white", label.color = NA,
                      family = "Baskerville") +
        geom_bar(data = explosions_depth, aes(x = year, y = n, col = country), 
                 stat = "identity", fill = NA) +
        scale_color_manual(values = manual_pal) +
        scale_x_continuous("", breaks = seq(1945, 2000, 1), expand = c(0.03, 0)) +
        scale_y_continuous(labels = \(x) abs(x)) +
        guides(color = guide_legend(ncol = 2)) +
        labs(x = "", y = "", 
             title = str_to_title("the number of nuclear explosions"),
             subtitle = str_wrap("A re-design for statistical chart in SIPRI report (2000) tallying 
                                 the number of nuclear explosions conducted by countries 
                                 in the air and under ground.", 85), 
             caption = "data: Stockholm International Peace Research Institute|graph: @chucc900") +
        theme_minimal(base_size = 16, base_family = "Baskerville") +
        theme(panel.background = element_rect("#000000"), 
              plot.background = element_rect("#000000"),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.position = c(0.7, 0.68),
              legend.text = element_text(size = 20, margin = margin(0, 5, -1, 0)),
              legend.title = element_blank(),
              legend.key = element_rect(fill = "#000000"),
              legend.key.size = unit(.8, "cm"), 
              text = element_text(color = "white"), 
              axis.text = element_text(color = "white"),
              axis.text.x = element_text(angle = 90), 
              axis.line.y = element_line(color = "white"),
              plot.margin = margin(40, 40, 10, 10),
              plot.title = element_text(size = 30, color = radioactive_cols()["gorse"], 
                                        face = "bold"),
              plot.subtitle = element_text(size = 20)
        )


# TODO: nuclear explosions at geographical coordinates and magnitude
# nuclear calendar --------------------------------------------------------

calendar <- tibble(year = rep(1945:1998, each = 84), 
                   month = rep(1:12, 7*54),
                   country = rep(rep(unique(nuclear_explosions$country), each = 12), 54))
nuc_timeline <- nuclear_explosions %>% 
        select(date_long, country) %>% 
        mutate(date_long = lubridate::ymd(date_long), 
               year = lubridate::year(date_long),
               month = lubridate::month(date_long)
        ) 

calendars <- calendar %>% 
        left_join(nuc_timeline, by = c("year", "month", "country")) %>% 
        mutate(n = ifelse(!is.na(date_long), 1, date_long)) %>% 
        group_by(year, month, country) %>% 
        summarize(n = sum(n)) %>% 
        ungroup() %>% 
        filter(!country %in% c("Pakistan", "India"), !(year == 1945 & month < 7)) %>% 
        group_by(country) %>% 
        group_split()
calendars <- set_names(
        calendars, 
        c("China", "France", "Soviet Union", "United Kingdom", "United States of America")
)

# filter observations before the first nuclear explosion took place in each country except US
calendars$China <- calendars$China %>% 
        filter(year >= 1964) %>% 
        filter(!(year == 1964 & month < 10))
calendars$France <- calendars$France %>% 
        filter(year >= 1960) %>% 
        filter(!(year == 1960 & month == 1))
calendars$`Soviet Union` <- calendars$`Soviet Union` %>% 
        filter(year >= 1949) %>% 
        filter(!(year == 1949 & month < 8))
calendars$`United Kingdom` <- calendars$`United Kingdom` %>% 
        filter(year >= 1952) %>% 
        filter(!(year == 1952 & month < 10))
calendars$`United States of America` <- calendars$`United States of America` %>% 
        filter(!(year == 1945 & month < 7))
calendar <- map_df(calendars, rbind) %>% arrange(year) %>% filter(!year > 1996)

annodat <- function(direction = "right", year, month, stretch, 
                    group, country) {
        
        x1 <- year - .5; x2 <- year + .5
        y1 <- month - .5; y2 <- month + .5
        x <- c(x1, x2, x2, x1, x1, x1)
        y <- switch(direction, 
                    "left" = c(y1, y1, y2, y2, y1, stretch),
                    "right" = c(y2, y2, y1, y1, y2, stretch)
        )
        
        d <- tibble(x, y, group = group, country = country)
        d
}

china_tests <- list(direction = c("right", "left", "left", "right", "left", "right"), 
                    year = c(1964, 1967, 1974, 1976, 1980, 1996), 
                    month = c(10, 6, 6, 11, 10, 7), 
                    stretch = c(13, -1, -1, 13, -1, 13),
                    group = 1:6,
                    country = "China"
)
france_tests <- list(direction = c("left", "right", "right", rep("left", 3)),
                     year = c(1960, 1961, 1966, 1968, 1975, 1996),
                     month = c(2, 11, 7, 8, 6, 1),
                     stretch = c(-1, 13, 13, -1, -1, -1),
                     group = 1:6,
                     country = "France"
)
ussr_tests <- list(direction = c(rep("right", 4), "left", "right"),
                   year = c(1949, 1953, 1955, 1961, 1965, 1990), 
                   month = c(8, 8, 11, 10, 1, 10), 
                   stretch = c(13, 13, 13, 13, -2, 13),
                   group = 1:6,
                   country = "Soviet Union"
)
uk_tests <- list(direction = c("left", "right", "left", "left", "right"), 
                 year = c(1952, 1953, 1956, 1958, 1991),
                 month = c(10, 10, 5, NA, 11),
                 stretch = c(8, 13, -1, -1, 13),
                 group = 1:5,
                 country = "United Kingdom"
)
usa_tests <- list(direction = c("right", "right", "left", "right", 
                                "left", "left", "left", "right"), 
                  year = c(1945, 1946, 1951, 1952, 1954, 1957, 1962, 1992), 
                  month = c(7, 7, 5, 11, 3, NA, 5, 9),
                  stretch = c(5, 13, -1, 13, -2, -2, -1, 13),
                  group = 8:1, 
                  country = "United States of America"
)

annodata <- list(china_tests, france_tests, ussr_tests, uk_tests, usa_tests) %>% 
        map(pmap, annodat) %>% reduce(c) %>% map_df(rbind)

addendum <- annodata %>% 
        group_by(country, group) %>% 
        group_split() %>%
        .[c(22, 26)] %>% 
        set_names(paste("Operation", c("Grapple", "Plumbob")))

addendum$`Operation Grapple` <- addendum$`Operation Grapple` %>% 
        mutate(x = c(1956, 1958, 1958, 1956, 1956, 1956) + .5,
               y = c(c(12, 12, 0, 0, 12) + .5, 13))
addendum$`Operation Plumbob` <- addendum$`Operation Plumbob` %>% 
        mutate(y = c(c(10, 10, 4, 4, 10) + 0.5, -2))
addendum <- addendum %>% tibble() %>% unnest()

annodata <- annodata %>% 
        group_by(country, group) %>% 
        group_split() %>% 
        .[-c(22, 26)] %>% 
        tibble() %>% 
        unnest() %>% 
        bind_rows(addendum)

annotext <- annodata %>% 
        group_by(country, group) %>% 
        slice(6) %>% 
        ungroup() %>% 
        mutate(text = as.character(1:31),
               y = ifelse(text %in% c("19", "31"), y - .5, ifelse(y > 0, y + .5, y - .5))) %>% 
        select(x, y, group, country, text)

(p <- ggplot() +
                geom_tile(data = filter(calendar, !year > 1996), aes(year, month, fill = n), col = "white") + 
                geom_path(data = annodata, aes(x, y, group = group, color = country), size = 1) + 
                geom_text(data = annotext, aes(x, y, label = text, color = country), size = 5) +   
                scale_fill_gradientn(colours = radioactive_pal("hot")(5), breaks = c(1, 5, 8, 15, 20)) +
                guides(color = "none", 
                       fill = guide_legend(title = "Tests/Explosions", reverse = TRUE, 
                                           direction = "horizontal", title.position = "top",
                                           keyheight = 1)) +
                scale_color_manual(values = manual_pal) +
                scale_x_reverse(breaks = c(1996:1945)) +
                scale_y_continuous(breaks = 1:12, labels = month.abb,
                                   limits = c(-4, 15), expand = c(-0.05, -.2)) + 
                coord_flip() +
                facet_wrap(~country, scales = "free_y") + 
                labs(x = "", y = "", 
                     title = "The Nuclear Tests Calendar", 
                     subtitle = "Below is the calendar chart during the period between\nthe first and the last test in each following nuclear state:\nChina, France, Soviet Union, United Kingdom and United States.", 
                     caption = "Data: Stockholm Internation Peace Research Institute | Wikipedia\nGraph: @chucc900") +
                theme_minimal(base_size = 16, base_family = "Baskerville") +
                theme(text = element_text(color = "#FFFFFF"),
                      plot.background = element_rect(fill = "#000000"),
                      plot.title = element_text(size = 40, color = "#4BE3AC", face = "bold"),
                      plot.subtitle = element_text(size = 30, color = "#FFFFFF"),
                      plot.caption = element_text(size = 30),
                      plot.margin = margin(20, 20, 10, 10),
                      panel.background = element_rect(fill = "#000000"),
                      axis.text.y = element_text(color = "#FFFFFF"),
                      axis.text.x = element_text(angle = 90, color = "#FFFFFF", size = 20),
                      axis.ticks = element_blank(),
                      strip.text = element_text(color = "#FFFFFF", size = 20),
                      strip.background = element_blank(),
                      panel.grid = element_blank(),
                      legend.position = c(.85, 1.1), 
                      legend.background = element_blank(),
                      legend.text = element_text(color = "#FFFFFF", size = 30),
                      legend.title = element_text(color = "#FFFFFF", face = "bold", size = 30))
)



# ggsave(here::here("plot", "2019034-002.png"), p, width = 20, height = 20)
