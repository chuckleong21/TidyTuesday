library(shiny.fluent)
library(shiny)
header <- tagList(
        tags$header(
                tags$img(src = "shiny/www/tidytuesday_logo.jpg"),
                Stack(
                        tokens = list(childrenGap = 5), 
                        Text(variant = "xLarge", "TidyTuesday"),
                        Text(variant = "xLarge", "Dashboard")
                )
        ), 
        tags$nav(
                tags$ul(
                        tags$li("Home"),
                        tags$li("About"),
                )
        )
)
htmltools::browsable(header)
