library(tidyverse)
library(VennDiagram)

df_cran <- tidytuesdayR::tt_load(2019, 46)$loc_cran_packages

subsetlang <- list(
        R = "R", 
        `Java/JavaScript` = c("Java", "JavaScript"), 
        `C/C++/C++ Header` = c("C", "C++", "C/C++ Header"),
        Python = "Python"
)

langlist <- map(subsetlang, ~{
        df_cran %>% 
                filter(language %in% .x) %>% 
                pull(pkg_name)
})

col <- jkmisc::tol4qualitative
v <- venn.diagram(
        x = langlist, 
        filename = NULL, 
        # circles
        lwd = 1, 
        col = col,
        fill = alpha(col, .3), 
        # numbers
        fontfamily = 'IBM Plex Serif', 
        # labels
        cat.cex = 1.4,
        cat.fontface = "bold",
        cat.default.pos = "outer", 
        cat.family = "IBM Plex Serif",
        cat.col = col
)

v[[10]]$label <- NULL
v[[11]]$label <- NULL
v[[15]]$label <- NULL
v[[16]]$label <- NULL
v[[21]]$label <- NULL
v[[22]]$label <- NULL
v[[14]]$label <- "httpuv"
v[[9]]$label <- "spam64\nRViennaCL"

draw(v)
