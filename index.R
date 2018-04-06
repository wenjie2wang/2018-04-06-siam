## ----setup, echo = FALSE-------------------------------------------------
options(width = 100)

### other simple utility functions
## create a tag with some handy defaults
a_new <- function(x, href = x, target = "_blank", ...)
{
    htmltools::tags$a(x, href = href, target = target, ...)
}
## paste with forward slash
paste4 <- function(...) paste(..., sep = "/")

### define some constants
rstudio_github <- "https://github.com/rstudio"
shiny_homepage <- "https://shiny.rstudio.com"
shiny_examples_github <- paste4(rstudio_github, "shiny-examples")
shiny_gallery <- paste4(shiny_homepage, "gallery")
shiny_tutorial <- paste4(shiny_homepage, "tutorial")
shiny_cheatsheet <- paste4(rstudio_github, "cheatsheets/blob/master/shiny.pdf")

## ----pkgs-slides, eval = FALSE-------------------------------------------
## install.packages(
##     c("bookdown", "data.table", "dplyr", "DT", "dygraphs", "htmltools",
##       "leaflet", "microbenchmark", "plotly", "R6", "readr", "shiny")
## )

## ----need-packages, echo = FALSE-----------------------------------------
##' Check, Install and Attach Multiple R packages Specified
##'
##' The function first Checks whether the packages given were installed. Then
##' install them if they are not, then attach them to the search path.
##'
##' @usage need.packages(pkg)
##' @param pkg A character vector specifying the packages needed to reproduce
##'     this document.
##' @param ... Other arguments passed to function \code{\link[base]require}.
##' @return NULL invisibly.
##' @examples
##' need.pacakges(c("ggplot2", "geepack"))
need.packages <- function(pkg, ...)
{
    new.pkg <- pkg[! (pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, repos = "https://cloud.r-project.org")
    foo <- function(a, ...) suppressMessages(require(a, ...))
    sapply(pkg, foo, character.only = TRUE)
    invisible(NULL)
}
need.packages(
    c("bookdown", "data.table", "dplyr", "DT", "dygraphs", "htmltools",
      "leaflet", "microbenchmark", "plotly", "R6", "readr", "shiny")
)

## ----session-info--------------------------------------------------------
sessionInfo()

## ----get-nrows-function--------------------------------------------------
get_nrows <- function(file) {
    as.integer(
        system2("wc", sprintf("-l %s | awk '{print $1}'", file), TRUE)
    )
}

## ----simu-large-data, cache = TRUE---------------------------------------
options(stringsAsFactors = FALSE)
set.seed(613); n <- 1e6
dat <- data.frame(foo = rnorm(n),
                  bar = rpois(n, 5),
                  alpha = sample(letters, size = n, replace = TRUE),
                  beta = as.Date(rpois(n, 10), origin = "2018-04-06"),
                  gamma = gl(5, k = n / 5, labels = LETTERS[1:5]))
str(dat)
write.table(dat, file = "data.csv", sep = ",", row.names = FALSE)
sprintf("File size: %.0f MB", file.info("data.csv")$size / 1024 ^ 2)

## ----simple-read-table, eval = FALSE-------------------------------------
## ## simply use `read.csv`
## dat1 <- read.csv("data.csv")
## ## or `read.table`
## dat2 <- read.table("data.csv", sep = ",", header = TRUE)
## ## or specify `nrows` and `colClasses`
## dat3 <- read.table("data.csv", sep = ",", header = TRUE,
##                    nrows = get_nrows("data.csv") - 1,
##                    colClasses = c("numeric", "integer",
##                                   rep("character", 3)))
## ## use readr::read_csv
## dat4 <- readr::read_csv("data.csv")
## dat5 <- readr::read_csv("data.csv", col_types = c("diccc"),
##                         n_max = get_nrows("data.csv") - 1)
## ## use data.table::fread
## dat6 <- data.table::fread("data.csv")
## dat7 <- data.table::fread("data.csv", header = TRUE, sep = ",",
##                           nrows = get_nrows("data.csv") - 1)
## ## exercise:
## ##   check the resulting column types of each `data.frame` object.
## ##   is there any difference? what are the classes of `dat4`,...,`dat7`?

## ----read-table-benchmark, cache = TRUE----------------------------------
library(microbenchmark)
microbenchmark(
    read.csv = read.csv("data.csv"),
    read.table = read.table("data.csv", sep = ",", header = TRUE),
    read.table_tricks = read.table("data.csv", sep = ",", header = TRUE,
                                   nrows = get_nrows("data.csv") - 1L,
                                   colClasses = c("numeric", "integer",
                                                  rep("character", 3))),
    read_csv = readr::read_csv("data.csv", col_types = cols()),
    read_csv_tricks = readr::read_csv("data.csv", col_types = c("diccc"),
                                      n_max = get_nrows("data.csv") - 1),
    fread = data.table::fread("data.csv"),
    fread_tricks = data.table::fread("data.csv", header = TRUE, sep = ",",
                                     nrows = get_nrows("data.csv") - 1,
                                     colClasses = c("numeric", "integer",
                                                    rep("character", 3))),
    times = 30, unit = "relative")

## ----dplyr-examples-select-1, cache = TRUE-------------------------------
names(dat)
str(select(dat, foo, alpha))
str(select(dat, bar:beta))
str(select(dat, -(bar:beta)))

## ----dplyr-examples-select-2, cache = TRUE-------------------------------
str(select(dat, starts_with("b")))
str(select(dat, ends_with("a")))
str(select(dat, -matches("^b|a$")))

## ----dplyr-examples-filter-1, cache = TRUE-------------------------------
str(filter(dat, foo > 2 & bar %in% c(3, 4)))
## str(filter(dat, foo > 2, bar %in% c(3, 4))) # equivalent

## ----dplyr-examples-filter-2, eval = FALSE-------------------------------
## ## why not use `[.data.frame`?
## subDat <- dat[dat$foo > 2 & dat$bar %in% c(3, 4), ]
## ## not a fan of `$` operator? okay! no `$` if using `with`, right?
## subDat <- with(dat, dat[foo > 2 & bar %in% c(3, 4), ])
## ## how about using `base::subset`?
## subDat <- base::subset(dat, foo > 2 & bar %in% c(3, 4))

## ----benchmark-subset, cache = TRUE--------------------------------------
dat_tbl <- tbl_df(dat)
dat_dt <- as.data.table(dat)
microbenchmark(
    "[_$" = dat[dat$foo > 2 & dat$bar %in% c(3, 4), ],
    "[_with" = with(dat, dat[foo > 2 & bar %in% c(3, 4), ]),
    "subset" = base::subset(dat, foo > 2 & bar %in% c(3, 4)),
    "filter_&" = dplyr::filter(dat, foo > 2 & bar %in% c(3, 4)),
    "filter_," = dplyr::filter(dat, foo > 2, bar %in% c(3, 4)),
    "filter_tbl" = dplyr::filter(dat_tbl, foo > 2, bar %in% c(3, 4)),
    "data.table" = dat_dt[foo > 2 & bar %in% c(3, 4)],
    times = 200, unit = "relative"
)

## ----dplyr-examples-arrange, cache = TRUE--------------------------------
asc_dat <- arrange(dat, beta, foo)
head(select(asc_dat, beta, foo), 3)
tail(select(asc_dat, beta, foo), 3)
desc_dat <- arrange(dat, desc(beta), foo)
head(select(desc_dat, beta, foo), 3)

## ----benchmark-order, cache = TRUE---------------------------------------
microbenchmark(
    "order_$" = dat[order(as.numeric(dat$beta), dat$foo,
                          decreasing = c(TRUE, FALSE)), ],
    "order_with" = with(dat, dat[order(as.numeric(beta), foo,
                                       decreasing = c(TRUE, FALSE)), ]),
    "arrange" = arrange(dat, desc(beta), foo),
    "arrange_as" = arrange(dat, desc(as.numeric(beta)), foo),
    "data.table" = dat_dt[order(- beta, foo), ],
    times = 100, unit = "relative"
)

## ----dplyr-examples-rename-1, cache = TRUE-------------------------------
names(rename(dat, x = foo, y = bar))

## ----dplyr-examples-rename-2, eval = FALSE-------------------------------
## names(dat)[names(dat) == "foo"] <- "x"
## names(dat)[names(dat) == "bar"] <- "y"

## ----data-table-examples-setnames, eval = FALSE--------------------------
## setnames(dat_dt, c("foo", "bar"), c("x", "y"))

## ----dplyr-examples-mutate-1, cache = TRUE-------------------------------
str(mutate(dat, bar_centered = bar - mean(bar)))

## ----dplyr-examples-mutate-2, eval = FALSE-------------------------------
## dat$bar_centered <- dat$bar - mean(dat$bar)
## ## or using `with`
## dat$bar_centered <- with(dat, bar - mean(bar))

## ----data-table-examples-newCol, eval = FALSE----------------------------
## dat_dt[, `:=`(bar_centered = bar - mean(bar))]

## ----dplyr-examples-group-by, cache = TRUE-------------------------------
dat_grouped <- group_by(dat, gamma)
summarise(dat_grouped, sd_foo = sd(foo), mean_bar = mean(bar))

## ----benchmark-pipe-1, cache = TRUE--------------------------------------
microbenchmark(
    nested = summary(head(dat, n = 100)),
    steps = {
        tmpDat <- head(dat, n = 100)
        summary(tmpDat)
    },
    pipe = dat %>% head(n = 100) %>% summary,
    times = 1e3, unit = "relative"
)

## ----benchmark-pipe-2, cache = TRUE--------------------------------------
microbenchmark(
    nested = prop.table(xtabs(~ bar, head(dat, n = 100))),
    steps = {
        tmpDat <- head(dat, n = 100)
        tmpTab <- xtabs(~ bar, tmpDat)
        prop.table(tmpTab)
    },
    pipe = dat %>% head(n = 100) %>% xtabs(~ bar, .) %>% prop.table,
    times = 1e3, unit = "relative"
)

## ----dplyr-example-all, cache = TRUE-------------------------------------
dat %>%
    select(- alpha) %>%
    rename(group = gamma) %>%
    mutate(abs_foo = abs(foo)) %>%
    group_by(group) %>%
    arrange(beta, desc(bar)) %>%
    head(n = 200) %>%
    summarize(median_abs_foo = median(abs_foo))

## ----sidebar-layout------------------------------------------------------
ui <- fluidPage(
    titlePanel("my title panel"),
    sidebarLayout(
        sidebarPanel("my sidebar panel"),
        mainPanel("my main panel")
    )
)

## ----sidebar-layout-preview, echo = FALSE, results = "asis"--------------
ui

## ----sidebar-layout-html, echo = FALSE, comment = "", class.output = "sourceCode html"----
print(ui)

## ----mtcars-app, echo = FALSE--------------------------------------------
knitr::include_app("https://shiny.wenjie-stat.me/examples/mtcars/",
                   height = "600px")

## ----leaflet-app, echo = FALSE-------------------------------------------
knitr::include_app("https://shiny.wenjie-stat.me/examples/leaflet/",
                   height = "500px")

## ----dygraphs-app, echo = FALSE------------------------------------------
knitr::include_app("https://shiny.wenjie-stat.me/examples/dygraphs/",
                   height = "500px")

## ----plotly-app, echo = FALSE--------------------------------------------
knitr::include_app("https://shiny.wenjie-stat.me/examples/plotly/",
                   height = "600px")

## ----dt-app, echo = FALSE------------------------------------------------
knitr::include_app("https://shiny.wenjie-stat.me/examples/DT/",
                   height = "600px")

