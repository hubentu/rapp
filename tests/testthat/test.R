randomplot <- function (n, dist = c("normal", "uniform")) 
{
    n <- as.integer(n)
    dist <- match.arg(dist)
    stopifnot(n < 1e+06)
    if (dist == "normal") {
        hist(rnorm(n))
    }
    if (dist == "uniform") {
        hist(runif(n))
    }
}

genstr <- function(n = 100, dist = c("normal", "uniform")){
    n <- as.integer(n)
    dist <- match.arg(dist)
    stopifnot(n < 1e+06)
    paste0(n, ":", dist)
}

## gentab <- function(n = 100, dist = c("normal", "uniform")){
##     n <- as.integer(n)
##     dist <- match.arg(dist)
##     stopifnot(n < 1e+06)
##     if (dist == "normal") {
##         dat <- rnorm(n)
##     }else if (dist == "uniform") {
##         dat <- runif(n)
##     }
##     as.data.frame(rbind(summary(dat), summary(dat)))
## }
genTab <- function(n = 100, dist = c("normal", "uniform")){
    n <- as.integer(n)
    dist <- match.arg(dist)
    stopifnot(n < 1e+06)
    if (dist == "normal") {
        dat <- rnorm(n)
    }else if (dist == "uniform") {
        dat <- runif(n)
    }
    as.data.frame(rbind(summary(dat)))
}

gentab <- function(n = 100, dist = c("normal", "uniform")){
    n <- as.integer(n)
    dist <- match.arg(dist)
    stopifnot(n < 1e+06)
    if (dist == "normal") {
        dat <- rnorm(n)
    }else if (dist == "uniform") {
        dat <- runif(n)
    }
    list(tdat = dat,
         tsum = as.data.frame(rbind(summary(dat))))
}

gendat <- function(n = 100, dist = c("normal", "uniform")) {
    ## genstr <- function(n = 100, dist = c("normal", "uniform")){
    ##     n <- as.integer(n)
    ##     dist <- match.arg(dist)
    ##     paste0(n, ":", dist)
    ## }

    ## gentab <- function(n = 100, dist = c("normal", "uniform")){
    ##     n <- as.integer(n)
    ##     dist <- match.arg(dist)
    ##     stopifnot(n < 1e+06)
    ##     if (dist == "normal") {
    ##         dat <- rnorm(n)
    ##     }else if (dist == "uniform") {
    ##         dat <- runif(n)
    ##     }
    ##     list(tdat = dat,
    ##         tsum = as.data.frame(rbind(summary(dat))))
    ## }
    dat <- gentab(n, dist)
    list(Str = genstr(n, dist),
         Dat = dat$tdat,
         Tsum = dat$tsum)
}

library(ggplot2)
datplot <- function(rdat){
    rdat <- data.frame(rdat = rdat)
    ggplot(rdat, aes(x = rdat)) + geom_histogram()
    ## hist(rdat)
}

ui <- card(title = "test", "min-width" = "800", class = "mx-auto",
           uiList=list(text_field(model="n", label="number"),
                       vselect(model = "dist", label = "distribution", change = "genTab"),
                       btn(onClick = "randomplot"),
                       data_table(table = "tsum"),
                       vimg(id = "plotOut", height = "600")))
##index <- BuildUI(uid = "test", ui)
BuildApp(app = "testapp", ui = ui,
         Rfun = list(genTab = genTab, randomplot = randomplot),
         outType = list("table", "plot"),
         outID = list("tsum", "plotOut"))

##
ui <- card(title = "test", "min-width" = "800", class = "mx-auto",
           uiList=list(text_field(model="n", label="number"),
                       vselect(model = "dist", label = "distribution",
                               change = "genstr", "v-on:change" = "gentab"),
                       btn(onClick = "randomplot"),
                       div("{{textA[0]}}"),
                       data_table(table = "tsum"),
                       vimg(id = "plotOut", height = "600")))
##index <- BuildUI(uid = "test", ui)
BuildApp(app = "testapp", ui = ui,
         Rfun = list(genstr = genstr, gentab = gentab, randomplot = randomplot),
         outType = list("text", "table", "plot"),
         outID = list("textA", "tsum", "plotOut"))

##
ui <- card(title = "Test App", text = "{{textA[0]}}",
           "min-width" = "800",
           uiList=list(text_field(model="n", label="number"),
                       vselect(model = "dist", label = "distribution",
                               change = "gendat"),
                       data_table(table = "tsum"),
                       br(),
                       btn(onClick = "datplot"),
                       br(),
                       vimg(id = "plotOut", height = "600")))
##index <- BuildUI(uid = "test", ui)
BuildApp(app = "testapp", ui,
         Rfun = list(gendat = gendat, datplot = datplot),
         outType = list(list("text", "text", "table"), "plot"),
         outID = list(list("textA", "rdat", "tsum"), "plotOut"))

## plotly
library(ggplot2)
library(plotly)
library(htmlwidgets)
##library(widgetframe)
ggp <- function(rdat){
    rdat <- data.frame(rdat = rdat)
    p <- ggplot(rdat, aes(x = rdat)) + geom_histogram()
    ggplotly(p)
}
ggpsave <- function(...){
    gg <- ggp(...)
    saveWidget(gg, file = "index.html", selfcontained = FALSE)
}
ui <- card(title = "Test App", text = "{{textA[0]}}",
           "min-width" = "800",
           uiList=list(text_field(model="n", label="number"),
                       vselect(model = "dist", label = "distribution",
                               change = "gendat"),
                       data_table(table = "tsum"),
                       br(),
                       btn(onClick = "ggp"),
                       br(),
                       iframe(id = "plotOut")))
## index <- BuildUI(uid = "test", ui)
BuildApp(app = "testapp", ui = ui,
         Rfun = list(gendat = gendat, ggp = ggp),
         outType = list(list("text", "text", "table"), "html"),
         outID = list(list("textA", "rdat", "tsum"), "plotOut"))

## v-html not work
library(htmltools)
ggh <- function(rdat){
    rdat <- data.frame(rdat = rdat)
    p <- ggplot(rdat, aes(x = rdat)) + geom_histogram()
    gg <- ggplotly(p)
    renderTags(gg)$html
}

ui <- card(title = "Test App", text = "{{textA[0]}}",
           "min-width" = "800",
           uiList=list(text_field(model="n", label="number"),
                       vselect(model = "dist", label = "distribution",
                               change = "gendat"),
                       data_table(table = "tsum"),
                       br(),
                       btn(onClick = "ggh"),
                       br(),
                       vtags$v_card(list(div("v-html"="plotOut")))
                       ))
## index <- BuildUI(uid = "test", ui)
BuildApp(app = "testapp", ui = ui,
         Rfun = list(gendat = gendat, ggh = ggh),
         outType = list(list("text", "text", "table"), "text"),
         outID = list(list("textA", "rdat", "tsum"), "plotOut"))

## next
