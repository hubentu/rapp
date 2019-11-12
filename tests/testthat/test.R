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

gendat <- function(n = 100, dist = c("normal", "uniform")){
    n <- as.integer(n)
    dist <- match.arg(dist)
    stopifnot(n < 1e+06)
    paste0(n, ":", dist)
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
    as.data.frame(rbind(summary(dat), summary(dat)))
}


ui <- card(title = "test", "min-width" = "800", class = "mx-auto",
           uiList=list(text_field(model="n", label="number"),
                       vselect(model = "dist", label = "distribution", change = "gentab"),
                       btn(onClick = "randomplot"),
                       data_table(table = "tsum"),
                       vimg(id = "plotOut", height = "600")))
index <- BuildUI(uid = "test", ui)
BuildApp(app = "testapp", ui = index,
         Rfun = list(gentab = gentab, randomplot = randomplot),
         outType = c("table", "plot"),
         outID = c("tsum", "plotOut"))


ui <- card(title = "test", "min-width" = "800", class = "mx-auto",
           uiList=list(text_field(model="n", label="number"),
                       vselect(model = "dist", label = "distribution",
                               change = c("gentab", "gendat")),
                       btn(onClick = "randomplot"),
                       div("{{textA[0]}}"),
                       data_table(table = "tsum"),
                       vimg(id = "plotOut", height = "600")))
index <- BuildUI(uid = "test", ui)
BuildApp(app = "testapp", ui = index,
         Rfun = list(gendat = gendat, gentab = gentab, randomplot = randomplot),
         outType = c("text", "table", "plot"),
         outID = c("textA", "tsum", "plotOut"))
