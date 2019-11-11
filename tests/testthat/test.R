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
    as.data.frame(rbind(summary(dat)))
}


ui <- card(title = "test", "min-width" = "800", class = "mx-auto",
           uiList=list(text_field(model="n", label="number"),
                       vselect(model = "dist", label = "distribution", change = "gentab"),
                       btn(onClick = "randomplot"),
                       div("{{ textA }}"),
                       vimg(id = "plotOut", height = "600")))
index <- BuildUI(uid = "test", ui)
BuildApp(app = "testapp", ui = index,
         Rfun = list(gentab = gentab, randomplot = randomplot),
         outType = c("text", "plot"),
         outID = c("textA", "plotOut"))
