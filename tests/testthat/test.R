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

ui <- div(id = "test-app",
          vtags$v_app(
                    list(vtags$v_card(
                                   list(vtags$v_container(
                                                  list(vtags$v_card_title("Title"),
                                                       vtags$v_text_field(props = c("v-model"="n",
                                                                                    "label"="Number")),
                                                       vtags$v_select(props = c("v-model"="dist",
                                                                                "label"="distribution",
                                                                                ":items"="distItems")),
                                                       vtags$v_card_actions("Click", props = c("v-on:click.prevent"="rdplot")),
                                                       vtags$v_img(props=c(id="plotdiv", height="600"))),    
                                              props = c(fluid=TRUE))),
                                   props = c("max-width"="800", "min-width"="600", "class"="mx-auto")
                               ))
                )
          )

BuildApp(app = "testapp", ui = ui, Rfun = list(rdplot = randomplot))
