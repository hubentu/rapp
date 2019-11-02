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


buildUI <- function(uid = character(), ...){
    div(id = uid, vtags$v_app(list(...)))
}

card <- function(title = NULL, subtitle = NULL, text = NULL, uiList = list(), ...){
    tlist <- list()
    if(!is.null(title)){
        tlist <- c(tlist, vtags$v_card_title(title))
    }
    if(!is.null(subtitle)){
        tlist <- c(tlist, vtags$v_card_subtitle(subtitle))
    }
    if(!is.null(text)){
        tlist <- c(tlist, vtags$v_text(text))
    }
    tlist <- c(tlist, uiList)
    vtags$v_card(list(
              vtags$v_container(tlist, props = c("fluid" = TRUE))
          ), props = c(...))
}


text_field <- function(model = character(), label = NULL, ...){
    vtags$v_text_field(props = c("v-model" = model, label = label, ...))
}

vselect <- function(model = character(), label = NULL, ...){
    if(length(model) > 0){
        pp <- c(":items" = paste0(model, "Items"))
    }else{
        pp <- character()
    }
    vtags$v_select(props = c("v-model" = model, label = label, pp, ...))
}

btn <- function(onClick = "", label = "Submit", ...){
    vtags$v_btn(label, props = c("v-on:click.prevent" = onClick, ...))
}

ui <- card(title = "test",
           uiList=list(text_field(model="n", label="number"),
                       vselect(model = "dist", label = "distribution"),
                       btn(onClick = "randomplot")))
index <- buildUI(uid = "test", ui)
BuildApp(app = "testapp", ui = index, Rfun = list(randomplot = randomplot))
