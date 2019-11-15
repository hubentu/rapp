BuildUI <- function(uid = character(), ...){
    div(id = uid, vtags$v_app(list(...)))
}

card <- function(title = NULL, subtitle = NULL, text = NULL,
                 uiList = list(), uiPos = "top", class = "mx-auto", ...){
    tlist <- list()
    if(!is.null(title)){
        tlist <- c(tlist, vtags$v_card_title(title))
    }
    if(!is.null(subtitle)){
        tlist <- c(tlist, vtags$v_card_subtitle(subtitle))
    }
    if(!is.null(text)){
        tlist <- c(tlist, vtags$v_card_text(text))
    }
    if(uiPos == "top") {
        tlist <- c(tlist, uiList)
    }else if(uiPos == "bottom"){
        tlist <- c(uiList, tlist)
    }
    vtags$v_card(list(
              vtags$v_container(tlist, props = c("fluid" = TRUE))
          ), props = c(class = class, ...))
}


text_field <- function(model = character(), label = NULL, ...){
    vtags$v_text_field(props = c("v-model" = model, label = label, ...))
}

vselect <- function(model = character(), label = NULL, change = NULL, ...){    
    if(length(model) > 0){
        pp <- c(":items" = paste0(model, "Items"))
    }else{
        pp <- character()
    }
    if(length(change) > 0){
        names(change) <- rep("@change", length(change))
    }
    vtags$v_select(props = c("v-model" = model, label = label, pp,
                             change, ...))
}

vimg <- function(src = character(), ...){
    vtags$v_img(props = c(src = src, ...))
}

btn <- function(onClick = NULL, label = "Submit", ...){
    if(length(onClick) > 0){
        names(onClick) <- rep("@click.prevent", length(onClick))
    }
    vtags$v_btn(label, props = c(onClick, ...))
}

data_table <- function(table, class = "elevation-1", items_per_page = 5, ...){
    vtags$v_data_table(props = c(":headers" = paste0(table, "headers"),
                                 ":items" = table,
                                 ":items-per-page" = items_per_page,
                                 "class" = class, ...))
}

iframe <- function(id = NULL, src = "about:blank",
                   height = "600", width = "100%", frameborder = "0", ...){
    vtag("iframe", props = c(id = id, src = src, height = height,
                             width = width, frameborder = frameborder, ...))
}

