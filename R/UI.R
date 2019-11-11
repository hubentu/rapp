BuildUI <- function(uid = character(), ...){
    div(id = uid, vtags$v_app(list(...)))
}

card <- function(title = NULL, subtitle = NULL, text = NULL,
                 uiList = list(), uiPos = "top", ...){
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
    if(uiPos == "top") {
        tlist <- c(tlist, uiList)
    }else if(uiPos == "bottom"){
        tlist <- c(uiList, tlist)
    }
    vtags$v_card(list(
              vtags$v_container(tlist, props = c("fluid" = TRUE))
          ), props = c(...))
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
    vtags$v_select(props = c("v-model" = model, label = label, pp,
                             "v-on:change" = change, ...))
}

vimg <- function(src = character(), ...){
    vtags$v_img(props = c(src = src, ...))
}

btn <- function(onClick = "", label = "Submit", ...){
    vtags$v_btn(label, props = c("v-on:click.prevent" = onClick, ...))
}

data_table <- function(header = "header", dataframe, class = "elevation-1", ...){
    vtags$v_data_table(props = c(":headers" = header,
                                 ":items" = dataframe,
                                 "class" = class, ...))
}
