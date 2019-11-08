#' vuetify tags
#'
#' @import htmltools
#' @import rlang
vt_tags <- c("v-app",
             "v-alert",
             "v-responsive",
             "v-avatar",
             "v-badge",
             "v-banner",
             "v-app-bar",
             "v-toolbar",
             "v-system-bar",
             "v-bottom-navigation",
             "v-dialog",
             "v-breadcrumbs",
             "v-btn",
             "v-calendar",
             "v-container",
             "v-card",
             "v-card-title",
             "v-card-subtitle",
             "v-card-actions",
             "v-carousel",
             "v-chip",
             "v-divider",
             "v-expansion-panel",
             "v-footer",
             "v-btn-toggle",
             "v-chip-group",
             "v-item",
             "v-item-group",
             "v-list-item-group",
             "v-slide-group",
             "v-window",
             "v-hover",
             "v-icon",
             "v-img",
             "v-autocomplete",
             "v-combobox",
             "v-file-input",
             "v-form",
             "v-input",
             "v-overflow-btn",
             "v-select",
             "v-checkbox",
             "v-radio",
             "v-radio-group",
             "v-switch",
             "v-slider",
             "v-textarea",
             "v-text-field",
             "v-lazy",
             "v-list",
             "v-menu",
             "v-navigation-drawer",
             "v-overlay",
             "v-pagination",
             "v-parallax",
             "v-color-picker",
             "v-date-picker",
             "v-time-picker",
             "v-progress-circular",
             "v-progress-linear",
             "v-rating",
             "v-sheet",
             "v-snackbar",
             "v-stepper",
             "v-subheader",
             "v-data-iterator",
             "v-simple-table",
             "v-data-table",
             "v-tabs",
             "v-timeline",
             "v-tooltip",
             "v-treeview",
             "v-layout",
             "v-row",
             "v-col")
names(vt_tags) <- gsub("-", "_", vt_tags)

.valid.children <- function(object){
    ck <- unlist(lapply(object@children, function(x){
        any(class(x) %in% c("character", "shiny.tag", "vtag"))
    }))
    if(length(ck) > 0 && any(!ck)){
        return("All elements in the list should character, shiny.tag or vtag.")
    }
    NULL
}
.valid.props <- function(object){
    if(any(names(object@props) == "")){
        return("The props vector should be named.")
    }
    NULL
}
.valid.vtag <- function(object){
    c(.valid.children(object),
      .valid.props(object))
}

setClass("TAG", contains = c("list"))
## setOldClass("shiny.tag")
## setClassUnion("characterORTAGs", c("character", "TAG", "shiny.tag"))
setClassUnion("characterORlogicalORNULL", c("character", "logical", "NULL"))
setClassUnion("characterORlist", c("character", "list"))
setClass("vtag",
         contains = "TAG",
         slots = c(tagName = "character",
                   props = "characterORlogicalORNULL",
                   children = "characterORlist"),
         prototype = prototype(
             tagName = character(),
             props = character(),
             children = list()),
         validity = .valid.vtag)
#' @export
vtag <- function(tagName, children = list(), props = character()){
    new("vtag",
        tagName = tagName,
        props = props,
        children = children)
}

removeEmpty <- function(text){
    text <- gsub(" >", ">", text)
    text <- gsub("\n\n", "\n", text)
    text <- gsub(">(\\w+)[[:blank:]]+</", ">\\1</", text)
    text <- gsub("(\\})[[:blank:]]+</", "\\1</", text)
    text <- gsub(">[[:blank:]]+</", "></", text)
    text
}

#' @importFrom htmltools HTML
writeVtag <- function(object){
    pp <- object@props
    if(length(pp) > 0){
        ppl <- grepl("true|false", ignore.case = TRUE, pp)
        if(any(ppl)){
            pp <- pp[toupper(pp) != FALSE]
            idx <- toupper(pp) == TRUE
            if(sum(!idx) > 0){
                p1 <- paste0(names(pp[!idx]), "=", "\"", pp[!idx], "\"")
            }else{
                p1 <- NULL
            }
            props <- paste(c(p1,
                             names(pp)[idx]), collapse = " ")
        }else{
            props <- paste(paste0(names(pp), "=", "\"", pp, "\""),
                           collapse = " ")
        }
    }else{
        props <- NULL
    }
    indent <- "  "
    childL <- lapply(object@children, function(x){
        if(is(x, "character")){
            child <- x
        }else if(is(x, "shiny.tag")){
            child <- as.character(x)
        }else if(is(x, "vtag")){
            child <- writeVtag(x)
        }
        child <- gsub("<", paste0(indent, "<"), as.character(child))
        child <- gsub("\n(</.*>$)", paste0("\n", indent, "\\1"), as.character(child))
    })
    childs <- paste(unlist(childL), collapse = "\n")
    if(is(object@children, "character")){
        tagc <- paste0("<", object@tagName, " ", props, ">",
                       childs, "</", object@tagName, ">")
    }else{
        tagc <- paste0("<", object@tagName, " ", props, ">\n",
                       childs, "\n", "</", object@tagName, ">")
    }
    tagc <- removeEmpty(tagc)
    HTML(tagc)
}
   
setMethod(show, "vtag", function(object){
    print(writeVtag(object))
})

#' vtags
#' 
#' @export
vtags <- lapply(vt_tags, function(tagname) {
    function(...) {
        writeVtag(do.call(vtag, list(tagname, ...)))
    }
})
