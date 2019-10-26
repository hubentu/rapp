#' Build a web APP
#'
#' @import devtools
#' @importFrom usethis create_package
#' @include vtags.R
BuildApp <- function(app, ui, Rfun, dir = tempdir()){
    path <- file.path(dir, app)
    if(!dir.exists(path)){
        message("creating package ", app)
        oldwd <- setwd(dir)
        testthat::with_mock(
                      `usethis:::check_not_nested` =
                          function(path, name) NULL,
                      create_package(app, rstudio = FALSE,
                                     open = FALSE)
                  )
        on.exit(setwd(oldwd), add = TRUE)
    }else{
        message("package ", app, " existing!")
    }
    
    ## Rfun
    if(is.character(Rfun)){
        stopifnot(file.exists(Rfun))
        invisible(file.copy(Rfun, file.path(path, "R")))
    }else if(is(Rfun, "list")){
        for(i in seq(Rfun)){
            ##browser()
            Rfun1 <- deparse(Rfun[[i]])
            Rfun1[1] <- paste(names(Rfun)[i], "<-", Rfun1[1])
            Rfun1 <- c(paste("#'", names(Rfun)[i]),
                      "#' @export", Rfun1)
            writeLines(Rfun1,
                       file.path(path, "R", paste0(names(Rfun)[i], ".R")))
        }
    }
    document(path)
    
    ## UI
    libdir <- file.path(path, "inst", "www", "lib")
    dir.create(libdir, recursive = TRUE, showWarnings = FALSE)
    vjs <- vueJS(ui, Rfun)
    writeLines(vjs, file.path(libdir, "app.js"))
    UI <- renderUI(ui, vuejs = "lib/app.js", opencpu = TRUE)
    
    save_html(UI,
              file = file.path(dirname(libdir),
                               paste0(deparse(substitute(ui)), ".html")))
    message("Package created: ", path)
    return(path)
}

## lst <- list(InputText("n", "count"),
##             InputSelect("dist", "Distribution", list(normal = "normal", uniform = "uniform")),
##             Button(id = "submit", "submit"))

#' Build web UI
#'
#' @param inputPanel input menu tag list.
#' @param outputPanel output tag list.
#' @importFrom jsonlite toJSON
#' @export
renderUI <- function(ui, vuejs = "lib/app.js", opencpu = FALSE){
    deps <- list(
        htmlDependency("vue", "2.6.10",
                       c(href = "https://cdn.jsdelivr.net/npm/vue@2.6.10/dist"),
                       script = "vue.js"),
        htmlDependency("vuetify", "2.0.2",
                       c(href = "https://cdn.jsdelivr.net/npm/vuetify@2.0.2/dist"),
                       script = "vuetify.min.js",
                       stylesheet = "vuetify.min.css"))

    if(opencpu){
        odeps <- list(
            htmlDependency("jquery", "1.11.1",
                           c(href = "https://code.jquery.com"),
                           script = "jquery-1.11.1.min.js"),
            htmlDependency("opencpu", "0.4",
                           c(href = "https://cdn.opencpu.org"),
                           script = "opencpu-0.4.js"))
        deps <- c(deps, odeps)
        ## ui <- tagList(ui, tags$head(singleton(tags$script(src="lib/ocpu.js"))))
    }
    uid <- ui$attribs$id
    tagList(deps,
            tags$div(id = paste0(uid, "App"), tag(tolower(paste0(uid, "app")), "")),
            tags$script(type = "text/x-template", id = uid, ui),
            tags$script(src = vuejs))
}


#' vue js
vueJS <- function(ui, Rfun, outType = "plot", actionID = "Submit", outID = "plotdiv", dataList = list(), methodList = list()){
    tmplID <- ui$attribs$id
    appID <- paste0(tmplID, "App")
    args <- lapply(formals(Rfun[[1]]),
                   function(x){
                       if(is.call(x)){
                           as.character(x)[-1]
                       }else{
                           as.character(x)
                       }
                   })

    for(i in seq(args)){
        if(length(args[[i]]) > 1){
            dat1 <- list("", as.list(args[[i]]))
            names(dat1) <- c(names(args)[i],
                             paste0(names(args)[i], "Items"))
        }else{
            dat1 <- list(args[[i]])
            names(dat1) <- names(args)[i]
        }
        dataList <- c(dataList, dat1)
    }
    
    ## data
    Dat <- paste0("data() {return ", toJSON(dataList, auto_unbox = T), "}")
    ## methods
    ##Rfun <- randomplot
    ##args <- formals(Rfun)
    argL <- as.list(paste0("this.", names(args)))
    names(argL) <- names(args)
    Args <- gsub("\"", "", toJSON(argL, auto_unbox = T))
    
    if(outType == "plot"){
        rMeth <- paste0(actionID, ': function () {var req = $("#', outID, '").rplot("', names(Rfun)[1], '", ', Args, ');}')
        methodList <- c(rMeth, methodList)
    }
    methodList <- paste(unlist(methodList), collapse = ",")
    methodList <- paste0('methods: {', methodList, '}')
    
    tmplJS <- paste0('Vue.component("', tolower(appID), '", {template: "#', tmplID, '", ', Dat, ' ,', methodList, '}); new Vue({el: "#', appID, '", vuetify: new Vuetify()});')
    HTML(tmplJS)
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
                                                       vtags$v_card_actions("Click", props = c("v-on:click.prevent"="Submit")),
                                                       vtags$v_img(props=c(id="plotdiv", height="600"))),    
                                              props = c(fluid=TRUE))),
                                   props = c("max-width"="800", "min-width"="600", "class"="mx-auto")
                               ))
                )
          )
