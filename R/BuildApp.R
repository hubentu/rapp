#' Build a web APP
#'
#' @import devtools
#' @importFrom usethis create_package
#' @include vtags.R
BuildApp <- function(app, ui, Rfun, dir = tempdir(), outType = "plot", outID = "plotOut", dataList = list(), methodList = list()){
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
        if(sum(names(Rfun)!="") < length(Rfun))
            stop("Rfun should be a named list")
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
    vjs <- vueJS(ui, Rfun, outType, outID, dataList, methodList)
    writeLines(vjs, file.path(libdir, "app.js"))
    UI <- renderUI(ui, vuejs = "lib/app.js", opencpu = TRUE, outType, outID)
    
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
renderUI <- function(ui, vuejs = "lib/app.js", opencpu = FALSE, outType = "plot", outID = "plotOut"){
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
    vbind <- c()
    for(i in seq(outType)){
        if(outType[i] == "text"){
            ##vbind[i] <- paste0('@', outID[i], 'text-gen="', outID[i], 'textgen"')
            vbind[i] <- paste0(tolower(outID[i]), "textgen")
            names(vbind)[i] <- paste0("@", tolower(outID[i]), "text-gen")
        }
    }
    atag <- tag(tolower(paste0(uid, "app")),
                na.omit(c(vbind, ref = paste0(uid, "Ref"))))
    
    tagList(deps,
            tags$div(id = paste0(uid, "App"), atag),
            tags$script(type = "text/x-template", id = uid, ui),
            tags$script(src = vuejs))
}


#' vue js
vueJS <- function(ui, Rfun, outType = "plot", outID = "plotOut", dataList = list(), methodList = list()){
    tmplID <- ui$attribs$id
    appID <- paste0(tmplID, "App")

    args <- c()
    rMeths <- c()
    vMeths <- c()
    for(j in seq(Rfun)){
        arg1 <- lapply(formals(Rfun[[j]]),
                       function(x){
                           if(is.call(x)){
                               as.character(x)[-1]
                           }else{
                               ifelse(is.null(x), "", as.character(x))
                           }
                       })
        args <- c(args, arg1[!names(arg1) %in% names(args)])
        
        argL <- as.list(paste0("this.", names(arg1)))
        names(argL) <- names(arg1)
        Args <- gsub("\"", "", toJSON(argL, auto_unbox = T))

        if(outType[j] == "plot"){
            rMeths[j] <- paste0(names(Rfun)[j], ': function () {var req = $("#', outID[j], '").rplot("', names(Rfun)[j], '", ', Args, ');}')
        }else if(outType[j] == "text"){
            rMeths[j] <- paste0(names(Rfun)[j],
                                ': function () {var self = this; var req = ocpu.rpc("',
                                names(Rfun)[j], '", ', Args,
                                ', function(output){ self.$emit("',
                                tolower(outID[j]), 'text-gen", output) });}')
            vMeths[j] <- paste0(tolower(outID[j]),
                                'textgen: function(text){this.$refs.',
                                tmplID, 'Ref.', outID[j],' = text}')
            ilist <- list("")
            names(ilist) <- outID[j]
            args <- c(args, ilist)
        }else if(outType[j] == "table"){
            rMeths[j] <- paste0(names(Rfun)[j],
                                ': function () {var self = this; var req = ocpu.rpc("',
                                names(Rfun)[j], '", ', Args,
                                ', function(output){ self.$emit("',
                                tolower(outID[j]), 'table-gen", output) });}')
            vMeths[j] <- paste0(tolower(outID[j]),
                                'tablegen: function(table){this.$refs.',
                                tmplID, 'Ref.', outID[j],' = table}')
            ilist <- vector("list", 2)
            names(ilist) <- c("header", outID[j])
            args <- c(args, ilist)
        }
    }

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
    ## ##Rfun <- randomplot
    ## ##args <- formals(Rfun)
    ## argL <- as.list(paste0("this.", names(args)))
    ## names(argL) <- names(args)
    ## Args <- gsub("\"", "", toJSON(argL, auto_unbox = T))
    
    ## if(outType == "plot"){
    ##     rMeth <- paste0(names(Rfun)[1], ': function () {var req = $("#', outID, '").rplot("', names(Rfun)[1], '", ', Args, ');}')
    ##     methodList <- c(rMeth, methodList)
    ## }
    methodList <- paste(c(rMeths, unlist(methodList)), collapse = ",")
    methodList <- paste0('methods: {', methodList, '}')
    
    tmplJS <- paste0('Vue.component("', tolower(appID), '", {template: "#', tmplID, '", ', Dat, ' ,', methodList, '}); var vm = new Vue({el: "#', appID, '", vuetify: new Vuetify()});')
    if(!is.null(vMeths)){
        vmeth <- paste0('methods: {', paste(na.omit(vMeths), collapse = ","), '}')
        tmplJS <- paste0(sub("});$", ",", tmplJS), vmeth, "});")
    }
    HTML(tmplJS)
}
