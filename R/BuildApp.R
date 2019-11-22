#' Build a web APP
#'
#' @import devtools
#' @importFrom usethis create_package
#' @importFrom codetools findGlobals
#' @importFrom utils find
#' @include vtags.R
BuildApp <- function(app, ui, sidePages = list(), Rfun = list(), dir = tempdir(), outType = list("plot"), outID = list("plotOut"), dataList = list(), methodList = list()){
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
            dumpFun(fun = Rfun[[i]], fname = names(Rfun)[i],
                    type = outType[i], path = path, app = app)
            ##browser()
            ## ff <- findGlobals(names(Rfun)[i])
            ## ff1 <- ff[grepl(".GlobalEnv", sapply(ff, find))]
            ## pkgs <- grep("package:", unlist(sapply(ff, find)), value = TRUE)
            ## pkgs <- sub("package:", "",
            ##             unique(setdiff(pkgs, c("package:base", paste0("package:", app)))))

            ## Rfun1 <- deparse(Rfun[[i]])
            ## Rfun1[1] <- paste(names(Rfun)[i], "<-", Rfun1[1])
            ## if(length(pkgs) > 0){
            ##     Rfun1 <- c(paste("#'", names(Rfun)[i]),
            ##                paste("#' @import", pkgs),
            ##                "#' @export", Rfun1)
            ## }else{
            ##     Rfun1 <- c(paste("#'", names(Rfun)[i]),
            ##                "#' @export", Rfun1)
            ## }
            ## rfile <- file.path(path, "R", paste0(names(Rfun)[i], ".R"))

            ## writeLines(Rfun1, rfile)
            ## dump(ff1, rfile, append = TRUE)
        }
    }
    document(path)
    
    ## UI
    wwwdir <- file.path(path, "inst", "www")
    ## dir.create(libdir, recursive = TRUE, showWarnings = FALSE)
    ## vjs <- vueJS(ui, Rfun, outType, outID, dataList, methodList)
    ## writeLines(vjs, file.path(libdir, "app.js"))
    ## UI <- renderUI(ui, vuejs = "lib/app.js", opencpu = TRUE, outType, outID)
    
    ## save_html(UI,
    ##           file = file.path(dirname(libdir),
    ##                            paste0(deparse(substitute(ui)), ".html")))
    if(length(sidePages) > 0){
        renderIndex(main = ui, sidePages = sidePages, outdir = wwwdir)
    }else{
        renderUI(ui = ui, uid = app, Rfun = Rfun,
                 dataList = dataList, methodList = methodList,
                 outdir = wwwdir, vuejs = file.path("lib", paste0(app, ".js")),
                 outType = outType, outID = outID)
    }
    message("Package created: ", path)
    return(path)
}

dumpFun <- function(fun, fname, type, path, app){
    ff <- findGlobals(fname)
    ff1 <- ff[grepl(".GlobalEnv", sapply(ff, find))]

    pkgs <- lapply(sapply(ff, find, USE.NAMES = TRUE),
                   function(x){
                       x <- grep("package:", x, value = TRUE)
                       x <- x[!x %in% c("package:base", "package:rapp", paste0("package:", app))]
                       sub("package:", "", x)
                   })
    pkgs <- pkgs[lengths(pkgs) > 0]
    if(length(pkgs) > 0){
        impkg <- c()
        for(i in 1:length(pkgs)){
            impkg <- c(impkg, paste("#' @importFrom", pkgs[[i]], names(pkgs)[i]))
        }
    }
    Rfun1 <- deparse(fun)
    if(type == "html"){
        Rfun1[1] <- paste0(fname, "_ <- ", Rfun1[1])
    }else{
        Rfun1[1] <- paste(fname, "<-", Rfun1[1])
    }
    if(length(pkgs) > 0){
        Rfun1 <- c(paste("#'", fname),
                   impkg,
                   "#' @export", Rfun1)
    }else{
        Rfun1 <- c(paste("#'", fname),
                   "#' @export", Rfun1)
    }

    if(type == "html"){
        sfun <- c(paste0("#' ", fname, "save"),
                  paste0("#' @importFrom htmlwidgets saveWidget"),
                  paste0("#' @export"),          
                  paste0(fname, " <- function(...){"),
                  paste0("gg <- ", fname, "_(...)"),
                  "saveWidget(gg, file = 'index.html', selfcontained = FALSE)",
                  "}")
        Rfun1 <- c(Rfun1, sfun)
    }
    
    rfile <- file.path(path, "R", paste0(fname, ".R"))    
    writeLines(Rfun1, rfile)
    if(length(ff1) > 0){
        dump(ff1, rfile, append = TRUE)
    }
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
renderUI <- function(ui, uid, Rfun = list(), dataList = list(), methodList = list(), outdir,
                     vuejs = "lib/app.js", opencpu = TRUE, outType = list("plot"),
                     outID = list("plotOut")){
    deps <- list(
        htmlDependency("vue", "2.6.10",
                       c(href = "https://cdn.jsdelivr.net/npm/vue@2.6.10/dist"),
                       script = "vue.js"),
        htmlDependency("vuetify", "2.0.2",
                       c(href = "https://cdn.jsdelivr.net/npm/vuetify@2.0.2/dist"),
                       script = "vuetify.min.js",
                       stylesheet = "vuetify.min.css"),
        htmlDependency("materialdesignicons", "4",
                       c(href = "https://cdn.jsdelivr.net/npm/@mdi/font@4.x/css"),
                       stylesheet = "materialdesignicons.min.css"))
        ## htmlDependency("MaterialIcons", "1",
        ##                c(href = "https://fonts.googleapis.com/css?family=Material+Icons"),
        ##                stylesheet = ""))
    if(opencpu){
        odeps <- list(
            htmlDependency("jquery", "1.11.1",
                           c(href = "https://code.jquery.com"),
                           script = "jquery-1.11.1.min.js"),
            htmlDependency("opencpu", "0.5",
                           c(href = "https://cdn.opencpu.org"),
                           script = "opencpu-0.5.js"))
        deps <- c(deps, odeps)
        ## ui <- tagList(ui, tags$head(singleton(tags$script(src="lib/ocpu.js"))))
    }
    if(!is.list(ui)){
        ui <- list(ui)
    }
    ui <- div(id = uid, vtags$v_app(ui))
    vbind <- c()
    for(i in seq(outType)){
        if(is.character(outType[[i]]) && outType[[i]] == "text"){
            vbind[i] <- paste0(tolower(outID[[i]]), "textgen")
            names(vbind)[i] <- paste0("@", tolower(outID[[i]]), "text-gen")
        }else if(is.character(outType[[i]]) && outType[[i]] == "table"){
            vbind[i] <- paste0(tolower(outID[[i]]), "tablegen")
            names(vbind)[i] <- paste0("@", tolower(outID[[i]]), "table-gen")
        }else if(is.list(outType[[i]])){
            vbind[i] <- paste0(paste(tolower(unlist(outID[[i]])), collapse = ""), "gen")
            names(vbind)[i] <- paste0("@", paste(tolower(unlist(outID[[i]])), collapse = "-"), "-gen")
        }
    }
    atag <- tag(tolower(paste0(uid, "app")),
                na.omit(c(vbind, ref = paste0(uid, "Ref"))))
    
    UI <- tagList(deps,
                  tags$div(id = paste0(uid, "App"), atag),
                  tags$script(type = "text/x-template", id = uid, ui),
                  tags$script(src = vuejs))

    vjs <- vueJS(ui, Rfun, outType, outID, dataList, methodList)
    dir.create(file.path(outdir, dirname(vuejs)),
               showWarnings = FALSE, recursive = TRUE)
    writeLines(vjs, file.path(outdir, vuejs))
    htmlout <- file.path(outdir, paste0(uid, ".html"))
    save_html(UI, file = htmlout)
    message("UI rendered to ", htmlout)
    return(htmlout)
}

#' vue js
vueJS <- function(ui, Rfun, outType = list("plot"), outID = list("plotOut"), dataList = list(), methodList = list()){
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

        if(is.character(outType[[j]]) && outType[[j]] == "plot"){
            rMeths[j] <- paste0(names(Rfun)[j],
                                ': function () {var req = $("#',
                                outID[[j]], '").rplot("',
                                names(Rfun)[j], '", ', Args, ');}')
        }else if(is.character(outType[[j]]) && outType[[j]] == "html"){
            rMeths[j] <- paste0(names(Rfun)[j],
                                ': function () {var req = ocpu.call("',
                                names(Rfun)[j], '", ', Args,
                                ', function(session){$("iframe#', outID[[j]],
                                '").attr("src", session.getFileURL("index.html"));}', ');}')
        }else if(is.character(outType[[j]]) && outType[[j]] == "text"){
            rMeths[j] <- paste0(names(Rfun)[j],
                                ': function () {var self = this; var req = ocpu.rpc("',
                                names(Rfun)[j], '", ', Args,
                                ', function(output){ self.$emit("',
                                tolower(outID[[j]]), 'text-gen", output) });}')
            vMeths[j] <- paste0(tolower(outID[[j]]),
                                'textgen: function(text){this.$refs.',
                                tmplID, 'Ref.', outID[[j]],' = text}')
            ilist <- list("")
            names(ilist) <- outID[[j]]
            args <- c(args, ilist)
        }else if(is.character(outType[[j]]) && outType[[j]] == "table"){
            rMeths[j] <- paste0(names(Rfun)[j],
                                ': function () {var self = this; var req = ocpu.rpc("',
                                names(Rfun)[j], '", ', Args,
                                ', function(output){ self.$emit("',
                                tolower(outID[[j]]), 'table-gen", output) });}')
            vMeths[j] <- paste0(tolower(outID[[j]]),
                                'tablegen: function(table){this.$refs.',
                                tmplID, 'Ref.', outID[[j]],' = table;',
                                'let keys = Object.keys(table[0]);let hd=[];',
                                'for(i=0;i<keys.length;i++){hd[i]={text: keys[i], value: keys[i]}};',
                                'this.$refs.testRef.', outID[[j]], 'headers=hd}')
            ilist <- list(list(), list())
            names(ilist) <- c(paste0(outID[[j]], "headers"), outID[[j]])
            args <- c(args, ilist)
        }else if(is.list(outType[[j]])){
            rMeths[j] <- paste0(names(Rfun)[j],
                                ': function () {var self = this; var req = ocpu.rpc("',
                                names(Rfun)[j], '", ', Args,
                                ', function(output){ self.$emit("',
                                tolower(paste(unlist(outID[[j]]), collapse = "-")),
                                '-gen", output) });}')
            
            vMeths[j] <- paste0(paste(unlist(tolower(outID[[j]])), collapse = ""),
                       'gen: function(dat){')
            m1 <- c()
            for(m in seq(outType[[j]])){
                m1[m] <- paste0('this.$refs.', tmplID, 'Ref.', outID[[j]][m],'=Object.values(dat)[',m-1,'];')
                if(outType[[j]][m] == "table"){
                    m2 <- paste0('let keys = Object.keys(Object.values(dat)[',m-1,'][0]);let hd=[];',
                                 'for(i=0;i<keys.length;i++){hd[i]={text: keys[i], value: keys[i]}};',
                                 'this.$refs.', tmplID, 'Ref.', outID[[j]][m], 'headers=hd;')
                    m1[m] <- paste(m1[m], m2)

                    ilist <- list(list(), list())
                    names(ilist) <- c(paste0(outID[[j]][m], "headers"), outID[[j]][m])
                    args <- c(args, ilist)
                }else if(outType[[j]][m] == "text"){
                    ilist <- list("")
                    names(ilist) <- outID[[j]][m]
                    args <- c(args, ilist)
                }
            }
            vMeths[j] <- paste0(paste(c(vMeths[j], m1), collapse = ""), "}")
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


