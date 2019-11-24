vueTmpJS <- function(Rfun, outType = list("plot"), outID = list("plotOut"),
                     dataList = list(), methodList = list()){
    jsList <- vueJS(Rfun = Rfun, outType = outType, outID = outID,
                    dataList = dataList, methodList = methodList)
    Dat <- jsList$Dat
    methodList <- jsList$methodList

    if(methodList != ""){
        methodList <- paste0('methods: {', methodList, '}')
        tmplJS <- paste0('module.exports = {', Dat, ',', methodList, '}')
    }else if(length(dataList) > 0){
        tmplJS <- paste0('module.exports = {', Dat, '}')
    }else{
        tmplJS <- NULL
    }
    return(HTML(tmplJS))
}

renderTemplate <- function(ui, Rfun = list(),
                           outType = list(), outID = list(), app = FALSE,
                           dataList = list(), methodList = list()){
    if(!is.list(ui)){
        ui <- list(ui)
    }
    if(app){
        vhtml <- tags$template(div(vtags$v_app(ui)))
    }else{
        vhtml <- tags$template(div(ui))
    }
    js <- vueTmpJS(Rfun, outType, outID, dataList, methodList)
    if(nchar(js) == 0){
        vhtml
    }else{
        tagList(vhtml, tags$script(js))
    }
}

vdeps <- function(opencpu){
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
                       stylesheet = "materialdesignicons.min.css"),
        htmlDependency("http-vue-loader", "1.4.1",
                       c(href = "https://unpkg.com/http-vue-loader@1.4.1/src"),
                       script = "httpVueLoader.js"),
        htmlDependency("vue-router", "3.1.1",
                       c(href = "https://unpkg.com/vue-router@3.1.3/dist"),
                       script = "vue-router.js"))
    if(opencpu){
        odeps <- list(
            htmlDependency("jquery", "1.11.1",
                           c(href = "https://code.jquery.com"),
                           script = "jquery-1.11.1.min.js"),
            htmlDependency("opencpu", "0.5",
                           c(href = "https://cdn.opencpu.org"),
                           script = "opencpu-0.5.js"))
        deps <- c(deps, odeps)
    }
    deps
}

renderIndex <- function(uid = "main", main, sidePages = list(),
                        outdir, opencpu=TRUE){
    stopifnot(!is.null(names(sidePages)))
    if(uid %in% names(sidePages))
        stop("sidePages should have a different name from ", uid)
    paths <- unlist(main$data, use.names = TRUE)
    paths <- paths[grep(".path", names(paths))]
    if(length(paths) > 0){
        if(!all(paths %in% names(sidePages)))
            stop("sidPaths should match sidePage names")
    }
    deps <- vdeps(opencpu)
    vbind <- c()
    ## for(i in seq(outType)){
    ##     if(is.character(outType[[i]]) && outType[[i]] == "text"){
    ##         vbind[i] <- paste0(tolower(outID[[i]]), "textgen")
    ##         names(vbind)[i] <- paste0("@", tolower(outID[[i]]), "text-gen")
    ##     }else if(is.character(outType[[i]]) && outType[[i]] == "table"){
    ##         vbind[i] <- paste0(tolower(outID[[i]]), "tablegen")
    ##         names(vbind)[i] <- paste0("@", tolower(outID[[i]]), "table-gen")
    ##     }else if(is.list(outType[[i]])){
    ##         vbind[i] <- paste0(paste(tolower(unlist(outID[[i]])), collapse = ""), "gen")
    ##         names(vbind)[i] <- paste0("@", paste(tolower(unlist(outID[[i]])), collapse = "-"), "-gen")
    ##     }
    ## }
    ## vfuns <- unlist(lapply(sidePages, function(x)names(x$js)))
    ## if(length(vfuns) > 0){
    ##     vfuns <- unlist(strsplit(vfuns, split = ","))
    ##     vbind <- sub("-gen$", "gen", vfuns)
    ##     names(vbind) <- paste0("@", vfuns)
    ## }
    ## atag <- tag(tolower(paste0(uid, "app")),
    ##             na.omit(c(vbind, ref = "appRef")))
    atag <- tag(tolower(paste0(uid, "app")), c(ref = "appRef"))
    dir.create(file.path(outdir, "lib"),
               showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(outdir, "views"),
               showWarnings = FALSE, recursive = TRUE)

    ## app.js/router
    vuejs <- file.path("lib", "app.js")
    routes <- c()
    ## first page
    route0 <- list(list(path = paste0("''"), 
                        component = paste0(
                            "httpVueLoader('views/",
                            names(sidePages)[1], ".vue')")))

    for(i in seq(sidePages)){
        routes[i] <- list(list(path = paste0("'/", names(sidePages)[i], "'"),
                                 component = paste0(
                                     "httpVueLoader('views/",
                                     names(sidePages)[i], ".vue')")))
    }
    routes <- c(route0, routes)
    routes <- gsub('"', '', as.character(toJSON(routes, auto_unbox = TRUE)))
    routes <- paste0('const routes = ', routes,
                     "; const router = new VueRouter({routes})")
    
    ## vmeths <- unlist(lapply(sidePages, function(x)x$js))
    ## if(length(vmeths) > 0){
    ##     vMeths <- paste0(', methods: {', paste(vmeths, collapse = ","), '}')
    ## }else{
    ##     vMeths <- NULL
    ## }
    
    vJS <- paste0(routes, '; var vm = new Vue({el: "#',
                  paste0(uid, "App"),
                  '", router, vuetify: new Vuetify(), ',
                  'components: {', paste0(uid, "app"),
                  ': httpVueLoader("', paste0('views/', uid, '.vue'), '")}',
                  '});')
    ## app.js
    writeLines(vJS, file.path(outdir, vuejs))
    ## Index.html
    Index <- tagList(deps,
                     tags$div(id = paste0(uid, "App"), atag),
                     tags$script(src = vuejs))
    message("Writing ", file.path(outdir, "index.html"))
    save_html(Index, file = file.path(outdir, "index.html"))
    ## app.vue
    rview <- vtags$v_content(list(vtags$router_view(props = c(ref = "routeRef"))))
    Main <- renderTemplate(ui = c(main$ui, rview), dataList = main$data, app = TRUE)
    message("Writing ", file.path(outdir, "views", paste0(uid, ".vue")))
    writeLines(as.character(Main), file.path(outdir, "views", paste0(uid, ".vue")))
    ## sidePages.vue
    for(i in seq(sidePages)){
        writeLines(as.character(sidePages[[i]]),
                   file.path(outdir, "views", paste0(names(sidePages)[i], ".vue")))
        
    }

    return(file.path(outdir, "index.html"))
}
