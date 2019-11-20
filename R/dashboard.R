vueTmpJS <- function(Rfun, outType = list("plot"), outID = list("plotOut"),
                     dataList = list(), methodList = list()){
    tmplID <- "appRef.$refs.route"
    ## tmplID <- ui$attribs$id
    ## appID <- paste0(tmplID, "App")

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
                                 'this.$refs.testRef.', outID[[j]][m], 'headers=hd;')
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
    Dat <- paste0("data: function(){return ", toJSON(dataList, auto_unbox = T), "}")
    methodList <- paste(c(rMeths, unlist(methodList)), collapse = ",")
    methodList <- paste0('methods: {', methodList, '}')
    
    ##tmplJS <- paste0('Vue.component("', tolower(appID), '", {template: "#', tmplID, '", ', Dat, ' ,', methodList, '});')
    tmplJS <- paste0('module.exports = {', Dat, ' ,', methodList, '}')
    ## vjs <- paste0('var vm = new Vue({el: "#', appID, '", vuetify: new Vuetify()});')
    if(!is.null(vMeths)){
        ## vmeth <- paste0('methods: {', paste(na.omit(vMeths), collapse = ","), '}')
        vmeth <- paste(na.omit(vMeths), collapse = ",")
        ## vjs <- paste0(sub("});$", ",", vjs), vmeth, "});")
    }else{
        vmeth  <- NULL
    }
    list(tmplJS = tmplJS, vmethod = vmeth)
}


renderTemplate <- function(ui, Rfun = list(),
                           outType = list(), outID = list(), app = TRUE,
                           dataList = list, methodList = list()){
    if(!is.list(ui)){
        ui <- list(ui)
    }
    if(app){
        vhtml <- tags$template(div(vtags$v_app(ui)))
    }else{
        vhtml <- tags$template(div(ui))
    }
    js <- vueTmpJS(Rfun, outType, outID, dataList, methodList)
    list(html = tagList(vhtml, tags$script(paste0("\n", js$tmplJS, "\n"))),
         js = js$vmethod)
}

renderIndex <- function(uid = "main", main, sidePages = list(),
                        Rfun=list(), outType=list("plot"), outID=list("plotOut"),
                        dataList=list(), methodList=list(),
                        outdir, vuejs="lib/app.js", opencpu=TRUE){
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
        ## ui <- tagList(ui, tags$head(singleton(tags$script(src="lib/ocpu.js"))))
    }
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

    Index <- tagList(deps,
                     tags$div(id = paste0(uid, "App"), atag),
                     tags$script(src = vuejs))
    ## router
    routes <- c()
    ## first page
    route0 <- list(list(path = paste0("''"), 
                        component = paste0(
                            "httpVueLoader('views/",
                            names(sidePages)[1], ".vue')")))

    for(i in seq(sidePages)){
        routes[i] <- list(list(path = paste0("'", names(sidePages)[i], "'"),
                                 component = paste0(
                                     "httpVueLoader('views/",
                                     names(sidePages)[i], ".vue')")))
    }
    routes <- c(route0, routes)
    routes <- gsub('"', '', as.character(toJSON(routes, auto_unbox = TRUE)))
    routes <- paste0('const routes = ', routes,
                     "; const router = new VueRouter({routes})")
    
    vjs <- vueTmpJS(Rfun, outType, outID, dataList, methodList)
    vJS <- paste0('var vm = new Vue({el: "#',
                  paste0(uid, "App"),
                  '", router, vuetify: new Vuetify(), ',
                  'components: {', paste0(uid, "app"),
                  ': httpVueLoader("', paste0('views/', uid, '.vue'), '")}',
                  '});')

    Main <- renderTemplate(ui = list(vh, vd$ui), dataList = vd$data)
    dir.create(file.path(outdir, "views"),
               showWarnings = FALSE, recursive = TRUE)
    writeLines(as.character(Main$html), file.path(outdir, "views", paste0(uid, ".vue")))
    
    vjs <- vueJS(ui, Rfun, outType, outID, dataList, methodList)



    dir.create(file.path(outdir, dirname(vuejs)),
               showWarnings = FALSE, recursive = TRUE)
    writeLines(vjs, file.path(outdir, vuejs))
    htmlout <- file.path(outdir, paste0(uid, ".html"))
    save_html(UI, file = htmlout)
    message("UI rendered to ", htmlout)
    return(htmlout)

}