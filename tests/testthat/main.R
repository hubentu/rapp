
ui <- c(
    vtags$v_navigation_drawer(props = c("v-model" = "drawer",
                                        ":clipped"="$vuetify.breakpoint.lgAndUp",
                                        app = TRUE)),
    vtags$v_app_bar(props = c(":clipped-left" = "$vuetify.breakpoint.lgAndUp",
                              color = "blue darken-3",
                              app = TRUE,
                              dark = TRUE), list(
                                                vtags$v_app_bar_nav_icon(props = c("@click.stop" = "drawer = !drawer")),
                                                vtags$v_toolbar_title("Contacts", props = c("style" = "width: 300px")), 
                                                text_field(flat = TRUE,
                                                           "hide-details" = TRUE,
                                                           "prepend-inner-icon" = "mdi-magnify",
                                                           label = "search"),
                                                vtags$v_spacer(),
                                                btn(label = list(vtags$v_icon("mdi-settings")),
                                                    icon = TRUE)
                                            )
                    )
)

ui <- BuildUI(uid = "header", ui)
renderUI(ui, dataList = list(drawer = ""), outdir = "/tmp/test")

ui <- vheader(title = "contact", ui = list(
                                     searchBar(),
                                     vspacer(),
                                     btn_icon(icon = "mdi-settings")))
renderUI(ui, uid = "header", dataList = list(drawer = ""), outdir = "/tmp/test")



mainbars <- dashBar(title = "test",
                sideTitles = c("Home", "Plot"),
                sideIcons = c("mdi-home-city", "mdi-chart-line"),
                sidePath = c("home", "plot"))
mainbars <- dashBar(title = "test",
                sideTitles = c("Home"),
                sideIcons = c("mdi-home-city"),
                sidePath = c("plot"))

plotui <- card(title = "Test App", text = "{{textA[0]}}",
               "min-width" = "800", "max-width" = "1000",
               uiList=list(text_field(model="n", label="number"),
                           vselect(model = "dist", label = "distribution",
                                   change = "gendat"),
                           data_table(table = "tsum"),
                           br(),
                           btn(onClick = "datplot"),
                           br(),
                           vimg(id = "plotOut", height = "600")))
plotPage <- renderTemplate(plotui,
                           Rfun = list(gendat = gendat, datplot = datplot),
                           outType = list(list("text", "text", "table"), "plot"),
                           outID = list(list("textA", "rdat", "tsum"), "plotOut"))

home <- renderTemplate(list(div(h1("Hello world"))))

renderIndex(main = mainbars, sidePages = list(home = home, plot = plotPage),
            Rfun = list(gendat = gendat, datplot = datplot),
            outType = list(list("text", "text", "table"), "plot"),
            outID = list(list("textA", "rdat", "tsum"), "plotOut"),
            outdir = "/tmp/dash")

BuildApp(app= "dash", ui = mainbars, sidePage = list(home = home, plot = plotPage),
         Rfun = list(gendat = gendat, datplot = datplot),
         dir = tempdir())

