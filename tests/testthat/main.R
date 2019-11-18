
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

vdrawer <- function(titles, icons, urls){
    listItem <- vtags$v_list_item(
                          props = c(
                              "v-for" = paste("item in dItems"),
                              ":key" = "item.title",
                              "@click" = "item.url"),
                          list(
                              vtags$v_list_item_icon(
                                        list(vtags$v_icon("{{item.icon}}"))
                                    ),
                              vtags$v_list_item_content(
                                        list(vtags$v_list_item_title("{{item.title}}"))
                                    )))

    ui <- vtags$v_navigation_drawer(
                    props = c("v-model" = "drawer",
                              ":clipped"="$vuetify.breakpoint.lgAndUp",
                              app = TRUE),
                    list(
                        vtags$v_list(
                                  props = c(dense = TRUE, nav = TRUE),
                                  list(listItem))
                    ))
    items <- list()
    for(i in seq(titles)){
        items[[i]] <- list(title = titles[i],
                           icon = icons[i],
                           url = urls[i])
    }
    data <- list(drawer = "true",
                 dItems = items)
    return(list(ui = ui, data = data))
    
}

vh <- vheader(title = "title")
vd <- vdrawer(titles = c("Home", "Account", "Users"),
              icons = c("mdi-home-city", "mdi-account", "mdi-account-group-outline"),
              urls = c("#a", "#b", "#c"))

renderUI(ui = c(vh, vd$ui), uid = "header", dataList = vd$data, outdir = "/tmp/test")
