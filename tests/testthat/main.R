
ui <- c(
    vtags$v_navigation_drawer(props = c("v-model" = "drawer",
                                        ":clipped"="$vuetify.breakpoint.lgAndUp",
                                        app = TRUE)),
    vtags$v_app_bar(props = c(":clipped-left" = "$vuetify.breakpoint.lgAndUp",
                              color = "blue darken-3",
                              app = TRUE,
                              dark = TRUE), list(
                                                vtags$v_app_bar_nav_icon(props = c("@click.stop" = "drawer = !drawer")),
                                                vtags$v_toolbar_title("Contacts"), 
                                                vtags$v_text_field(props = c(flat = TRUE,
                                                                             "hide-details" = TRUE,
                                                                             "prepend-inner-icon" = "search",
                                                                             label = "search")
                                                                   )
                                            )
                    )
)

ui <- BuildUI(uid = "header", ui)
renderUI(ui, dataList = list(drawer = ""), outdir = "~/Downloads/test")
