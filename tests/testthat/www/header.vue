<template>
  <div class="header"><v-app>
  <v-app-bar :clipped-left="$vuetify.breakpoint.lgAndUp" color="blue" app dark>
    <v-app-bar-nav-icon @click.stop="drawer = !drawer">
    </v-app-bar-nav-icon>
    <v-toolbar-title style="width: 300px">title</v-toolbar-title>
  </v-app-bar>
  <v-navigation-drawer v-model="drawer" :clipped="$vuetify.breakpoint.lgAndUp" app>
    <v-list dense nav>
      <v-list-item v-for="item in dItems" :key="item.title" :to="item.path == '#' ? '' : item.path" router>
        <v-list-item-icon>
          <v-icon>{{item.icon}}</v-icon>
        </v-list-item-icon>
        <v-list-item-content>
          <v-list-item-title>{{item.title}}</v-list-item-title>
        </v-list-item-content>
      </v-list-item>
    </v-list>
  </v-navigation-drawer>
  <v-content fluid>
    <router-view ref="routeRef"></router-view>
  </v-content>
</v-app></div>
</template>

<script>
module.exports = {
    data: function(){
        return {
            "drawer": "true",
            "dItems": [{
                "title": "Home",
                "icon": "mdi-home-city",
                "path": "Dashboard"
            }, {
                "title": "Account",
                "icon": "mdi-account",
                "path": "Projects"
            }, {
                "title": "Users",
                "icon": "mdi-account-group-outline",
                "path": "Team"
            }]
        }
    }
}
</script>