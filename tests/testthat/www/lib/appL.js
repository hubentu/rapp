const routes = [
    { path: '', component: httpVueLoader('views/Dashboard.vue') },
    { path: '/Dashboard', component: httpVueLoader('views/Dashboard.vue') },
    { path: '/Projects', component: httpVueLoader('views/Projects.vue') },
    { path: '/Team', component: httpVueLoader('views/Team.vue') }
  ]
  
const router = new VueRouter({
    routes // short for `routes: routes`
})

var vm = new Vue({
    el: "#headerApp",
    router,
    vuetify: new Vuetify(),
    components: {
        'headerapp': httpVueLoader('header.vue')
    }
});

