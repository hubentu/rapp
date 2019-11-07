Vue.component("testapp", {
    template: "#test",
    data() {
        return {
            "n": "100",
            "dist": "",
            "distItems": ["normal", "uniform"],
            "textA": "textB"
        }
    },
    methods: {
        gendat: function () {
            var self = this
            var req = ocpu.rpc("gendat", {
                n: this.n,
                dist: this.dist
            }, function(output){
                //$("textC").text(output)
                self.$emit("text-gen", output)
            });
            
            req.fail(function(){
                alert("Server error: " + req.responseText);
            });
        }
    }
});
var vm = new Vue({
    el: "#testApp",
    vuetify: new Vuetify(),
    data: {
        textD: null
    },
    methods: {
        textgen: function(text){
            this.textD = text
        }
    }
});
