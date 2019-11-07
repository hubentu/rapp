Vue.component('date-picker', {
    template: '<input/>',
    props: [ 'dateFormat' ],
    mounted: function() {
    var self = this;
    $(this.$el).datepicker({
      dateFormat: this.dateFormat,
      onSelect: function(date) {
        self.$emit('update-date', date);
      }
    });
    },
    beforeDestroy: function() {
      $(this.$el).datepicker('hide').datepicker('destroy');
    }
  });
  
  var vdat = new Vue({
    el: '#app',
    data: {
      date: null
    },
    methods: {
      updateDate: function(date) {
        this.date = date;
      }
    }
  });