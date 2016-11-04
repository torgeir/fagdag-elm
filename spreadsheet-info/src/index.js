require('./main.css');

var Elm = require('./Main.elm');
var root  = document.getElementById('root');
var app = Elm.Main.embed(root);

window.showInfo = function(data) {
  var content = data.feed.entry;
  var entries = content.map(function(e) {
    return {
      title: e.title.$t,
      content: e.content.$t
    }
  });
  app.ports.entriesport.send(entries);
}
