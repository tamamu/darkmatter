(function(ace) {

ace.ext = ace.ext || {};
ace.ext.lang = ace.ext.lang || {};
var codemarker = ace.ext.lang.codemarker = {
  CodeMarker: CodeMarker,
  ensureIn: ensureIn
};

function ensureIn(session, id) {
  var name = '$ext.lang.codeMarker';
  if (id) name += "-" + id;
  var marker = session[name];
  if (!marker) {
    marker = session[name] = new CodeMarker();
    marker.attach(session);
  }
  return marker;
}

// used as a "dynamic marker" inside an ace editor. The update method draws
// into the ace rendering area
function CodeMarker() {
  this.markerRanges = [];
}

(function() {

  this.attach = function(session) {
    if (!this.id || !(this.id in session.$backMarkers))
      session.addDynamicMarker(this);
  }

  this.detach = function(session) {
    this.markerRanges.length = 0;
    session._signal('changeBackMarker');
    session.removeMarker(this);
    session.$astFeedbackMarker = null;
  }

  this.update = function(html, markerLayer, session, config) {
    var Range = ace.require("ace/range").Range;
    var screenStartRow = config.firstRow, screenEndRow = config.lastRow;
    this.markerRanges.forEach(function(range) {
      var start, end;
      if ("pos" in range) {
        start = session.doc.indexToPosition(range.pos-1),
        end = session.doc.indexToPosition(range.pos+1);
      } else if ("start" in range && "end" in range) {
        start = session.doc.indexToPosition(range.start);
        end = session.doc.indexToPosition(range.end);
      } else if ("startPos" in range && "endPos" in range) {
        start = range.startPos;
        end = range.endPos;
      } else {
        console.warn('ace.ext.lang.codemarker cannot render %s', range);
        return;
      }
      if (start.row < screenStartRow || end.row > screenEndRow) return;
      var realRange = Range.fromPoints(start, end);
      var method = start.row === end.row ? 'drawSingleLineMarker' : 'drawTextMarker';
      markerLayer[method](html, realRange.toScreenRange(session), range.cssClassName || "ace-ext-lang-codemarker", config);
    });
  }

  this.redraw = function(session) {
    session._signal('changeBackMarker');
  }

  this.set = function(markerRanges, session) {
    this.markerRanges = markerRanges;
    this.redraw(session);
  }

}).call(CodeMarker.prototype);

})(window.ace);
