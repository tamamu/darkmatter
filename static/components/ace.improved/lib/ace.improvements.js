(function(ace) {

var Editor = ace.require("ace/editor").Editor;
var oop = ace.require("ace/lib/oop");

oop.mixin(Editor.prototype, {

  posToIdx: function(pos) { return this.session.doc.positionToIndex(pos); },
  idxToPos: function(idx) { return this.session.doc.indexToPosition(idx); },

  getCursorIndex: function() { return this.posToIdx(this.getCursorPosition()); },
  moveCursorToIndex: function(idx) { return this.moveCursorToPosition(this.idxToPos(idx)) },

  setSelection: function(rangeOrString, maybeIdx) {
    var range = this.addSelection(rangeOrString, maybeIdx);
    this.selection.toSingleRange(range);
    return range;
  },

  addSelection: function(rangeOrString, maybeIdx) {
    var range;
    if (typeof rangeOrString === 'string') {
      range = parsePosRange(rangeOrString);
    } else if (typeof rangeOrString === 'number') {
      range = Range.fromPoints(this.idxToPos(rangeOrString), this.idxToPos(maybeIdx));
    } else range = rangeOrString;
    if (range) this.selection.addRange(range);
    return range;
  },

  saveExcursion: function(doFunc) {
    // will remember the current selection. doFunc can change the
    // selection, cursor position etc and then invoke the passed in callback
    // `reset` to undo those changes
    var ranges = this.selection.getAllRanges(), self = this;
    function reset() {
      self.selection.setRange(ranges[0]);
      ranges.slice(1).forEach(function(ea) {
        self.selection.addRange(ea); });
    }
    return doFunc.call(this, reset);
  }

});

// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// wrap the mode changer of ace so that we can automatically hook into any
// editor.
var edProto = ace.require('ace/editor').Editor.prototype;
var onChangeMode = edProto.onChangeMode.originalFunction || edProto.onChangeMode;
edProto.onChangeMode = function(e) {
  var mode = this.session.getMode();
  var res = onChangeMode.call(this, e);
  if (mode.attachToEditor) mode.attachToEditor(this);
  return res;
};
edProto.onChangeMode.originalFunction = onChangeMode;

// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// helper
var Range = ace.require('ace/range').Range;
var posRe = /^\s*\[([0-9]+)\/([0-9]+)\]$/;
function parsePosString(s) {
  // s like [9/23]
  var match = s.match(posRe);
  return match ? {
    row: Number(match[1]),
    column: Number(match[2])
  } : null;
}

function parsePosRange(s) {
  // s like Range: [9/23] -> [2/23]
  var startEndS = s.replace("Range:","").split("->");
  var start = parsePosString(startEndS[0]);
  var end = parsePosString(startEndS[1]);
  if (!end || !start) return null;
  return Range.fromPoints(start, end);
}

ace.improved = true;

})(window.ace);
