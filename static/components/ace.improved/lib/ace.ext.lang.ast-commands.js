(function(ace) {
  ace.ext = ace.ext || {};
  ace.ext.lang = ace.ext.lang || {};

  ace.ext.lang.astCommands = [{
    name: 'forwardSexp',
    bindKey: 'Ctrl-Alt-f|Ctrl-Alt-Right',
    exec: execCodeNavigator('forwardSexp'),
    multiSelectAction: 'forEach',
    readOnly: true
  }, {
    name: 'backwardSexp',
    bindKey: 'Ctrl-Alt-b|Ctrl-Alt-Left',
    exec: execCodeNavigator('backwardSexp'),
    multiSelectAction: 'forEach',
    readOnly: true
  }, {
    name: 'backwardUpSexp',
    bindKey: 'Ctrl-Alt-u|Ctrl-Alt-Up',
    exec: execCodeNavigator('backwardUpSexp'),
    multiSelectAction: 'forEach',
    readOnly: true
  }, { 
    name: 'forwardDownSexp',
    bindKey: 'Ctrl-Alt-d|Ctrl-Alt-Down',
    exec: execCodeNavigator('forwardDownSexp'),
    multiSelectAction: 'forEach',
    readOnly: true
  }, {
    name: 'markDefun',
    bindKey: 'Ctrl-Alt-h',
    exec: execCodeNavigator('markDefun'),
    multiSelectAction: 'forEach',
    readOnly: true
  }, {
    name: 'expandRegion',
    bindKey: {win: 'Shift-Ctrl-E|Ctrl-Shift-Space', mac: 'Shift-Command-Space|Ctrl-Shift-Space'},
    exec: function(ed, args) {
      args = args || {};
      
      // if we get start/end position indexes to expand to handed in then we do
      // that
      var newState;
      var start = args.start, end = args.end;
      if (typeof start === "number" && typeof end === "number") {
        var state = ensureExpandState();
        newState = {range: [start, end], prev: ensureExpandState()}
      } else {
        // ... otherwise we leave it to the code navigator...
        var ast = ed.session.$ast;
        if (!ed.session.$ast) return;
        var mode = ed.session.getMode(),
            expander = mode.getCodeNavigator && mode.getCodeNavigator(),
            newState = expander.expandRegion(ed, ed.getValue(), ast, ensureExpandState());
      }

      if (newState && newState.range) {
        ed.selection.setSelectionRange({
          start: iToP(ed, newState.range[0]),
          end: iToP(ed, newState.range[1])});
        ed.$expandRegionState = newState;
      }

      ed.selection.once('changeCursor', function(evt) {
        ed.$expandRegionState = null; });

      // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-      

      function ensureExpandState() {
        var state = ed.$expandRegionState;
        var pos = pToI(ed, ed.getCursorPosition());
        if (state
          // has cursor moved? invalidate expansion state
         && (state.range  [0] === pos || state.range[1] === pos)) 
           return state;

        var aceRange = ed.getSelectionRange();
        return ed.$expandRegionState = {
          range: [pToI(ed,aceRange.start), pToI(ed,aceRange.end)]
        };
      }
    },
    multiSelectAction: 'forEach',
    readOnly: true
  }, {
    name: 'contractRegion',
    bindKey: {win: 'Shift-Ctrl-S|Ctrl-Alt-Space', mac: 'Ctrl-Command-space|Ctrl-Alt-Space'},
    exec: function(ed) {
      if (ed.getSelectionRange().isEmpty()) return;
      var ast = ed.session.$ast;
      if (!ed.session.$ast) return;
      var state = ed.$expandRegionState;
      if (!state) return;
      var mode = ed.session.getMode();
      var expander = mode.getCodeNavigator && mode.getCodeNavigator();
      if (!expander) return;
      var newState = expander.contractRegion(ed, ed.getValue(), ast, state);
      if (newState && newState.range) {
        ed.selection.setSelectionRange({
          start: iToP(ed, newState.range[0]),
          end: iToP(ed, newState.range[1])});
        ed.$expandRegionState = newState;
      }
      ed.selection.once('changeCursor', function(evt) { ed.$expandRegionState = null; });
    },
    multiSelectAction: 'forEach',
    readOnly: true
  }, {
    name: 'gotoNextError',
    bindKey: 'Ctrl-`',
    exec: function(ed, args) {
      var ast = ed.session.$ast;
      if (!ed.session.$ast) return;
      var backwards = args && args.backwards;
      var errs = ast.parseError ? [ast.parseError] : (ast.errors || []);
      var idx = pToI(ed, ed.getCursorPosition());
      var filteredErrs = errs.filter(function(err) {
        return backwards ? err.end < idx : idx < err.end; });
      var err = filteredErrs[backwards ? filteredErrs.length - 1 : 0];
      if (err) ed.moveCursorToPosition(iToP(ed, err.end));
    },
    multiSelectAction: 'forEach',
    readOnly: true
  }, {
    name: 'gotoPrevErrorOrWarning',
    exec: function(ed, args) {
      ed.execCommand('gotoNextErrorOrWarning', {backwards: true});
    },
    multiSelectAction: 'forEach',
    readOnly: true
  }];


  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  // helper
  // -=-=-=-

  function pToI(ed, pos) { return ed.session.doc.positionToIndex(pos); }
  function iToP(ed, pos) { return ed.session.doc.indexToPosition(pos); }
  
  function execCodeNavigator(sel) {
    return function(ed, args) {
      var mode = ed.session.getMode(),
          nav = mode.getCodeNavigator && mode.getCodeNavigator();
      if (!nav) return null;
      ed.pushEmacsMark && ed.pushEmacsMark(ed.getCursorPosition());
      var count = (args && args.count || 1);
      for (var i = 0; i < count; i++) { nav[sel](ed, args); }
    }
  }

})(window.ace);
