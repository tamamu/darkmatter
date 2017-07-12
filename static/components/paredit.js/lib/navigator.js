/*global window, process, global,module*/

"format global";

;(function(run) {
  var isNodejs = typeof module !== "undefined" && module.exports;
  var exports = isNodejs ? module.exports : window.paredit;
  var util = isNodejs ? require('./util').util : window.paredit.util;
  run(util, exports);

})(function(util, exports) {

  function last(a) { return a[a.length-1]; };

  var nav = exports.navigator = {

    forwardSexp: function(ast, idx) {
      var current = last(w.containingSexpsAt(ast,idx,
        function(n) { return !w.hasChildren(n); }));
      if (current) return current.end;
      var next = w.nextSexp(ast, idx);
      return next ? next.end : idx;
    },

    backwardSexp: function(ast, idx) {
      var current = last(w.containingSexpsAt(ast,idx,
        function(n) { return !w.hasChildren(n); }));
      if (current) return current.start;
      var prev = w.prevSexp(ast, idx);
      return prev ? prev.start : idx;
    },

    forwardDownSexp: function(ast, idx) {
      var next = w.nextSexp(ast, idx,
        function(n) { return n.type === 'list'});
      if (!next) return idx;
      if (next.children && next.children[0])
        return next.children[0].start;
      return next.start + 1;
    },

    backwardUpSexp: function(ast, idx) {
      var containing = w.containingSexpsAt(ast, idx,
        function(n) { return n.type === 'list'
                   || n.type === 'string'
                   || n.type === 'comment'; });
      if (!containing || !containing.length) return idx;
      return last(containing).start;
    },

    closeList: function(ast, idx) {
      var containing = w.containingSexpsAt(ast, idx);
      var l = last(containing);
      if (!l || l.type === "toplevel") return idx;
      if (l.type === "string" || l.type === "comment") return undefined;
      var lists = containing.filter(w.hasChildren);
      return last(lists).end;
    },

    sexpRange: function(ast, idx) {
      // finds the range of the sexp at idx
      return nav.sexpRangeExpansion(ast, idx, idx);
    },

    sexpRangeExpansion: function(ast, startIdx, endIdx) {
      // startIdx, endIdx define a range. Return the range of the next
      // enclosing sexp.

      // If we have another non-list entity directly to our left or right like
      // in @*xxx* we select that

      if (startIdx !== endIdx) {
        // find the entity already selected...
        var directMatchedStart = last(w.sexpsAt(ast, startIdx, function(n) {
          return n.start === startIdx; }));
        var directMatchedEnd = directMatchedStart && last(w.sexpsAt(ast, endIdx, function(n) {
          return n.end === endIdx; }));
        if (directMatchedStart && directMatchedEnd) {
          var directLeft = last(w.sexpsAt(ast, startIdx, function(n) {
            return n.start < startIdx && !w.hasChildren(n); }));
          if (directLeft) return [directLeft.start, endIdx];
          var directRight = last(w.sexpsAt(ast, endIdx, function(n) {
            return endIdx < n.end && !w.hasChildren(n); }));
          if (directRight) return [startIdx, directRight.end];
        }
      }

      var sexp = last(util.flatFilterTree(ast, function(n) {
        if (n.type === 'toplevel') return false;
        if (startIdx === endIdx) return n.start <= startIdx && endIdx <= n.end;
        if (n.start === startIdx) return endIdx < n.end;
        if (n.end === endIdx) return n.start < startIdx;
        return n.start < startIdx && endIdx < n.end;
      }, getChildren));

      if (!sexp) return null;
      var isBorderSel = sexp.start === startIdx || sexp.end === endIdx;
      if (sexp.type === 'list' || sexp.type === 'string') {
        if (isBorderSel && (startIdx === sexp.start || endIdx === sexp.end))
          return [sexp.start, sexp.end];
        if (sexp.start+1 < startIdx || endIdx < sexp.end-1)
          return [sexp.start+1, sexp.end-1]
      }

      return [sexp.start, sexp.end];
    },

    rangeForDefun: function(ast, idx) {
      var node = ast.children && ast.children.filter(function(n) {
        return n.start <= idx && idx <= n.end;
      })[0];
      return node ? [node.start, node.end] : null;
    }
  };

  var w = exports.walk = {

    hasChildren: function(n) {
      return n.type === 'list'
          || n.type === 'toplevel'
          || (n.type === 'error' && n.children);
    },

    containingSexpsAt: function(ast, idx, matchFunc) {
      return util.flatFilterTree(ast, function(n) {
        return (n.type === 'toplevel'
             || (n.type === 'error' && n.start < idx && idx <= n.end)
             || (n.start < idx && idx < n.end))
            && (!matchFunc || matchFunc(n));
      }, getChildren);
    },

    sexpsAt: function(ast, idx, matchFunc) {
      return util.flatFilterTree(ast, function(n) {
        return n.start <= idx && idx <= n.end && (!matchFunc || matchFunc(n));
      }, getChildren);
    },

    nextSexp: function(ast, idx, matchFunc) {
      // Find the next sexp following idx. If idx directly points to a list start,
      // the list it is. Otherwise get the containing list and find the closest
      // following children sexp.

      var listsAt = util.flatFilterTree(ast, function(n) {
        return n.start <= idx && idx < n.end && w.hasChildren(n);
      }, getChildren);

      if (!listsAt.length) return null;

      var direct = listsAt.filter(function(n) {
        return n.start === idx && n.type !== 'toplevel'; })[0];
      if (direct) return direct;

      var list = last(listsAt).children.filter(function(n) {
        return idx <= n.start && (!matchFunc || !!matchFunc(n)); })
      if (list.length) return list[0];

      return null;
    },

    prevSexp: function(ast,idx,matchFunc) {
      var listsAt = util.flatFilterTree(ast, function(n) {
        return n.start < idx && idx <= n.end && w.hasChildren(n);
      }, getChildren);
      if (!listsAt.length) return null;

      var direct = listsAt.filter(function(n) {
        return n.end === idx && n.type !== 'toplevel';; })[0];
      if (direct) return direct;

      var list = last(listsAt).children.filter(function(n) {
        return n.end <= idx && (!matchFunc || !!matchFunc(n)); })
      if (list.length) return last(list);

      return null;
    },

    stringify: function(node) {
      return util.mapTree(node,
        function(n, children) {
          if (n.type === 'list' || n.type === 'toplevel')
            return '(' + children.join(" ") + ')';
          return n.source ? n.source :
            util.times(node.end-node.start, 'x'); },
        function(n) { return (n && n.children) || []; });
    },

    source: function(src, node) {
      return node.source ? node.source :
        src.slice(node.start,node.end);
    }

  }

  function getChildren(node) { return node.children || []; }
});
