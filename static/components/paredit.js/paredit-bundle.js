/*global window, process, global, module*/

"format global";

// If not on nodejs: concat or load lib files after loading this files.

(function() {
  var isNodejs = typeof module !== "undefined" && module.exports,
      exports = isNodejs ? module.exports : (window.paredit = {});

  if (isNodejs) {
    exports.reader       = require("./lib/reader").reader;
    exports.navigator    = require("./lib/navigator").navigator;
    exports.editor       = require("./lib/editor").editor;
    exports.specialForms = require("./lib/editor").specialForms;
  }

  exports.parse = function(src, options) {
    options = options || {};
    var addSrc = options.hasOwnProperty('addSourceForLeafs') ?
       options.addSourceForLeafs : true;
    var errors = [];

    var nodes = exports.reader.readSeq(src, function xform(type, read, start, end, args) {
      var result = {type: type, start: start.idx, end: end.idx};
      if (type === "error") {
        result.error = read.error;
        if (read.children) result.children = read.children;
        errors.push(result);
      } else if (addSrc && type !== 'list')
        result.source = src.slice(result.start, result.end);
      if (type === "list") result.children = read;
      if (type === "list" || type === "string" || (type === "error" && args)) {
        result.open = args.open;
        result.close = args.close;
      }
      return result;
    });

    return {
      type: "toplevel", start: 0,
      end: (nodes && nodes.length && nodes[nodes.length-1].end) || 0,
      errors: errors,
      children: nodes
    };
  };

})();

/*global window, process, global,module*/

"format global";

;(function(run) {
  var isNodejs = typeof module !== "undefined" && module.exports;
  var exports = isNodejs ? module.exports : window.paredit;
  run(exports);

})(function(exports) {

  var util = exports.util = {
    merge: function (objs) {
      if (arguments.length > 1) {
        return util.merge(Array.prototype.slice.call(arguments));
      }

      if (Array.isArray(objs[0])) { // test for all?
        return Array.prototype.concat.apply([], objs);
      }

      return objs.reduce(function(merged, ea) {
        for (var name in ea)
          if (ea.hasOwnProperty(name))
              merged[name] = ea[name];
        return merged;
      }, {});
    },

    mapTree: function (treeNode, mapFunc, childGetter) {
      // Traverses the tree and creates a structurally identical tree but with
      // mapped nodes
      var mappedNodes = (childGetter(treeNode) || []).map(function(n) {
        return util.mapTree(n, mapFunc, childGetter);
      })
      return mapFunc(treeNode, mappedNodes);
    },

    flatFilterTree: function (treeNode, testFunc, childGetter) {
      // Traverses a `treeNode` recursively and returns all nodes for which
      // `testFunc` returns true. `childGetter` is a function to retrieve the
      // children from a node.
      var result = [];
      if (testFunc(treeNode)) result.push(treeNode);
      return result.concat(
        (childGetter(treeNode) || []).reduce(function(filtered, node) {
          return filtered.concat(util.flatFilterTree(node, testFunc, childGetter));
        }, []));
    },

    last: function(a) { return a[a.length-1]; },

    times: function(n, ch) { return new Array(n+1).join(ch); },

    clone: function(obj) {
      // Shallow copy
      if (Array.isArray(obj)) return Array.prototype.slice.call(obj);
      var clone = {};
      for (var name in obj) { clone[name] = obj[name]; }
      return clone;
    }
  }

});

/*global window, process, global,module*/

"format global";

;(function(run) {
  var isNodejs = typeof module !== "undefined" && module.exports;
  var exports = isNodejs ? module.exports : window.paredit;
  run(exports);
})(function(exports) {

  exports.reader = {

    readSeq: function(src, xform) {
      return readSeq(null,src,Object.freeze([]),startPos(),xform).context;
    },

    readSexp: function(src, xform) {
      return readSexp(null,src,Object.freeze([]),startPos(),xform).context[0];
    }
  };

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  // read logic

  var eosexp = {}, eoinput = {}, // flags
      close = {'[': ']', '(': ')', '{': '}'},
      opening = Object.keys(close),
      closing = opening.map(function(k) { return close[k]; }),
      symRe = /[^\s\[\]\(\)\{\},"\\`@^#~]/,
      readerSpecials = /[`@^#~]/;

  function readSexp(contextStart, input, context, pos, xform) {
    var ch = input[0];

    // We have reached the end of input but expting more.
    if (!ch && contextStart && close[contextStart]) {
      return {input: input, context: context, pos: pos, flag: eoinput};
    }

    // If there is no contextStart and no char left we are at the topLevel
    // and done reading.
    if (!ch && !close[contextStart]) return {
      input: input,
      context: context,
      pos: pos,
      flag: eoinput
    }

    // 3. whitespace
    if (/\s|,/.test(ch)) return {
      input: input.slice(1),
      context: context,
      pos: forward(pos, ch)
    };

    // 4. Various read rules
    if (readerSpecials.test(ch)) return readReaderSpecials(input, context, pos, xform);
    if (ch === ';')              return readComment(input, context, pos, xform);
    if (ch === '"')              return readString(input, context, pos, xform);
    if (ch === '\\')             return readChar(input, context, pos, xform);
    if (/[0-9]/.test(ch))        return readNumber(input, context, pos, xform);
    if (symRe.test(ch))          return readSymbol(input, context, pos, xform);

    // 5. list end?
    if (closing.indexOf(ch) > -1) {
      if (!contextStart) {
        var junk = readJunk(input, context, pos, xform);
        return {input: junk.input, context: junk.context, pos: junk.pos}
      }
      return {input: input, context: context, pos: pos, flag: eosexp}
    }

    //  6. list start?
    if (opening.indexOf(ch) > -1) {
      var startPos = clonePos(pos),
          nested = readSeq(ch, input.slice(1), Object.freeze([]), forward(pos, ch), xform),
          nextCh = nested.input[0],
          brackets = {open: ch, close: close[ch]};

      var sexp, endPos;
      if (nextCh !== close[ch]) {
        var errPos = clonePos(nested.pos),
            errMsg = "Expected '" + close[ch] + "'"
                   + (nextCh ? " but got '" + nextCh + "'" :
                      " but reached end of input"),
            children = nested.context,
            err = readError(errMsg, startPos, errPos, children);
        sexp = callTransform(xform, "error", err, startPos, errPos, brackets);
        endPos = nextCh ? forward(nested.pos, nextCh) : nested.pos;
      } else {
        endPos = nextCh ? forward(nested.pos, nextCh) : nested.pos;
        sexp = callTransform(xform, "list", nested.context, startPos, endPos, brackets);
      }

      context = context.concat([sexp]);
      var restInput = nested.input.slice(nextCh ? 1 : 0);

      return {input: restInput, context: context, pos: endPos}
    }

    // If we are here, either there is a char not covered by the sexp reader
    // rules or we are toplevel and encountered garbage
    var startPos = clonePos(pos), errPos = forward(pos, ch);
    var err = readError("Unexpected character: " + ch, startPos, errPos, null);
    err = callTransform(xform, "error", err, startPos, errPos);
    context = context.concat([err]);
    return {input: input.slice(1), context: context, pos: errPos};
  }

  function readSeq(contextStart, input, context, pos, xform) {
    var result, counter = 0;
    while (true) {
      var startRow = pos.row, startCol = pos.column;
      result = readSexp(contextStart, input, context, pos, xform);
      input = result.input; context = result.context; pos = result.pos;
      var endReached = result.flag === eoinput || (result.flag === eosexp && (contextStart || !input.length));
      if (!endReached && pos.row <= startRow && pos.column <= startCol)
        throw new Error("paredit reader cannot go forward at " + printPos(pos) + " with input " + input);
      if (endReached) break;

      // if (result.flag === eosexp && !contextStart)
      //   result = readJunk(input, context, pos, xform);
      // input = result.input; context = result.context; pos = result.pos;
    };
    return {input: input, context: context, pos: pos};
  }

  function readString(input, context, pos, xform) {
    var escaped = false;
    var startPos = clonePos(pos);
    var string = input[0];
    pos = forward(pos, input[0]); input = input.slice(1);
    return takeWhile(input, pos, function(c) {
      if (!escaped && c === '"') return false;
      if (escaped) escaped = false
      else if (c === "\\") escaped = true;
      return true;
    }, function(read, rest, prevPos, newPos) {
      string = string + read + rest[0];
      newPos = forward(newPos, rest[0]); rest = rest.slice(1);
      var result = callTransform(xform, "string", string, startPos, newPos,
        {open: '"', close: '"'});
      context = context.concat([result]);
      return {pos:newPos,input:rest,context:context};
    });
  }

  function readChar(input, context, pos, xform) {
    // char like \x
    var prevPos = clonePos(pos),
        read = input.slice(0,2),
        newPos = forward(pos, read),
        result = callTransform(xform, "char", read, prevPos, newPos),
        rest = input.slice(2);
    context = context.concat([result]);
    return {pos:newPos, input:rest, context: context};
  }

  function readSymbol(input, context, pos, xform) {
    return takeWhile(input, pos,
      function(c) { return symRe.test(c); },
      function(read, rest, prevPos, newPos) {
        var result = callTransform(xform, "symbol", read, prevPos, newPos);
        context = context.concat([result]);
        return {pos: newPos,input:rest,context:context};
      });
  }

  function readNumber(input, context, pos, xform) {
    return takeWhile(input, pos,
      function(c) { return /[0-9]/.test(c); },
      function(read, rest, prevPos, newPos) {
        var result = callTransform(xform, "number", Number(read), prevPos, newPos);
        context = context.concat([result])
        return {pos:newPos,input:rest,context:context};
      });
  }

  function readComment(input, context, pos, xform) {
    var prevPos = clonePos(pos),
        comment = "", rest = input;
    while (rest.length && /^\s*;/.test(rest)) {
      var read = readline(rest);
      comment += read[0];
      rest = read[1];
    }
    var newPos = forward(pos, comment),
        result = callTransform(xform, "comment", comment, prevPos, newPos);
    context = context.concat([result]);
    return {pos: newPos, input:rest, context:context};
  }

  function readReaderSpecials(input, context, pos, xform) {
    var prevPos = clonePos(pos),
        read = input.slice(0,1),
        newPos = forward(pos, read),
        result = callTransform(xform, "special", read, prevPos, newPos),
        rest = input.slice(1);
    context = context.concat([result]);
    return {pos:newPos, input:rest, context: context};
  }

  function readJunk(input, context, pos, xform) {
    return takeWhile(input, pos,
      // FIXME: there can be other junk except closing parens...
      function(c) { return closing.indexOf(c) > -1; },
      function(read, rest, prevPos, newPos) {
        var err = readError("Unexpected input: '" + read + "'", prevPos, newPos, null);
        var result = callTransform(xform, "error", err, prevPos, newPos);
        context = context.concat([result]);
        return {pos: newPos,input:rest,context:context};
      });
  }

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  function readError(msg, startPos, endPos, children) {
    return {
      error: msg + " at line "
          + (endPos.row+1) + " column " + endPos.column,
      start: clonePos(startPos), end: clonePos(endPos),
      children: children
    }
  }

  function callTransform(xform, type, read, start, end, args) {
    return xform ? xform(type, read, clonePos(start), clonePos(end), args) : read;
  }

  function takeWhile(string, pos, fun, withResultDo) {
    var startPos = clonePos(pos), result = "";
    for (var i = 0; i < string.length; i++) {
      if (fun(string[i])) result += string[i];
      else break;
    }
    return withResultDo(
      result, string.slice(result.length),
      startPos, forward(pos, result));
  }

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  // position helpers

  function startPos() { return {idx: 0, column: 0, row: 0}; }

  function clonePos(pos) { return {idx: pos.idx, column: pos.column, row: pos.row}; }

  function printPos(pos) { return JSON.stringify(pos); }

  function readline(input) {
    var endIdx = input.indexOf("\n");
    endIdx = endIdx > -1 ? endIdx+1 : input.length;
    var read = input.slice(0, endIdx);
    var rest = input.slice(endIdx);
    return [read, rest];
  }

  function forward(pos, read) {
    // note: pos is deliberately transient for performance
    if (!read) return pos;
    pos.idx += read.length;
    var lines = read.split("\n");
    var ll = lines.length;
    pos.row += ll-1;
    var lastRowL = lines[ll-1].length;
    pos.column = ll > 1 ? lastRowL : pos.column + lastRowL;
    return pos;
  }

});

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

/*global window, process, global,module*/

"format global";

;(function(run) {
  var isNodejs = typeof module !== "undefined" && module.exports;
  var exports = isNodejs ? module.exports : window.paredit;
  var util = isNodejs ? require('./util').util : window.paredit.util;
  var nav = isNodejs ? require("./navigator").navigator : window.paredit.navigator;
  var w = isNodejs ? require("./navigator").walk : window.paredit.walk;
  run(nav, w, util, exports);

})(function(nav, w, util, exports) {

  // (map (comp name first) (seq clojure.lang.Compiler/specials))
  exports.specialForms = [
    "&", "monitor-exit", /^case/, "try", /^reify/, "finally", /^(.*-)?loop/, /^do/,
    /^let/, /^import/, "new", /^deftype/, /^let/, "fn", "recur", /^set.*!$/,
    ".", "var", "quote", "catch", "throw", "monitor-enter",

    'ns', 'in-ns', /^([^\/]+\/)?def/,/^if/,/^when/,/^unless/,/->/, "while", "for",
    /(^|\/)with/, "testing", "while", "cond", "condp", "apply",
    "binding", "locking", "proxy", "reify", /^extend/,

    // midje
    "facts"];

  var ed = exports.editor = {

    rewrite: function(ast, nodeToReplace, newNodes) {
      var indexOffset = newNodes.length ?
        last(newNodes).end - nodeToReplace.end :
        nodeToReplace.start - nodeToReplace.end;

      var parents = w.containingSexpsAt(ast, nodeToReplace.start);

      // Starting from the parent of the nodeToReplace: construct new
      // parents out of the changed child, bottom up
      // With that we don't need to modify the exising AST. Note that during
      // this recursive construction we need to update the children to the "right"
      // of the modification
      var replaced = parents.reduceRight(function(replacement, parent) {
        var idxInParent = parent.children.indexOf(replacement.original);
        var childList;

        if (idxInParent > -1) {
          childList = parent.children.slice(0,idxInParent)
            .concat(replacement.nodes)
            .concat(parent.children.slice(idxInParent+1)
              .map(moveNode.bind(null,indexOffset)));
        } else childList = parent.children;

        var newParent = util.merge(parent, {
          end: parent.end+indexOffset,
          children: childList
        });

        return {original: parent, nodes: [newParent]};
      }, {original: nodeToReplace, nodes: newNodes});

      return replaced.nodes[0];
    },

    openList: function(ast, src, idx, args) {
      args = args || {};
      var count = args.count || 1;
      var open = args.open || '(', close = args.close || ')';

      if (args.freeEdits || ast.errors && ast.errors.length) return {
        changes: [["insert", idx, open]],
        newIndex: idx+open.length
      }

      var containing = w.containingSexpsAt(ast, idx);
      var l = last(containing);
      if (l && l.type === "comment" || l.type === "string")
          return {changes: [["insert", idx, open]], newIndex: idx+open.length}

      if (!args.endIdx) { // not a selection range
        return {changes: [["insert", idx, open+close]], newIndex: idx+open.length}
      }


      var parentStart = last(w.containingSexpsAt(ast, idx, w.hasChildren));
      var parentEnd = last(w.containingSexpsAt(ast, args.endIdx, w.hasChildren));

      // does selection span multiple expressions? collapse selection
      // var left = parentEnd.children.filter(function(ea) { return ea.end <= pos; });
      // var right = parentEnd.children.filter(function(ea) { return pos <= ea.start; });

      if (parentStart !== parentEnd) {
        return {changes: [["insert", idx, open+close]], newIndex: idx+open.length}
      }

      var inStart = parentEnd.children.filter(function(ea) {
            return ea.start < idx && idx < ea.end ; }),
          inEnd = parentEnd.children.filter(function(ea) {
            return ea.start < args.endIdx && args.endIdx < ea.end ; }),
          moveStart = inStart[0] && inStart[0] !== inEnd[0]
                   && (inEnd[0] || inStart[0].type !== 'symbol'),
          moveEnd = inEnd[0] && inStart[0] !== inEnd[0]
                 && (inStart[0] || inEnd[0].type !== 'symbol'),
          insertOpenAt = moveStart ? inStart[0].end : idx,
          insertCloseAt = moveEnd ? inEnd[0].start : args.endIdx;

      return {
        changes: [["insert", insertCloseAt, close],
                  ["insert", insertOpenAt, open]],
        newIndex: insertOpenAt+open.length
      };

    },

    spliceSexp: function(ast, src, idx) {
      var sexps = w.containingSexpsAt(ast,idx,w.hasChildren);
      if (!sexps.length) return null;
      var parent = sexps.pop();
      var onTop = parent.type === "toplevel";

      var insideSexp = parent.children.filter(function(n) {
        return n.start < idx && idx < n.end; })[0];
      var insideString = insideSexp && insideSexp.type === 'string';


      var changes = [], newIndex = idx;

      if (!onTop) changes.push(['remove', parent.end-1, parent.close.length]);
      if (insideString) {
        changes.push(['remove', insideSexp.end-1, insideSexp.close.length]);
        changes.push(['remove', insideSexp.start, insideSexp.open.length]);
        newIndex -= insideSexp.open.length;
      }
      if (!onTop) {
        changes.push(['remove', parent.start, parent.open.length]);
        newIndex -= parent.open.length;
      }

      return {changes: changes, newIndex: newIndex};
    },

    spliceSexpKill: function(ast, src, idx, args) {
      args = args || {}
      var count = args.count || 1;
      var backward = args.backward;

      var sexps = w.containingSexpsAt(ast,idx,w.hasChildren);
      if (!sexps.length) return null;

      if (backward) {
        var left = leftSiblings(last(sexps), idx);
        var killed = ed.killSexp(ast, src, idx/*last(left).end*/,
          {count: left.length, backward: true})
      } else {
        var right = rightSiblings(last(sexps), idx);
        var killed = ed.killSexp(ast, src, idx/*last(right).end*/,
          {count: right.length, backward: false})
      }

      var spliced = ed.spliceSexp(ast,src,idx);

      if (!killed) return spliced;
      if (!spliced) return killed;

      var changes = Array.prototype.slice.call(spliced.changes);
      if (changes.length === 2) changes.splice(1,0,killed.changes[0])
      else if (changes.length === 4) changes.splice(2,0,killed.changes[0])

      return {
        changes: changes,
        newIndex: killed.newIndex-(changes.length === 3 ? 1 : 2)
      }

    },

    splitSexp: function(ast, src, idx) {
      var sexps = w.containingSexpsAt(ast,idx);
      if (!sexps.length) return null;
      var sexp = sexps.pop();
      if (sexp.type === "toplevel") return;
      if (!w.hasChildren(sexp) && sexp.type !== "string")
        return null;
      // we are dealing with a list or string split
      var insertion = sexp.close+" "+sexp.open,
          newIndex = idx+sexp.close.length,
          changes = [['insert', idx, insertion]];
      return {changes: changes, newIndex: newIndex};
    },

    killSexp: function(ast, src, idx, args) {
      args = args || {}
      var count = args.count || 1;
      var backward = args.backward;
      var sexps = w.containingSexpsAt(ast,idx, w.hasChildren);
      if (!sexps.length) return null;
      var parent = sexps.pop();

      var insideSexp = parent.children.filter(function(n) {
        return n.start < idx && idx < n.end; })[0];

      if (insideSexp) {
        var from = backward ? insideSexp.start : idx;
        var to = backward ? idx : insideSexp.end;
        if (insideSexp.type === 'string') {
          from += backward ? insideSexp.open.length : 0;
          to += backward ? 0 : -insideSexp.close.length;
        }
        return {
          changes: [['remove', from, to-from]],
          newIndex: from
        }
      }

      if (insideSexp && insideSexp.type === 'string') {
        var from = backward ? insideSexp.start+insideSexp.open.length : idx;
        var to = backward ? idx : insideSexp.end-insideSexp.close.length;
        return {
          changes: [['remove', from, to-from]],
          newIndex: from
        }
      }

      if (backward) {
        var left = leftSiblings(parent, idx);
        if (!left.length) return null;
        var remStart = left.slice(-count)[0].start;
        var changes = [['remove', remStart, idx-remStart]];
        var newIndex = remStart;
      } else {
        var right = rightSiblings(parent, idx);
        if (!right.length) return null;
        var newIndex = idx;
        var changes = [['remove', idx, last(right.slice(0,count)).end-idx]];
      }

      return {changes: changes, newIndex: newIndex};
    },

    wrapAround: function(ast, src, idx, wrapWithStart, wrapWithEnd, args) {
      var count = (args && args.count) || 1;
      var sexps = w.containingSexpsAt(ast,idx, w.hasChildren);
      if (!sexps.length) return null;
      var parent = last(sexps);
      var sexpsToWrap = parent.children.filter(function(c) {
        return c.start >= idx; }).slice(0,count);
      var end = last(sexpsToWrap);
      var changes = [
        ['insert', idx, wrapWithStart],
        ['insert', (end ? end.end : idx) + wrapWithStart.length, wrapWithEnd]];
      return {changes: changes, newIndex: idx+wrapWithStart.length};
    },

    closeAndNewline: function(ast, src, idx, close) {
      close = close || ")"
      var sexps = w.containingSexpsAt(ast,idx, function(n) {
        return w.hasChildren(n) && n.close === close; });
      if (!sexps.length) return null;
      var parent = last(sexps),
          newlineIndent = times(rowColumnOfIndex(src, parent.start), ' '),
          insertion = "\n"+newlineIndent;
      var changes = [
        ['insert', parent.end, insertion]];
      return {changes: changes, newIndex: parent.end+insertion.length};
    },

    barfSexp: function(ast, src, idx, args) {
      var backward = args && args.backward;
      var sexps = w.containingSexpsAt(ast,idx, w.hasChildren);
      if (!sexps.length) return null;
      var parent = last(sexps), inner = last(w.containingSexpsAt(ast,idx));
      if (inner === parent) inner = null;
      if (backward) {
        var left = leftSiblings(parent, idx);
        if (!left.length) return null;
        var changes = [
          ['insert', left[1] ? left[1].start : (inner ? inner.start : idx), parent.open],
          ['remove', parent.start, parent.open.length]];
      } else {
        var right = rightSiblings(parent, idx);
        if (!right.length) return;
        var changes = [
          ['remove', parent.end-parent.close.length, parent.close.length],
          ['insert', right[right.length-2] ? right[right.length-2].end : (inner ? inner.end : idx), parent.close]];
      }
      return {changes: changes, newIndex: idx};
    },

    slurpSexp: function(ast, src, idx, args) {
      var backward = args && args.backward;
      var count = args.count || 1;
      var sexps = w.containingSexpsAt(ast,idx, w.hasChildren);
      if (sexps.length < 2) return null;
      var parent = sexps.pop();
      var parentParent = sexps.pop();
      if (backward) {
        var left = leftSiblings(parentParent, idx);
        if (!left.length) return;
        var changes = [
          ['remove', parent.start, parent.open.length],
          ['insert', left.slice(-count)[0].start, parent.open]];
      } else {
        var right = rightSiblings(parentParent, idx);
        if (!right.length) return;
        var changes = [
          ['insert', last(right.slice(0,count)).end, parent.close],
          ['remove', parent.end-parent.close.length, parent.close.length]];
      }
      return {changes: changes, newIndex: idx};
    },

    transpose: function(ast,src,idx,args) {
      args = args || {};

      var outerSexps = w.containingSexpsAt(ast, idx, w.hasChildren),
          parent = last(outerSexps),
          left = leftSiblings(parent, idx),
          right = rightSiblings(parent, idx);

      // if "inside" a leaf node do nothing
      if (parent.children.some(function(n) {
          return n.start < idx && idx < n.end; })) return null;
      // nothing there to transpose...
      if (!left.length || !right.length) return null;

      var l = last(left), r = right[0],
          insertion = src.slice(l.end, r.start) + w.source(src, l);

      return {
        changes: [
          ['insert', r.end, insertion],
          ['remove', l.start, r.start-l.start]],
        newIndex: idx - (l.end-l.start)+(r.end-r.start)
      };

    },

    delete: function(ast,src,idx,args) {
      args = args || {};

      var count = args.count || 1,
          backward = !!args.backward,
          endIdx = args.endIdx; // for text ranges

      if (args.freeEdits || ast.errors && ast.errors.length) {
        return endIdx ? {
          changes: [["remove", idx, endIdx-idx]],
          newIndex: idx
        } : {
          changes: [["remove", backward ? idx-count : idx, count]],
          newIndex: backward ? idx-count : idx
        }
      }


      var outerSexps = w.containingSexpsAt(ast, idx),
          outerLists = outerSexps.filter(function(n) { return w.hasChildren(n); }),
          parent = last(outerLists), sexp = last(outerSexps);

      var deleteRange = typeof endIdx === "number";
      if (deleteRange) {
        var endParent = last(w.containingSexpsAt(ast, endIdx, w.hasChildren));
        if (parent !== endParent) return null;
        var insideNodeStart = last(w.sexpsAt(parent, idx));
        var insideNodeEnd = last(w.sexpsAt(parent, endIdx));

        // don't delete only one " of strings
        var atStartOfUnsaveDelete = !isSaveToPartialDelete(insideNodeStart) && insideNodeStart.start === idx;
        var atEndOfUnsaveDelete = !isSaveToPartialDelete(insideNodeEnd) && insideNodeEnd.end === endIdx;
        if (insideNodeStart === insideNodeEnd
         && ((atStartOfUnsaveDelete && !atEndOfUnsaveDelete)
          || (!atStartOfUnsaveDelete && atEndOfUnsaveDelete))) return null;
        // if (!isSaveToPartialDelete(insideNodeStart) && insideNodeStart.start === idx) return null;
        // if (!isSaveToPartialDelete(insideNodeEnd) && insideNodeEnd.end === endIdx) return null;
        if (((insideNodeEnd !== parent && !isSaveToPartialDelete(insideNodeEnd) && !atEndOfUnsaveDelete)
         || (insideNodeStart !== parent && !isSaveToPartialDelete(insideNodeStart) && !atStartOfUnsaveDelete))
         && insideNodeStart !== insideNodeEnd) return null;
        if ((parent.children.indexOf(insideNodeStart) === -1 && insideNodeStart !== parent)
         || (parent.children.indexOf(insideNodeEnd) === -1 && insideNodeEnd !== parent)) return null;
        var delStart = Math.min(idx, endIdx),
            delEnd = Math.max(idx, endIdx);
        return {changes: [['remove', delStart, delEnd-delStart]], newIndex: delStart}
      }

      var isInList = parent === sexp,
          left = isInList && leftSiblings(parent, idx),
          right = isInList && rightSiblings(parent, idx),
          noDelete = {changes: [], newIndex: idx},
          simpleDelete = {
            changes: [['remove', backward ? idx-count : idx, count]],
            newIndex: backward ? idx-count : idx
          },
          changes = [], newIndex = idx;

      if (!isInList && sexp.type === 'comment') return simpleDelete;

      if (left && left.length && backward) {
        var n = last(left);
        if (n.end !== idx || isSaveToPartialDelete(n)) return simpleDelete;
        if (isEmpty(n) || n.type === "char") return deleteSexp(n);
        return noDelete;
      }

      if (right && right.length && !backward) {
        var n = right[0];
        if (n.start !== idx || isSaveToPartialDelete(n)) return simpleDelete;
        if (isEmpty(n) || n.type === "char") return deleteSexp(n);
        return noDelete;
      }

      if (!isInList) parent = sexp;

      var atStart = idx === parent.start+(parent.open ? parent.open.length : 0);
      var atEnd = idx === parent.end-(parent.close ? parent.close.length : 0);
      if ((!parent.children || !parent.children.length)
       && ((atStart && backward) || (atEnd && !backward))) {
        return deleteSexp(parent);
      }

      if (atStart && backward && (isInList ? parent.children.length : parent.end-parent.start > 1))
        return noDelete;
      if (atEnd && !backward && (isInList ? parent.children.length : parent.end-parent.start > 1))
        return noDelete;

      return simpleDelete;

      // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      function isEmpty(sexp) {
        return (sexp.type === 'string' || sexp.type === "list")
            && sexp.end-sexp.start === sexp.open.length+sexp.close.length
      }
      function deleteSexp(sexp) {
        return {
          changes: [['remove', sexp.start, sexp.end-sexp.start]],
          newIndex: sexp.start}
      }
      function isSaveToPartialDelete(n) {
        return n.type === 'symbol'|| n.type === 'comment' || n.type === 'number'
            || n.type === 'special';
      }
    },

    indentRange: function(ast, src, start, end) {
      var startLineIdx = rowStartIndex(src, start),
          endLineIdx = src.slice(end).indexOf("\n");
      endLineIdx = endLineIdx > -1 ? endLineIdx+end : src.length;

      var linesToIndent = src.slice(startLineIdx, endLineIdx).split("\n");

      return linesToIndent.reduce(function(indent, line) {
        var idx = indent.idx,
            changes = indent.changes,
            ast = indent.ast,
            src = indent.src;

        var outerSexps = w.containingSexpsAt(ast, idx, w.hasChildren),
            parent = last(outerSexps),
            sexpAtBol = parent && last(w.sexpsAt(ast, idx));

        if (!parent) return {
          idx: idx+line.length+1,
          newIndex: idx,
          changes:changes, ast:ast, src: src
        };

        // whitespace at bol that needs to be "removed"
        var ws = line.match(/^\s*/)[0],
            // figure out much whitespace we need to add
            indentOffset = sexpAtBol
                        && sexpAtBol.type === 'string'
                        && idx > sexpAtBol.start ?
                          0 : computeIndentOffset(src, parent, idx) - ws.length,
            lineLength = line.length + indentOffset;

        // record what needs to be changed and update source
        if (indentOffset > 0) {
          var insert = times(indentOffset, " ");
          changes.push(["insert", idx, insert]);
          src = src.slice(0,idx) + insert + src.slice(idx);
        } else if (indentOffset < 0) {
          changes.push(["remove", idx, -indentOffset]);
          src = src.slice(0,idx) + src.slice(idx-indentOffset);
        }

        // also update the ast: "move" the next node to the right accordingly,
        // "update" the entire ast
        var right = rightSiblings(parent, idx)[0];
        if (right) {
          var indentedRight = moveNode(indentOffset, right);
          ast = ed.rewrite(ast, right, [indentedRight]);
        } else {
          // if no siblings, udpdate the end of the list node
          ast = ed.rewrite(ast, parent,
            [util.merge(parent, {end: parent.end+indentOffset})]);
        }

        return {
          idx: idx + lineLength + 1, /*newline*/
          newIndex: idx + indentOffset, // for cursor placement
          changes: changes, ast: ast, src: src
        }
      }, {idx: startLineIdx, changes: [], ast: ast, src: src});
    }

  };


  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  // positioning helpers

  function rowStartIndex(src, idx) { return src.slice(0,idx).lastIndexOf("\n")+1; }

  function rowColumnOfIndex(src, idx) { return idx - rowStartIndex(src,idx); }

  function computeIndentOffset(src, parentSexp, idx) {
    if (parentSexp.type === 'toplevel') return 0;
    var left = leftSiblings(parentSexp, idx);
    if (isSpecialForm(parentSexp, src)) return rowColumnOfIndex(src, parentSexp.start + parentSexp.open.length+1);
    if (left.length <= 1 || parentSexp.open !== "(")
      return rowColumnOfIndex(src, parentSexp.start + parentSexp.open.length);
    return rowColumnOfIndex(src, left[1].start);
  }

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  // lang helper

  function last(a) { return a[a.length-1]; };

  function times(n, ch) { return new Array(n+1).join(ch); }


  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  // ast helpers
  function moveNode(offset, n) {
    // changes start/end of node and its children
    return util.mapTree(n,
      function(n, children) {
        return util.merge(n, {
          start: n.start+offset,
          end: n.end+offset,
          children: children
        });
      }, function(n) { return n.children; });
  }

  function leftSiblings(parentNode, idx) {
    return parentNode.children.filter(function(n) {
      return n.end <= idx; });
  }

  function rightSiblings(parentNode, idx) {
    return parentNode.children.filter(function(n) {
      return idx <= n.start; });
  }

  function isSpecialForm(parentSexp, src) {
    if (!w.hasChildren(parentSexp) || !parentSexp.children.length) return false;
    var srcOfFirstNode = parentSexp.children[0].source;
    if (!srcOfFirstNode) return false;
    return exports.specialForms.some(function(f) {
      if (typeof f === "string") return f === srcOfFirstNode;
      else if (typeof f === "function") return f(srcOfFirstNode, parentSexp.children[0]);
      else if (f instanceof RegExp) return f.test(srcOfFirstNode);
      else return false;
    });
  }

});
