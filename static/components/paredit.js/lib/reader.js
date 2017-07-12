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
