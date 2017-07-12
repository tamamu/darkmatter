(function(ace) {

var oop = ace.require("ace/lib/oop");

function CustomTokenizer(textAttrs) {
  this.textAttrs = textAttrs;
}

(function() {

  this.getLineTokens = function(line, state, row) {
    var tokens = (this.textAttrs[row]||[]).map(function(ea) {
      return ea.token; });
    return {state: "", tokens: tokens}
  }

}).call(CustomTokenizer.prototype);

// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// define a new mode
ace.define('ace/mode/attributedtext', function(require, exports, module) {
  var TextMode = require("ace/mode/text").Mode;

  var Mode = function() {
      this.$tokenizer = {
        getLineTokens: function() {
          return {state: "", tokens: []};
        }
      }
      // this.$behaviour = new Behaviour();
  };
  oop.inherits(Mode, TextMode);


  (function() {
    // this.type = "attributedtext";
    // this.getNextLineIndent = function(state, line, tab) { return ''; };
    this.$id = "ace/mode/attributedtext";

    this.attachToEditor = function(ed) {
      ed.once("changeMode", this.detach.bind(this, ed));
      ed.setReadOnly(true);
    };

    this.detach = function(ed) {
      if (!ed.session.$attributedtext) return;
      delete ed.session.$attributedtext.commands;
      if (ed.session.$attributedtext.keyhandler) {
        ed.keyBinding.removeKeyboardHandler(ed.session.$attributedtext.keyhandler);
        delete ed.session.$attributedtext.keyhandler;
        ed.keyBinding.setDefaultHandler(ed.commands);
      }
      ed.off("click", ed.session.$attributedtext.clickHandler);
      delete ed.session.$attributedtext.clickHandler;
      delete ed.session.$attributedtext;
    };

    this.initMouseHandler = function(ed) {
      var mode = this;
      if (ed.session.$attributedtext.clickHandler) return;
      ed.session.$attributedtext.clickHandler = function(evt) {
        // var pos = evt.$pos || ed.renderer.pixelToScreenCoordinates(evt.x,evt.y);
        var pos = evt.$pos || ed.renderer.screenToTextCoordinates(evt.x,evt.y)
        var attr = mode.textAttributeOfPosition(ed, pos);
        if (!attr || !attr.onClick) return;
        handleOnClick(ed, evt, attr);
      };
      ed.on("mouseup", ed.session.$attributedtext.clickHandler);
    }

    this.initKeyboardHandler = function(ed, spec) {
      // get the commands
      var cmds = spec.reduce(function(commands, specPart) {
        var attr = specPart[1];
        if (!attr || !attr.commands) return commands;
        return commands.concat(
          attr.commands.filter(function(c) {
            return commands.indexOf(c) === -1; }));
      }, ed.session.$attributedtext.commands || []);

      ed.session.$attributedtext.commands = cmds;

      // setup the key handler
      var h;
      if (!ed.session.$attributedtext.keyhandler) {
        h = ed.session.$attributedtext.keyhandler = new PositionAwareKeyHandler();
        ed.keyBinding.setDefaultHandler(h);
      } else { h = ed.session.$attributedtext.keyhandler; }

      h.addCommands(cmds);
    };

    this.textAttributeOfPosition = function(ed, pos) {
      var attrs = ed.session.$attributedtext && ed.session.$attributedtext.textAttrs;
      if (!attrs) return;
      var line = attrs[pos.row] || [];
      return line.reduce(function(akk, attr) {
        if (akk.found) return akk;
        akk.pos += attr.token.value.length;
        if (pos.column <= akk.pos) akk.found = attr;
        return akk;
      }, {pos: 0, found: null}).found;
    };

    this.set = function(ed, spec) {
      // spec like [["foo", "x"], ["bar", {type: "y"}]]
      var textAttrsPerLine = spec.reduce(function(lines, specPart) {
        return specPartLines(specPart).reduce(function(lines, specPart, i) {
          var lastLine = i > 0 ? [] : lines.pop();
          var type = specPart[1] && (typeof specPart[1] === "string" ?
            specPart[1] : specPart[1].type);
          var attr = oop.mixin({}, {token: newToken(specPart[0], type)});
          if (typeof specPart[1] === 'object') oop.mixin(attr, specPart[1]);
          lastLine.push(attr);
          lines.push(lastLine);
          return lines;
        }, lines);
      }, [[]]);

      var text = textAttrsPerLine.map(function(l) {
        return l.map(function(ea) { return ea.token.value; }).join("");
      }).join("\n");

      ed.session.setValue(text);
      ed.session.$attributedtext = ed.session.$attributedtext || {};
      ed.session.$attributedtext.textAttrs = textAttrsPerLine;

      this.initKeyboardHandler(ed, spec);
      this.initMouseHandler(ed);

      modifyBgTokenizer(ed.session, new CustomTokenizer(textAttrsPerLine));
    };
  }).call(Mode.prototype);

  exports.Mode = Mode;
});

ace.require("ace/edit_session").EditSession.prototype.$modes['ace/mode/attributedtext'] = new (ace.require("ace/mode/attributedtext")).Mode()

// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

// ace.require("ace/mode/attributedtext").Mode

function specPartLines(specPart) {
  // specPart like ["foo\nbar", attributes];
  return specPart[0].split("\n").map(function(s) {
    return [s, specPart[1]]; })
}

function newToken(value, type) {
  return {value: value || "", type: type || "text"}
}

function modifyBgTokenizer(session, tokenizer) {
  session.bgTokenizer.setTokenizer(tokenizer);
  session.bgTokenizer.setDocument(session.getDocument());
}

function handleOnClick(ed, evt, attr) {
  if (evt) {
    evt.stopPropagation && evt.stopPropagation();
    evt.preventDefault && evt.preventDefault();
  }
  if (typeof attr.onClick === "function") {
    return attr.onClick(evt, ed, attr);
  } else if (typeof attr.onClick === "string") {
    var cmd = ed.session.$attributedtext.keyhandler.commands[attr.onClick];
    cmd.exec(ed, {attr: attr, evt: evt});
  } else { /*...*/ }
}

// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// key control

var HashHandler = ace.require("ace/keyboard/hash_handler").HashHandler;

function PositionAwareKeyHandler() {
  HashHandler.call(this);
  // this.update();
}

oop.inherits(PositionAwareKeyHandler, HashHandler);

(function() {

  this.isAttributedtextKeyboardHandler = true;

  this.handleKeyboard = function(data, hashId, keyString, keyCode) {
      // ed = that.aceEditor
      // ed.keyBinding.$callKeyboardHandlers
      // ed.getKeyboardHandler().commandKeyBinding
      var cmd = HashHandler.prototype.handleKeyboard.call(this, data, hashId, keyString, keyCode);
      var attr = data.editor.session.getMode().textAttributeOfPosition(
        data.editor, data.editor.getCursorPosition());
      var stop = {command: "null"};

      if (!cmd || !cmd.command) {
        if (keyCode === 13 && attr.onClick) { // Enter
          handleOnClick(data.editor, null, attr);
          return stop;
        } else return cmd;
      }

      if (!attr || !attr.commands) return stop;
      if (attr.commands.indexOf(cmd.command) >= 0) return cmd;
      return stop;
  };

}).call(PositionAwareKeyHandler.prototype);

})(window.ace);
