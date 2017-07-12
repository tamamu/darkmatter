(function(ace) {

// imports
var Editor      = ace.require("ace/editor").Editor,
    oop         = ace.require("ace/lib/oop"),
    useragent   = ace.require("ace/lib/useragent"),
    keyLib      = ace.require("ace/lib/keys"),
    KeyBinding  = ace.require("ace/keyboard/keybinding").KeyBinding,
    HashHandler = ace.require("ace/keyboard/hash_handler").HashHandler;

// exports
ace.ext = ace.ext || {};
var exports = ace.ext.keys = ace.ext.keys || {};

exports.lookupKeys                      = lookupKeys;
exports.captureEditorCommand            = captureEditorCommand;
exports.allEditorCommands               = allEditorCommands;
exports.simulateKey                     = simulateKey;
exports.simulateKeys                    = simulateKeys;
exports.KeyHandlerForCustomizations     = KeyHandlerForCustomizations;
exports.addKeyCustomizationLayer        = addKeyCustomizationLayer;
exports.removeKeyCustomizationLayer     = removeKeyCustomizationLayer;
exports.removeAllKeyCustomizationLayers = removeAllKeyCustomizationLayers;
exports.getKeyCustomizationLayer        = getKeyCustomizationLayer;

// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

function nameOfCommand(cmd) {
  if (!cmd) return "no command!";
  if (Array.isArray(cmd)) return nameOfCommand(cmd[0]);
  if (typeof cmd === "string") return cmd;
  if (cmd.name) return cmd.name;
  if (cmd.command) return cmd.command;
  return "unknown command, no name";
}

function allEditorCommands(ed) {
  return flattenedCommandKeyBindings(allCommandKeyBindings(ed));
}

function flattenedCommandKeyBindings(allBindings) {
  var bindings = flatten(allBindings.map(keysAndCommandsOfCommandKeyBinding));
  return bindings.reduce(function(map, cmdBnd) {
    var existing = map[cmdBnd.cmdName];
    if (!existing) map[cmdBnd.cmdName] = [];
    var alreadyAdded = map[cmdBnd.cmdName].some(function(bound) {
      return bound.bindings === cmdBnd.bindings
          && bound.cmd === cmdBnd.cmd
          && bound.key === cmdBnd.key; });
    if (!alreadyAdded) map[cmdBnd.cmdName].push(cmdBnd);
    return map;
  }, {});
}

function keysAndCommandsOfCommandKeyBinding(b) {
  return Object.keys(b).map(function(k) {
      var cmd = b[k], keys = k.replace(/cmd-/i, "command-").toLowerCase();
      return {key: keys, cmd: cmd, cmdName: nameOfCommand(cmd), bindings: b}
  });
}

function allCommandKeyBindings(ed) {
  var custHandlers = ed.keyBinding["ace.ext.keys.customized"];
  var handlers = getCustomizationAwareKeyHandlers(ed, custHandlers);
  return [ed.commands.commandKeyBinding].concat(
    handlers.map(function(h) {
      if (!h.isEmacs) return h.commandKeyBinding;
      return Object.keys(h.commandKeyBinding).reduce(function(bnd, k) {
        var sanitized = k.replace(/cmd-/ig, "command-").replace(/c-/ig, "ctrl-").replace(/m-/ig, "alt-").replace(/s-/ig, "shift-").toLowerCase();
        bnd[sanitized] = h.commandKeyBinding[k];
        return bnd;
      }, {})
    }));
}

function convertKeysEmacsToGeneric(keys) {
  return (keys || "")
         .replace(/c-/gi, 'ctrl-')
         .replace(/m-/gi, 'alt-')
         .replace(/cmd-/gi, 'command-')
         .replace(/s-/gi, 'shift-');
}

function convertKeysGenericToEmacs(keys) {
  return (keys || "")
          .replace(/ctrl-/gi, "c-")
          .replace(/alt-/gi, "m-")
          .replace(/shift-/gi, "s-")
          .replace(/command-/gi, "cmd-");
}
// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// customized key bindings
// -=-=-=-=-=-=-=-=-=-=-=-=-

function KeyHandlerForCustomizations(bindings) {
  HashHandler.call(this, bindings || {});
  this.fixInputBindings();
}

oop.inherits(KeyHandlerForCustomizations, HashHandler);

(function() {

  this.takeOverEmacsBindings = function() {
    var emacs = ace.require("ace/keyboard/emacs");
    var emacsH = emacs && emacs.handler;
    if (!emacsH) return
    Object.keys(emacsH.commandKeyBinding).forEach(function(k) {
      var name = emacsH.commandKeyBinding[k].name || emacsH.commandKeyBinding[k].command || emacsH.commandKeyBinding[k],
          keys = convertKeysEmacsToGeneric(k);
      if (!this.commandKeyBinding[keys])
        this.bindKey(keys,name);
    }, this);
    this.addCommands(emacsH.commands);
    return this;
  };


  this.fixInputBindings = function() {
    // huh?
    // FIXME! some characters like ` can't be used in key combos b/c ace
    // escapes them strangely
    var newBnds = this.commandKeyBinding;
    Object.keys(newBnds).forEach(function(ea) {
      if (ea.match(/input/)) newBnds[ea.replace(/input/g, '')] = newBnds[ea];
    });
    return this;
  };

  this.handleKeyboard = function(data, hashId, keyString, keyCode) {
    var key = keyLib.KEY_MODS[hashId] + keyString;
    var cmd = this.commandKeyBinding[key];
    if (data.$keyChain) {
        cmd = this.commandKeyBinding[data.$keyChain + " " + key];
    }

    if (!cmd || cmd === 'null') return cmd;

    if (typeof cmd === 'object') {
      cmd = {command: cmd.name || cmd.command,
             args: cmd.args ? clone(cmd.args || cmd.args) : {}}
    } else cmd = {command: cmd, args: {}};

    if (!cmd.command || cmd.command === 'null') return cmd;
    if (cmd.command == "chainKeys" || cmd.command[cmd.command.length - 1] == "chainKeys") {
      if (data.$keyChain) data.$keyChain += " " + key;
      data.$keyChain = data.$keyChain || key;
      return {command: "null"};
    }

    // deactivate to not steal keyChain
    if (data.$keyChain && keyCode > 0 && cmd)
        data.$keyChain = "";

    if (keyLib.KEY_MODS[hashId].indexOf("shift-") > -1)
      cmd.args.shifted = true;

    if (data.count) {
      cmd.args.count = data.count;
      data.count = null;
    }

    return cmd;
  }

}).call(KeyHandlerForCustomizations.prototype);


function getCustomizationAwareKeyHandlers(ed, customHandlers) {
  var handlers = ed.keyBinding.$handlers;
  if (!customizedKeysEnabled(ed)) return handlers;
  if (customHandlers) {
    var modeId = ed.session.getMode().$id;
    if (modeId)
      customHandlers = customHandlers.filter(function(ea) {
        return !ea.bindingsSpec.modes || ea.bindingsSpec.modes.indexOf(modeId) > -1; });
    handlers = handlers.concat(customHandlers);
  }
  return handlers;
}

function customizedKeysEnabled(ed) {
  // dynamically checks if it is ok to apply key customizations. right now we
  // only check if isearch / occur / jump char behavior is active. This might be
  // extended from the outside
  var handlers = ed.keyBinding.$handlers;
  var last = handlers[handlers.length-1];
  if (last.$iSearch || last.isOccurHandler || last.isJumpChar || last.isIyGoToChar) return false;
  if (ed.completer && ed.completer.activated) return false;
  return true;
}

function patchData(data) {
  if (data["patched-by-ace.ext.keys"]) return;
  data["patched-by-ace.ext.keys"] = true;
  // keyChain vs $keycHain: emacs handler uses one, the rest uses the latter
  data.__defineGetter__("keyChain", function() { return convertKeysGenericToEmacs(this.$keyChain); });
  data.__defineSetter__("keyChain", function(v) { this.$keyChain = convertKeysEmacsToGeneric(v); return v; });
}

function ensureKeyBindingCustomization() {
  if (!KeyBinding.prototype.$callKeyboardHandlersUnmodified)
    KeyBinding.prototype.$callKeyboardHandlersUnmodified = KeyBinding.prototype.$callKeyboardHandlers;

  KeyBinding.prototype.$callKeyboardHandlers = function(hashId, keyString, keyCode, e) {
    patchData(this.$data);
    exports.$lastKeyChain = this.$data.$keyChain;
    var custHandlers = this["ace.ext.keys.customized"];
    this.$handlers = getCustomizationAwareKeyHandlers(this.$editor, custHandlers);
    try {
      var res = this.$callKeyboardHandlersUnmodified(hashId, keyString, keyCode, e);
      // if (res) exports.$lastKeyChain = "";
      return res;
    } finally {
      if (custHandlers) {
        this.$handlers = this.$handlers.filter(function(h) {
          return custHandlers.indexOf(h) === -1; });
      }
    }
  }
}

// KeyBinding.prototype["ace.ext.keys.customized"]
// this.aceEditor.keyBinding.$handlers
function getKeyCustomizationLayer(layerName) {
  var cust = KeyBinding.prototype["ace.ext.keys.customized"] || (KeyBinding.prototype["ace.ext.keys.customized"] = []);
  var found;
  for (var i = cust.length-1; i >= 0; i--) { if (cust[i].layerName === layerName) {found = cust[i]; break; }; }
  return found ? found.bindingsSpec : null;
}

function removeKeyCustomizationLayer(layerName) {
  var cust = KeyBinding.prototype["ace.ext.keys.customized"] || [];
  KeyBinding.prototype["ace.ext.keys.customized"] = cust.filter(function(h) {
    return h.layerName !== layerName; });
}

function removeAllKeyCustomizationLayers() {
  var layers = KeyBinding.prototype["ace.ext.keys.customized"];
  layers && layers.forEach(function(ea) {
    removeKeyCustomizationLayer(ea.layerName); });
}

function addKeyCustomizationLayer(layerName, bindingsSpec) {
  // ed.session
  ensureKeyBindingCustomization();
  var handler = bindingsSpec.keyHandler || new KeyHandlerForCustomizations();
  if (bindingsSpec.commandKeyBinding)
    handler.bindKeys(bindingsSpec.commandKeyBinding);
  handler.fixInputBindings && handler.fixInputBindings();
  handler.layerName = layerName;
  handler.bindingsSpec = bindingsSpec;

  var cust = KeyBinding.prototype["ace.ext.keys.customized"] || [];
  cust = cust.filter(function(h) { return h.layerName !== layerName; })
             .concat([handler])
             .sort(function(a,b) { return (a.bindingsSpec.priority || 0) - (b.bindingsSpec.priority || 0); });
  KeyBinding.prototype["ace.ext.keys.customized"] = cust;
}

// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

// Commands invoke through key actions or otherwise will be captured after
// running when while running doFunc and the result returned
// *instead* of triggering normal actions. This basically overwrites the
// typical ed.commands.exec handler
function captureEditorCommand(ed, captureCommandFunc, captureCallFunc) {
  var origCallKeyboardHandlers = ed.keyBinding.$callKeyboardHandlers,
      commandExecHandler = ed.commands.addEventListener('exec', function(e) {
        uninstall();
        e.stopPropagation(); e.preventDefault();
        captureCommandFunc && captureCommandFunc(e.command, e);
        return true;
      });

  var orig = ed.keyBinding.$callKeyboardHandlers;
  ed.keyBinding.$callKeyboardHandlers = function(hashId, keyString, keyCode, e) {
    captureCallFunc && captureCallFunc(hashId, keyString, keyCode, e);
    return orig.call(this, hashId, keyString, keyCode, e);
  };

  return uninstall;

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  function uninstall() {
    commandExecHandler && ed.commands.removeEventListener('exec', commandExecHandler);
    ed.keyBinding.$callKeyboardHandlers = orig;
  }

}

function lookupKeys(ed, keys) {
  var result, reset = captureEditorCommand(ed, lookupKeys_captureCommandFunc);
  simulateKey(ed, keys);
  reset();
  return result

  function lookupKeys_captureCommandFunc(cmd) { result = cmd; }
}

// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

var ts = 0;


var getModifierHash = useragent.isMac && useragent.isOpera && !("KeyboardEvent" in window)
    ? function(e) {
        return 0 | (e.metaKey ? 1 : 0) | (e.altKey ? 2 : 0) | (e.shiftKey ? 4 : 0) | (e.ctrlKey ? 8 : 0);
    }
    : function(e) {
        return 0 | (e.ctrlKey ? 1 : 0) | (e.altKey ? 2 : 0) | (e.shiftKey ? 4 : 0) | (e.metaKey ? 8 : 0);
    };

exports.getModifierString = function(e) {
    return keyLib.KEY_MODS[getModifierHash(e)];
};

/**
 * Computes the "hashId" that is determined by the modifiers pressed. Will call
 * `callback` only the event qualifies as a "command key".
 * Will mutate `pressedKeys`!
 *
 * @param {Function} callback - Callback that takes parameters event, hashId, keyCode
 * @param {KeyEvent} keydown or keypress event
 * @param {Number} character code of the key pressed
 * @param {Object} pressedKeys - Map of key codes to truthy values for keeping track which keys were pressed
 * @return {Boolean} return value that'll be returned by the keydown/keypress event handler
 *
 **/
exports.normalizeCommandKeys = function normalizeCommandKeys(callback, e, keyCode, pressedKeys) {
    var hashId = getModifierHash(e);

    if (!useragent.isMac && pressedKeys) {
        if (pressedKeys[91] || pressedKeys[92])
            hashId |= 8;
        if (pressedKeys.altGr) {
            if ((3 & hashId) != 3)
                pressedKeys.altGr = 0;
            else
                return;
        }
        if (keyCode === 18 || keyCode === 17) {
            var location = "location" in e ? e.location : e.keyLocation;
            if (keyCode === 17 && location === 1) {
                ts = e.timeStamp;
            } else if (keyCode === 18 && hashId === 3 && location === 2) {
                var dt = -ts;
                ts = e.timeStamp;
                dt += ts;
                if (dt < 3)
                    pressedKeys.altGr = true;
            }
        }
    }

    if (keyCode in keyLib.MODIFIER_KEYS) {
        switch (keyLib.MODIFIER_KEYS[keyCode]) {
            case "Alt":
                hashId = 2;
                break;
            case "Shift":
                hashId = 4;
                break;
            case "Ctrl":
                hashId = 1;
                break;
            default:
                hashId = 8;
                break;
        }
        keyCode = -1;
    }

    if (hashId & 8 && (keyCode === 91 || keyCode === 93)) {
        keyCode = -1;
    }

    if (!hashId && keyCode === 13) {
        var location = "location" in e ? e.location : e.keyLocation;
        if (location === 3) {
            callback(e, hashId, -keyCode);
            if (e.defaultPrevented)
                return;
        }
    }

    if (useragent.isChromeOS && hashId & 8) {
        callback(e, hashId, keyCode);
        if (e.defaultPrevented)
            return;
        else
            hashId &= ~8;
    }

    // If there is no hashId and the keyCode is not a function key, then
    // we don't call the callback as we don't handle a command key here
    // (it's a normal key/character input).
    // 2014-03-11 rksm: command key == mac cmd? If just cmd is pressed hashId
    // will be -1 and callback will be called... please clarify.
    if (!hashId && !(keyCode in keyLib.FUNCTION_KEYS) && !(keyCode in keyLib.PRINTABLE_KEYS)) {
        return false;
    }

    return callback(e, hashId, keyCode);
}

var keyCodeCache = {};
function getKeyCodeForKey(key, type) {
    // reverse mapping, key -> code
    key = key.toLowerCase();
    if (keyCodeCache[key]) return keyCodeCache[key];
    var base = keyLib;
    // "MODIFIER_KEYS","FUNCTION_KEYS","PRINTABLE_KEYS"
    if (type) base = keyLib[type];
    for (var code in base)
        if (key === base[code].toLowerCase())
            return keyCodeCache[key] = typeof code === 'string' ?
                parseInt(code, 10) : code;
}

exports.isFunctionKey = function(string) {
    return !!getKeyCodeForKey(string, 'FUNCTION_KEYS');
};

exports.isModifierKey = function(string) {
    return !!getKeyCodeForKey(string, 'MODIFIER_KEYS');
};

exports.isPrintableKey = function(string) {
    return !!getKeyCodeForKey(string, 'PRINTABLE_KEYS');
};

function keySpec(key) {
    // 1. create a key event object. We first gather what properties need to be
    // passed to the event creator in terms of the keyboard state

    var spec = {
        keyString: '',
        keyCode: 0,
        ctrlKey: false,
        shiftKey: false,
        altKey: false,
        metaKey: false,
        altGraphKey: false,
        isFunctionKey: false,
        isModified: false
    };

    // 2. Are any modifier keys pressed?
    var keyMods = key.split(/[\-]/);
    var trailing = keyMods.pop();
    var modsToEvent = {
        shift: "shiftKey",
        control: "ctrlKey",
        ctrl: "ctrlKey",
        alt: "altKey",
        meta: "metaKey",
        command: "metaKey",
        cmd: "metaKey"
    }
    keyMods.forEach(function(mod) {
        var modEventFlag = modsToEvent[mod.toLowerCase()];
        if (!modEventFlag) return;
        spec.isModified = true;
        spec[modEventFlag] = true;
    });

    // 3. determine the key code and key string of the event.
    spec.isFunctionKey = exports.isFunctionKey(trailing);
    if (spec.isFunctionKey) {
        spec.keyCode = getKeyCodeForKey(trailing, 'FUNCTION_KEYS');
    } else if (spec.isModified) {
        spec.keyCode = trailing.toUpperCase().charCodeAt(0);
    } else {
        spec.keyString = trailing;
    }

    return spec;
}

function simulateKey(editor, key) {
    var spec = keySpec(key);
    var e = exports.createKeyboardEvent(spec);

    if (spec.isFunctionKey || spec.isModified) {
        exports.normalizeCommandKeys(editor.onCommandKey.bind(editor), e, e.keyCode, {});
    }
    if (!e.defaultPrevented && spec.keyString) {
        editor.onTextInput(spec.keyString);
        if (editor.session.$syncInformUndoManager) // FIXME for vim
            editor.session.$syncInformUndoManager();
    }
};

/*
 * usage: keys.simulateKeys(editor, "h i  t h e e Left r")
 * (should print "hi there")
 */
function simulateKeys(editor, keysString) {
    // there can be multiple pressed keys separated by spaces. To simulate a
    // space press use a double space. split up the individual keys and
    // simulate each
    function ensureSpaces(s) { return s.length ? s : ' '; }
    var pressedKeys = keysString.length === 1 ?
        [keysString] :
        keysString.split(/ /g).map(ensureSpaces)
    pressedKeys.forEach(exports.simulateKey.bind(null,editor));
};

// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// eventing stuff
// -=-=-=-=-=-=-=-
function toUnicode(charCode) {
    var result = charCode.toString(16).toUpperCase();
    while (result.length < 4) result = '0' + result;
    return '\\u' + result;
}

exports.createKeyboardEvent = function(spec) {
    var evt = document.createEvent('KeyboardEvent'),
        id = spec.keyString ? toUnicode(spec.keyString.charCodeAt(0)) : "";

    function charCode() {
        return id.length ?
            String.fromCharCode(id.replace('\\u', '0x')).charCodeAt(0) :
            spec.keyCode;
    }

    function addProp(name, getter) {
        try { Object.defineProperty(evt, name, {get: getter}); } catch (e) {}
    }

    if (evt.initKeyboardEvent) { // webkit
        evt.initKeyboardEvent(
            "keydown",            // type
            true,                 // canBubble
            true,                 // cancelable
            document.defaultView, // view
            id,                   // keyIdentifier
            0,                    // keyLocation
            spec.ctrlKey,
            spec.altKey,
            spec.shiftKey,
            spec.metaKey,
            spec.altGraphKey);
    } else if (evt.initKeyEvent) { // gecko
        evt.initKeyEvent("keydown", true, true, document.defaultView,
            spec.ctrlKey, spec.altKey, spec.shiftKey, spec.metaKey,
            false, false, false, false, charCode(), id);
    } else { // jsdom
        evt._type = "keydown";
        evt._bubbles = true;
        evt._cancelable = true;
        evt._target = document.defaultView;
        evt._currentTarget = null;
        evt._keyLocation = 0;

        evt.defaultPrevented = false;
        evt.preventDefault = function() { evt.defaultPrevented = true; };
        evt.ctrlKey = spec.ctrlKey;
        evt.shiftKey = spec.shiftKey;
        evt.altKey = spec.altKey;
        evt.metaKey = spec.metaKey;
        evt.altGraphKey = spec.altGraphKey;
    }

    evt.keyCode || addProp("keyCode", charCode);
    evt.which || addProp("which", charCode);
    evt.keyIdentifier || addProp("keyIdentifier", function() { return id; });

    return evt;
};

// helpers

function clone(obj) {
  // Shallow copy
  if (Array.isArray(obj)) return Array.prototype.slice.call(obj);
  var clone = {};
  for (var name in obj) { clone[name] = obj[name]; }
  return clone;
}

function flatten(array, optDepth) {
  if (typeof optDepth === "number") {
    if (optDepth <= 0) return array;
    optDepth--;
  }
  return array.reduce(function (flattened, value) {
    return flattened.concat(Array.isArray(value) ? flatten(value, optDepth) : [value]);
  }, []);
}

})(window.ace);
