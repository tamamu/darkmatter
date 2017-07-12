# ace.improved

This package extends the [ace editor](http://ace.c9.io/) with useful functionality such as:

- helpers to lookup, install and remove key bindings
- code markers
- extended editor selection interface
- ast based editor commands

## Setup

In your webpage load ace and then include whatever parts of ace.improved you need:

```html
<script type="text/javascript" src="http://cdn.jsdelivr.net/ace/1.2.3/noconflict/ace.js"></script> 
<script src="ace.improved/lib/ace.improvements.js"></script>
<script src="ace.improved/lib/ace.ext.keys.js"></script>
<script src="ace.improved/lib/ace.ext.lang.codemarker.js"></script>
<!--...-->
```

## Interface

### Editor object

- `editor.posToIndex(pos) => index`

  Converts `pos`, a `{column: NUMBER, line: NUMBER}` object, into the string
  `index` / offset of `editor.getValue()`.

- `editor.idxToPos(index) => pos`

  The reverse of `posToIndex`.

- `editor.getCursorIndex() => index`

  Index of the current cursor position.

- `editor.setSelection(...) => Range`

  - `editor.setSelection(rangeString)`

    Sets the selection range via a descriptive string, such as
    `editor.setSelection("[0/2]->[0/4]")` or
    `editor.setSelection("[0/4]->[0/2]")`. The first number designates the row,
    the second the column.

  - `editor.setSelection(startIndex, endIndex)`

    Sets the selection range via a start and end index into the
    `editor.getValue()` string, example: `ed.setSelection(3,10)`.

  - `editor.setSelection(range)`

    Also accepts a ace range object:
    ```js
    var Range = ace.require("ace/range").Range;
    var r = new Range(1,2,3, 10);
    editor.setSelection(r)
    ```

- `editor.addSelection(...) => Range`

  Like `setSelection` but for multiple selection ranges.

- `editor.saveExcursion(doFunction)`

  Remembers the current selection state and calls `doFunction` with one
  argument, a reset function. `doFunction` can modify the selection state and
  is able to revert all selection changes by calling the reset function in turn.
  Example:
  ```js
  editor.selectAll()
  editor.saveExcursion(function(resetFunc) {
    editor.setSelection("[0/0]->[0/10]");
    setTimeout(function() { editor.setSelection("[0/0]->[0/10]"); }, 500);
    setTimeout(function() { editor.setSelection("[1/0]->[1/10]"); }, 1000);
    setTimeout(function() { editor.setSelection("[2/0]->[2/10]"); }, 1500);
    setTimeout(resetFunc, 2000);
  });
  ```

### ace.ext.keys

Some helpers around key handling: Simulating pressing keys and key sequences,
looking up key bindings, easily adding and removing new key bindings.

- `ace.ext.keys.lookupKeys(editor, keyString) => Command`

  Simulates inputing `keyString` into editor and records the editor command
  this invokes without running the command. Returns the command.
  (Editor commands are defined in `editor.keyBinding.$handlers`)

  Example:

  ```js
  ace.ext.keys.lookupKeys(ed, "Alt-Left")
  // => {
  //   name: "gotowordleft",
  //   bindKey: {mac: "Option-Left", win: "Ctrl-Left"},
  //   exec: function(e) {/*...*/},
  //   ...
  // }
  ```

  If you want to implement an emacs-like _describe-key_ behavior you can use
  `ace.ext.keyscaptureEditorCommand(editor, captureCommandFunc)` which is the
  foo behind the lookupKeys method. This little incantation lets you do that:

  ```js
  var keyLib = ace.require("ace/lib/keys");
  var lastKeys = [];
  var uninstallCapture = ace.ext.keys.captureEditorCommand(editor,
    function(cmd) {
      console.log(`Pressing ${lastKeys.join(" ")}  invokes ${cmd.name}`);
      uninstallCapture();
    },
    function(hashId, keyString, keyCode, event) {
      if ((hashId in keyLib.KEY_MODS && keyCode === -1)
       || [16,17,18,91,93,224].indexOf(keyCode) > -1) return; // Just a modifier being pressed
      lastKeys.push(keyLib.KEY_MODS[hashId] + keyString);
    });

  ```


- `ace.ext.keys.allEditorCommands(editor) => commandMap`

  Searches through all handlers and key bindings and returns a complete map of
  all commands in `editor`.
  Particularly, returns a JS object that maps command names to lists of command
  objects of the form `{cmd: {/*...*/}, cmdName: "golineup", key: "up"}`. For
  example `ace.ext.keys.allEditorCommands(editor).golineup` returns
  ```
  [{
    bindings: {/*...*/},
    cmdName: "golineup",
    key: "up"
  },{
    cmd: [/*...*/],
    cmdName: "golineup",
    key: "ctrl-p"
  }]
  ```

- `ace.ext.keys.simulateKey(editor, keyString)`

  Creates a keyboard event from `keyString` and runs it through `editor`,
  effectively simulating user key press(es). Some normalization will be
  applied, e.g. 'ctrl-A' and 'Control-A' is the same.
  Input "H": `ace.ext.keys.simulateKey(ed, 'H')`
  Select all (Mac OS): `ace.ext.keys.simulateKey(ed, 'Command-A')`
  Select all (Win / Linux): `ace.ext.keys.simulateKey(ed, 'Control-A')`

- `ace.ext.keys.simulateKeys(editor, keyString)`

  Splits `keyString` by spaces and then inputs ea part as with `simulateKey`.
  Processes both string input and command keys.
  Example, enters text, then select it:
  ```js
  ace.ext.keys.simulateKeys(ed, 'H e l l o \ W o r l d Command-a');
  ```

- `ace.ext.keys.addKeyCustomizationLayer(name, layerSpec)` and `ace.ext.keys.removeKeyCustomizationLayer(name)`

  A quick (and easily reversable) way to add key bindings to `editor`. For key
  customizations, temporary (e.g. mode dependant) key bindings etc. `name`
  is the the layer id and layerSpec is an object like

  ```
  {
    priority: NUMBER?,
    modes: [STRING]?,
    commandKeyBinding: {KEY_STRING: CMD_NAME, ...}
  }
  ```

  Example:
  ```js
  // First: add a new command
  ed.commands.addCommands([{name: "test-command", exec: function() { alert("test-command"); }}]);
  // Now bind a key to it
  ace.ext.keys.addKeyCustomizationLayer("test-layer", {priority: 10, commandKeyBinding: {"alt-t": "test-command"}});
  ```

  When you now press `alt-t` you should see a popup. Separating the binding
  from the commands allows to easily change and override bindings:

  ```js
  // Add a second command
  ed.commands.addCommands([{name: "test-command-2", exec: function() { alert("test-command-2"); }}]);
  // ... and bind it
  ace.ext.keys.addKeyCustomizationLayer("test-layer-2", {priority: 20, commandKeyBinding: {"alt-t": "test-command-2"}});
  ```

  Pressing `alt-t` now invokes the new command. To revert to the original behavior:

  ```js
  ace.ext.keys.removeKeyCustomizationLayer("test-layer");
  ace.ext.keys.removeKeyCustomizationLayer("test-layer-2");
  ```

<!---=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--->

### code markers

Easily highlight areas inside the editor.

Example, add a yellow marker to 'this' and 'test'

<!--document.getElementById("codemarker-css") && document.getElementById("codemarker-css").parentNode.removeChild(document.getElementById("codemarker-css"))-->
```js

// Ensure CSS
document.head.insertAdjacentHTML(
  "beforeend",
  `<style id="codemarker-css">
    .example-codemarker {
    	position: absolute;
    	border-radius: 3px;
    	background: rgba(204,204,0,0.7);
    }
  </style>`)

// set the text
ed.session.setValue("this\n is\na\n test")

// create a marker and highlight 'this' and 'test'
var marker = ace.ext.lang.codemarker.ensureIn(ed.session, "example-codemarker")
marker.set([{
  cssClassName: "example-codemarker",
  startPos: {row: 0, column: 0}, endPos: {row: 0, column: 4}
}, {
  cssClassName: "example-codemarker",
  startPos: {row: 3, column: 1}, endPos: {row: 3, column: 5}
}]);
```


<!---=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--->

### ast commands

<!--
Adds the editor commands

- forwardSexp
- backwardSexp
- backwardUpSexp
- forwardDownSexp
- markDefun
- expandRegion
- contractRegion
- gotoNextError
- gotoPrevErrorOrWarning

`ace.ext.lang.astCommands`
-->

_TODO_ add doc


<!---=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--->

### attributed text mode

```js
ed.session.setMode("ace/mode/text");
ed.session.getMode().set(ed, [
  ["Hello", {commands: [{name: 'oink', bindKey: 'enter'}]}]
]);
```

_TODO_ add doc


<!---=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--->

# Development

Run the tests by visiting [tests/run-tests.html](tests/run-tests.html).

<!---=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--->

# License

[MIT](LICENSE)
