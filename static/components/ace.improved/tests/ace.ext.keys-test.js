/*global ace, createEditor, expect, before, beforeEach, afterEach, describe, it*/

describe('ace.ext.keys', function() {

  var ed, keys = ace.ext.keys, platform, commandKey;
  beforeEach(function() {
    keys.removeAllKeyCustomizationLayers();
    ed = createEditor("this is a\ntest\n\ntext\n");
    platform = ed.commands.platform;
    commandKey = platform === 'mac' ? 'Command' : 'Control';
  });

  afterEach(function() {
    if (ed) {
      ed.destroy();
      ed.container.parentNode.removeChild(ed.container);
    }
    keys.removeAllKeyCustomizationLayers();
  });

  describe("find command for key", function() {

    it("finds commands bound in key handler", function() {
      var run = false;
      var cmd = {name: "test-command", bindKey: "Alt-t", exec: function() { run = true; }};
      var keyHandler = new (ace.require("ace/keyboard/hash_handler")).HashHandler([cmd]);
      ed.setKeyboardHandler(keyHandler);
      var result = ace.ext.keys.lookupKeys(ed, "Alt-t");
      expect(cmd).to.equal(result);
      expect(run).to.equal(false);
    });

    it("finds commands bound in commands", function() {
      var cmd = {name: "test-command", bindKey: "Alt-t", exec: function() {}};
      ed.commands.addCommands([cmd])
      var result = ace.ext.keys.lookupKeys(ed, "Alt-t");
      expect(cmd).to.equal(result);
    });

  });

  describe("key input simulation", function() {

    it("simulate simple input", function() {
        ed.setValue('foo\nbar'); ed.selection.clearSelection();
        ed.moveCursorToPosition({row: 1, column: 0});

        keys.simulateKey(ed, 'a');
        keys.simulateKey(ed, ' ');
        expect(ed).to.have.rangeAndContent(1,2,1,2, 'foo\na bar');
    });

    it("simulateKey select all", function() {
        ed.setValue('foo\nbar'); ed.moveCursorToPosition({row: 0, column: 0});
        ed.moveCursorToPosition({row: 1, column: 0});

        keys.simulateKey(ed, commandKey + '-a');
        expect(ed).to.have.rangeAndContent(0,0,1,3, 'foo\nbar');
    });

    it("simulateKey compound command", function() {
        ed.setValue('foo\nbar'); ed.moveCursorToPosition({row: 0, column: 0});

        keys.simulateKey(ed, 'Right');
        expect(ed).to.have.rangeAndContent(0,1,0,1, 'foo\nbar');

        keys.simulateKey(ed, 'Left');
        var selectToEndKeys = ed.commands.byName.selecttoend.bindKey[platform];
        keys.simulateKey(ed, selectToEndKeys);
        expect(ed).to.have.rangeAndContent(0,0,1,3, 'foo\nbar');

        ed.moveCursorToPosition({row: 0, column: 0});
        var dupKeys = ed.commands.byName.duplicateSelection.bindKey[platform];
        keys.simulateKey(ed, dupKeys);
        expect(ed).to.have.rangeAndContent(0,0,0,0, 'foo\nfoo\nbar');
    });

    it("simulate multiple keys at once", function() {
        ed.setValue(''); ed.moveCursorToPosition({row: 0, column: 0});
        keys.simulateKeys(ed, "h i  t h e e Left r");
        expect(ed).to.have.rangeAndContent(0,7,0,7, 'hi there');
    });

  });

  describe("key customization", function() {

    it("can define new keybindings for exisiting commands", function() {
      var run = false;
      var cmd = {name: "test-command", exec: function() { run = true; }};
      ed.commands.addCommands([cmd]);
      ace.ext.keys.addKeyCustomizationLayer("test-layer",
        {commandKeyBinding: {"alt-t": "test-command"}})
      var found = ace.ext.keys.lookupKeys(ed, "Alt-t")
      expect(cmd).to.equal(found);
      expect(ed.keyBinding.$handlers.length).to.equal(1);
    });

    it("can bind keys with objectified command", function() {
      var result;
      var cmd = {name: "test-command", exec: function(_d, args) { result = args.value; }};
      ed.commands.addCommands([cmd]);
      ace.ext.keys.addKeyCustomizationLayer("test-layer",
        {commandKeyBinding: {"alt-t": {name: "test-command", args: {value: 23}}}})
      ace.ext.keys.simulateKey(ed, "Alt-t");
      expect(result).to.equal(23);
    });

    it("can remove customizations", function() {
      var cmd = {name: "test-command", exec: function() {}};
      ed.commands.addCommands([cmd]);
      ace.ext.keys.addKeyCustomizationLayer("test-layer", {commandKeyBinding: {"alt-t": "test-command"}});
      ace.ext.keys.removeKeyCustomizationLayer("test-layer")
      var found = ace.ext.keys.lookupKeys(ed, "Alt-t")
      expect(undefined).to.equal(found);
    });

    it("can define multiple customizations with priority ", function() {
      var run;
      var cmd1 = {name: "test-command-1", exec: function() { run = 1; }};
      var cmd2 = {name: "test-command-2", exec: function() { run = 2; }};
      var cmd3 = {name: "test-command-3", exec: function() { run = 3; }};
      ed.commands.addCommands([cmd1, cmd2, cmd3]);
      ace.ext.keys.addKeyCustomizationLayer("test-layer-1", {priority: 10, commandKeyBinding: {"alt-t": "test-command-1"}})
      ace.ext.keys.addKeyCustomizationLayer("test-layer-2", {priority: 12, commandKeyBinding: {"alt-t": "test-command-2"}})
      ace.ext.keys.addKeyCustomizationLayer("test-layer-3", {priority: 2, commandKeyBinding: {"alt-t": "test-command-3"}})
      ace.ext.keys.simulateKey(ed, "Alt-t");
      expect(run).equals(2);
    });

    it("can define multiple customizations with priority ", function() {
      var run;
      var cmd1 = {name: "test-command-1", exec: function() { run = 1; }};
      var cmd2 = {name: "test-command-2", exec: function() { run = 2; }};
      var cmd3 = {name: "test-command-3", exec: function() { run = 3; }};
      ed.commands.addCommands([cmd1, cmd2, cmd3]);
      ace.ext.keys.addKeyCustomizationLayer("test-layer-1", {priority: 10, commandKeyBinding: {"alt-t": "test-command-1"}})
      ace.ext.keys.addKeyCustomizationLayer("test-layer-2", {priority: 12, commandKeyBinding: {"alt-t": "test-command-2"}})
      ace.ext.keys.addKeyCustomizationLayer("test-layer-3", {priority: 2, commandKeyBinding: {"alt-t": "test-command-3"}})
      ace.ext.keys.simulateKey(ed, "Alt-t");
      expect(run).equals(2);
    });

    it("can make customizations mode specific", function() {
      var run;
      var cmd = {name: "test-command", exec: function() { run = 1; }};
      ed.commands.addCommands([cmd]);
      ace.ext.keys.addKeyCustomizationLayer("test-layer", {priority: 10, modes: ["ace/mode/javascript"], commandKeyBinding: {"alt-t": "test-command"}})
      ace.ext.keys.simulateKey(ed, "Alt-t");
      expect(run).equals(undefined);
      ed.setOption("mode", "ace/mode/javascript")
      ace.ext.keys.simulateKey(ed, "Alt-t");
      expect(run).equals(1);
    });

    it("can have a custom key handler", function() {
      var HashHandler = ace.require("ace/keyboard/hash_handler").HashHandler;
      var h = new HashHandler();
      var run;
      h.addCommand({bindKey: "alt-t", name: "test-command", exec: function() { run = 1; }})
      ace.ext.keys.addKeyCustomizationLayer("test-layer", {keyHandler: h})
      ace.ext.keys.simulateKey(ed, "Alt-t");
      expect(run).equals(1);
    });

  });

  describe("allEditorCommands", function() {

    it("looks up command in default handler", function() {
      var all = keys.allEditorCommands(ed);
      expect(all).deep.property("selectall[0].cmdName").to.equal("selectall")
      expect(all).deep.property("selectall[0].key").to.equal(commandKey.toLowerCase() + "-a");
      expect(all).deep.property("selectall[0].cmd").to.equal(ed.getKeyboardHandler().commands.selectall);
    });

    it("looks up command in custom handler", function() {
      var cmd = {name: "test-command", bindKey: "Alt-t", exec: function() { }},
          keyHandler = new (ace.require("ace/keyboard/hash_handler")).HashHandler([cmd]);
      ed.setKeyboardHandler(keyHandler);
      var all = keys.allEditorCommands(ed);
      expect(all).property('test-command').to.have.length(1);
      expect(all).deep.property("test-command[0].cmdName").to.equal("test-command")
      expect(all).deep.property("test-command[0].key").to.equal("alt-t")
      expect(all).deep.property("test-command[0].cmd").to.equal(cmd)
    });

    it("merges commands of handlers and customization layers", function() {
      var cmd = {name: "test-command", bindKey: "Alt-t", exec: function() { }},
          keyHandler = new (ace.require("ace/keyboard/hash_handler")).HashHandler([cmd]);
      ed.setKeyboardHandler(keyHandler);
      ace.ext.keys.addKeyCustomizationLayer("test-layer", {commandKeyBinding: {"alt-x": "test-command"}});
      var all = keys.allEditorCommands(ed);
      expect(all).property('test-command').to.have.length(2);
      expect(all).deep.property("test-command[0].cmdName").to.equal("test-command")
      expect(all).deep.property("test-command[0].key").to.equal("alt-t")
      expect(all).deep.property("test-command[0].cmd").to.equal(cmd)
      expect(all).deep.property("test-command[1].key").to.equal("alt-x")
      expect(all).deep.property("test-command[1].cmd").to.equal("test-command")
    });
  });
});
