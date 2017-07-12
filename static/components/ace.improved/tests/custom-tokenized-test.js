/*global process, expect, beforeEach, afterEach, describe, it*/

describe('custom tokens', function() {

  var ed;
  beforeEach(function() {
    ed = createEditor("this is a\ntest\n\ntext\n", {mode: 'ace/mode/attributedtext'});
  });

  afterEach(function() {
    if (ed) {
      ed.destroy();
      ed.container.parentNode.removeChild(ed.container);
    }
  });

  describe("tokenization", function() {
    
    it("sets text and attributes from spec", function() {
      var spec = [["Hello", "a"],[" ", "b"], ["World", "c"]];
      ed.session.getMode().set(ed, spec);
      expect(ed.getValue()).eq("Hello World");
      expect(ed.session.getTokens(0)).deep.eq([
        {type: "a", value: "Hello"},
        {type: "b", value: " "},
        {type: "c", value: "World"}]);
    });

    it("supports muliple lines", function() {
      var spec = [["Hel\nlo", "a"], ["World", "b"]];
      ed.session.getMode().set(ed, spec);
      expect(ed.getValue()).eq("Hel\nloWorld");
      expect(ed.session.getTokens(0)).deep.eq([
        {type: "a", value: "Hel"}], "0");
      expect(ed.session.getTokens(1)).deep.eq([
        {type: "a", value: "lo"},
        {type: "b", value: "World"}], "1");
    });

  });

  describe("text attr to position mapping", function() {
    
    it("text attr to position mapping", function() {
      var spec = [["Hel\nlo", "a"], ["World", "b"]];
      ed.session.getMode().set(ed, spec);
      expect(ed.session.getMode().textAttributeOfPosition(ed, {row:0, column: 1}))
        .to.containSubset({token: {type: 'a', value: "Hel"}});
    });

  });

  describe("key actions", function() {

    it("binds commands to keys", function() {
      var spec = [
        ["Hello", {commands: [{name: 'oink', bindKey: 'enter'}]}]
      ];
      ed.session.getMode().set(ed, spec);
      
      var h = ed.getKeyboardHandler()
      expect(h.isAttributedtextKeyboardHandler).to.be.true;

      expect(h.commands.oink).to.be.an("object")
      
      expect(h.commandKeyBinding)
        .to.have.property("return")
        .that.has.property("name")
        .that.equals("oink")
    });
    
    it("removes bounds commands on detach", function() {
      var spec = [
        ["Hello", {commands: [{name: 'oink', bindKey: 'enter'}]}]
      ];
      ed.session.getMode().set(ed, spec);

      ed.session.setMode("ace/mode/text");

      expect(ed.getKeyboardHandler().isAttributedtextKeyboardHandler).to.be.undefined;
    });
  });

});
