/*global ace, createEditor, expect, before, beforeEach, afterEach, describe, it*/

describe('improvements', function() {

  var ed;
  beforeEach(function() {
    ed = createEditor("this is a\ntest\n\ntext\n");
  });

  afterEach(function() {
    if (ed) {
      ed.destroy();
      ed.container.parentNode.removeChild(ed.container);
    }
  });

  describe("selection", function() {

    it("selects via range string", function() {
      ed.setSelection("[0/2]->[0/4]");
      expect(ed).to.have.selection("[0/2]->[0/4]");
      expect(ed.getSelectedText()).eq("is");
      expect(ed.selection.isBackwards()).eq(false);
    });

    it("selects via range string backward", function() {
      ed.setSelection("[0/4]->[0/2]");
      expect(ed.getSelectedText()).eq("is");
      expect(ed.selection.isBackwards()).eq(true);
    });

    it("selects via index", function() {
      ed.setSelection(1,3);
      expect(ed.getSelectedText()).eq("hi");
      expect(ed.selection.isBackwards()).eq(false);
    });

    it("selects via index backwards", function() {
      ed.setSelection(3,1);
      expect(ed.getSelectedText()).eq("hi");
      expect(ed.selection.isBackwards()).eq(true);
    });

    it("does multiple selections", function() {
      ed.addSelection(0, 4);
      ed.addSelection("[1/0]->[1/4]");
      expect(ed.getSelectedText()).eq("this\ntest\n");
    });

    it('saveExcursion', function(done) {
      expect(ed).to.exist();
      ed.setSelection("[0/2]->[0/4]");
      ed.saveExcursion(function(reset) {
        ed.selection.clearSelection();
        expect(ed).to.have.selection("[0/4]->[0/4]");
        reset();
        expect(ed).to.have.selection("[0/2]->[0/4]");
        done();
      });
    });

    it('saveExcursion resets to multi selection', function(done) {
      ed.setSelection(0, 1); ed.addSelection(2,3); ed.addSelection(5,6);
      expect(ed.getSelectedText()).eq("t\ni\ni");
      ed.saveExcursion(function(resetFunc) {
        ed.setSelection("[0/0]->[0/10]");
        setTimeout(resetFunc, 20);
        setTimeout(function() {
          expect(ed.getSelectedText()).eq("t\ni\ni");
          done();
        }, 30);
      });
    });
  });

});
