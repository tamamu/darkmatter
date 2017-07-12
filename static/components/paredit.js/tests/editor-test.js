/*global process, beforeEach, afterEach, describe, it,module*/

"format global";

var isNodejs = typeof module !== "undefined" && module.exports;
var paredit = isNodejs ? require("../index") : window.paredit;

var expect, i;
if (isNodejs) {
  var chai = module.require('chai');
  chai.use(module.require('chai-subset'));
  expect = chai.expect;
} else { expect = window.chai.expect; }

var d = JSON.stringify;

var ed = paredit.editor;
var parse = function(src) {
  return paredit.parse(src, {addSourceForLeafs: true});
};

function times(n, ch) { return new Array(n+1).join(ch); }

function expectIndent(src, expected) {
  var actual = ed.indentRange(parse(src), src, 0,src.length);
  expect(actual.src).to.eql(expected);
}

function expectChangesAndIndex(actual, changes, idx) {
  if (!changes) expect(actual).to.eq(null,d(actual));
  else expect(actual.changes).to.deep.eq(changes, d(actual.changes));
  if (typeof idx == "number") {
    expect(actual.newIndex).equals(idx);
  }
}

function edit(methodName/*,args*/) {
  // a bit prettier than
  // expectChangesAndIndex(
  //   ed.wrapAround(parse("()"), "()", 1, '(', ')'),
  //   [['insert', 1, "("],
  //   ['insert', 2, ")"]],
  //   2);

  var args = Array.prototype.slice.call(arguments);
  var method = args.shift();
  return {
    transforms: function(title, fromTo) {
      if (typeof fromTo === "undefined") {
        fromTo = title;
        title = undefined;
      }
      var from = fromTo.split("->")[0];
      var to = fromTo.split("->")[1];
      title = (title ? title+":" : "") + from + "->" + to;
      return {
        withChanges: function(changes) {
          return it(title, function() {
            var src = from.replace(/\|/, "");
            var idx = from.indexOf("|");
            if (idx === -1) throw new Error("need from index! -> |");
            var newIdx = to.indexOf("|");
            if (newIdx === -1) newIdx = undefined;
            expectChangesAndIndex(
              ed[method].apply(ed, [parse(src), src, idx].concat(args)),
              changes, newIdx);
          });
        },
      }

    },
  }
}

describe('paredit editor', function() {

  describe("openList", function() {
    edit('openList')
      .transforms("|->(|)")
      .withChanges([['insert', 0, "()"]]);

    edit('openList')
      .transforms("|()->(|)()")
      .withChanges([['insert', 0, "()"]]);

    edit('openList')
      .transforms("(|)->((|))")
      .withChanges([['insert', 1, "()"]]);

    edit('openList', {endIdx: 5})
      .transforms("|(foo)->(|(foo))")
      .withChanges([['insert', 5, ")"],
                    ['insert', 0, "("]]);

    edit('openList')
      .transforms('("f|oo")->("f(|oo")')
      .withChanges([['insert', 3, "("]]);

    edit('openList')
      .transforms('(\n;f|oo\n)->(\n;f(|oo\n)')
      .withChanges([['insert', 4, "("]]);

    edit('openList', {endIdx: 7})
      .transforms('(a |b (c d))->(a (|)b (c d))')
      .withChanges([['insert', 3, "()"]]);

    describe("with errors", function() {
      edit('openList', {})
        .transforms('a |())->a (|())')
        .withChanges([['insert', 2, "("]]);
    });

    describe("with paredit correction disabled", function() {
      edit('openList', {freeEdits: true})
        .transforms('a |(())->a (|(())')
        .withChanges([['insert', 2, "("]]);
    });

  });

  describe("splitting", function() {
    edit('splitSexp', "()", 1)
      .transforms("(|)->()| ()")
      .withChanges([['insert', 1, ") ("]]);

    edit('splitSexp', "(foo)", 1)
      .transforms("updates child indexes","(|foo)->()| (foo)")
      .withChanges([['insert', 1, ") ("]]);

    edit('splitSexp', "[]", 1)
      .transforms("uses correct paren for change","[|]->[]| []")
      .withChanges([["insert", 1, "] ["]]);

    describe("strings", function() {
      edit("splitSexp",'"foo"', 3)
        .transforms('"fo|o"->"fo"| "o"')
        .withChanges([["insert", 3, '" "']]);
    });
  });

  describe("wrap around", function() {

    edit("wrapAround",'(', ')')
      .transforms("(|)->((|))")
      .withChanges(
        [['insert', 1, "("],
         ['insert', 2, ")"]]);

    edit("wrapAround",'(', ')')
      .transforms("|a->(|a)")
      .withChanges(
        [['insert', 0, "("],
         ['insert', 2, ")"]]);

    edit("wrapAround",'(', ')', {count: 2})
      .transforms("|a bb->(|a bb)")
      .withChanges(
        [['insert', 0, "("],
         ['insert', 5, ")"]]);

  });

  describe("splice", function() {
    edit('spliceSexp')
      .transforms('(aa| bb)->aa| bb')
      .withChanges([['remove', 6, 1],['remove', 0, 1]]);

    edit('spliceSexp')
      .transforms('"foo |bar"->foo |bar')
      .withChanges([['remove', 8, 1],['remove', 0, 1]]);
  });

  describe("splice and kill", function() {

    edit('spliceSexpKill', {backward: true})
      .transforms("(aa |bb)->|bb")
      .withChanges(
        [['remove', 6, 1],
         ['remove', 1, 3],
         ['remove', 0, 1]]);

    edit('spliceSexpKill', {backward: true})
      .transforms("(|bb)->|bb")
      .withChanges(
        [['remove', 3, 1],
         ['remove', 0, 1]]);

    edit('spliceSexpKill', {backward: true})
      .transforms("aa (x|y)->aa |y")
      .withChanges(
        [['remove', 6, 1],
         ['remove', 4, 1],
         ['remove', 3, 1]]);

    edit('spliceSexpKill', {backward: false, count: 2})
      .transforms("(aa |bb cc)->aa |")
      .withChanges(
        [['remove', 9, 1],
         ['remove', 4, 5],
         ['remove', 0, 1]]);

    edit('spliceSexpKill', {backward: true})
      .transforms("(\"f|oo\" bb)->|oo bb")
      .withChanges(
        [['remove', 9, 1],
         ['remove', 5, 1],
         ['remove', 2, 1],
         ['remove', 1, 1],
         ['remove', 0, 1]]);
  });

  describe("closeAndNewline", function() {

    edit("closeAndNewline")
      .transforms(" (aa| bb)-> (aa bb)\n |")
      .withChanges([['insert', 8, "\n "]]);

    edit("closeAndNewline", ']')
      .transforms("[(aa| bb)]->[(aa bb)]\n|")
      .withChanges([['insert', 9, "\n"]]);

  });

  describe("barfSexp", function() {
    edit("barfSexp", {backward: true})
      .transforms("backward: ", "(foo (bar baz |quux) zot)->(foo bar (baz |quux) zot)")
      .withChanges([
        ['insert', 10, '('],
        ['remove', 5, 1]]);

    edit("barfSexp", {backward: false})
      .transforms("forward: ", "(foo (bar baz |quux) zot)->(foo (bar baz |) zot)")
      .withChanges(
        [['remove', 18, 1],
         ['insert', 14, ')']]);

    edit("barfSexp", {backward: false})
      .transforms("forward: ", "(fo|o bar)->(fo|o) bar")
      .withChanges(
        [['remove', 8, 1],
         ['insert', 4, ')']]);

    edit("barfSexp", {backward: true})
      .transforms("forward: ", "(foo b|ar)->foo (b|ar)")
      .withChanges(
        [['insert', 5, '('],
         ['remove', 0, 1]]);
  });

  describe("slurpSexp", function() {
    edit("slurpSexp", {backward: false, count: 2})
      .transforms("forward: ", "(a (a |b) c d)->(a (a |b c d))")
      .withChanges(
        [['insert', 12, ')'],
         ['remove', 7, 1]]);

    edit("slurpSexp", {backward: true, count: 2})
      .transforms("backward: ", "(x y z (a |b) c d)->(x (y z a |b c d))")
      .withChanges(
        [['remove', 7, 1],
         ['insert', 3, '(']]);
  });

  describe("killSexp", function() {
    edit("killSexp",{backward: false, count: 2})
      .transforms("forward: ","(a |b c d)->(a | d)")
      .withChanges([['remove', 3, 3]]);

    edit("killSexp",{backward: true, count: 2})
      .transforms("backward: ","(a |b c d)->(|b c d)")
      .withChanges([['remove', 1, 2]]);

    edit("killSexp",{backward: true})
      .transforms("string: ",'("fo|o" b)->("|o" b)')
      .withChanges([['remove', 2, 2]]);

    edit("killSexp",{backward: true})
      .transforms('fo|o->|o')
      .withChanges([['remove', 0, 2]]);

    edit("killSexp",{backward: true})
      .transforms("comment: ",'(;fo|oo\nb)->(|oo\nb)')
      .withChanges([['remove', 1, 3]]);
  });

  describe("deletion", function() {
    edit("delete",{backward: true, count: 2})
      .transforms("deletes non list-like things", "(abc|)->(a|)")
      .withChanges([['remove', 2, 2]]);

    edit("delete",{backward: false, count: 2})
      .transforms("does not delete forward ad end of list", "(abc|)->(abc|)")
      .withChanges([]);

    edit("delete",{backward: true})
      .transforms("deletes empty lists backward", "(|)->|")
      .withChanges([['remove', 0, 2]]);

    edit("delete",{backward: true})
      .transforms("deletes empty lists backward but only at beginning", "( |)->(|)")
      .withChanges([['remove', 1, 1]]);
;
    edit("delete",{backward: false})
      .transforms("deletes empty lists forward","(|)->|")
      .withChanges([['remove', 0, 2]]);

    edit("delete",{backward: false})
      .transforms("deletes empty lists forward on toplevel", "|()->|")
      .withChanges([['remove', 0, 2]]);

    edit("delete",{backward: false})
      .transforms('deletes empty strings', '"|"->|')
      .withChanges([['remove', 0, 2]]);

    edit("delete",{backward: true, endIdx: 5})
      .transforms('doesn\'t delete string so that it\'s broken', '"fo|o"->"foo"')
      .withChanges(null);

    edit("delete",{backward: true, endIdx: 5})
      .transforms('don\'t cross delete strings', 'a|b "foo"->ab "foo"')
      .withChanges(null);

    edit("delete",{backward: true, endIdx: 8})
      .transforms('delete string when deleted completely', 'a|b "foo"->a|')
      .withChanges([['remove', 1, 7]]);

    edit("delete",{backward: true, endIdx: 4})
      .transforms('delete range inside string', '"f|oo"->"f|"')
      .withChanges([['remove', 2, 2]]);

    edit("delete",{backward: true, endIdx: 12})
      .transforms('deletes entire sexps when no overlap', "(foo| bar baz)->(foo|)")
      .withChanges([['remove', 4, 8]]);

    edit("delete",{backward: true, endIdx: 12})
      .transforms('no overlap range dels', "(fo|o bar baz)->(fo|)")
      .withChanges([['remove', 3, 9]]);

    edit("delete",{backward: true})
      .transforms('simply deletes numbers', "123|->12|")
      .withChanges([['remove', 2, 1]]);

    edit("delete",{backward: true})
      .transforms('specials', "a #|b->a |b")
      .withChanges([['remove', 2, 1]]);

    edit("delete",{backward: true, endIdx: 3})
      .transforms("a|bc->a|")
      .withChanges([['remove', 1, 2]]);

    edit("delete",{backward: true, endIdx: 4})
      .transforms("a|b ab->a|b")
      .withChanges([['remove', 1, 3]]);

    edit("delete",{backward: true, endIdx: 2})
      .transforms("delete including space", "|a  b->| b")
      .withChanges([['remove', 0, 2]]);

    describe("with errors", function() {
      edit("delete",{backward: true})
        .transforms("allow simple delete", "fo(|o->fo|o")
        .withChanges([['remove', 2, 1]]);

      edit("delete",{backward: false, endIdx: 4})
        .transforms("allow simple range delete", "a |((x) x|->a |x) x")
        .withChanges([['remove', 2, 2]]);
    });

    describe("with paredit correction disabled", function() {
      edit("delete",{backward: true, freeEdits: true})
        .transforms("fo(|o)->fo|o)")
        .withChanges([['remove', 2, 1]]);

      edit("delete",{backward: false, endIdx: 4, freeEdits: true})
        .transforms("a |((x)) x|->a |x)) x")
        .withChanges([['remove', 2, 2]]);
    });

  });

  describe("transpose", function() {

    edit("transpose",{})
      .transforms("(xxx |yyy)->(xxx |yyy)")
      .withChanges([['insert', 8, " xxx"],['remove', 1, 4]]);

    edit("transpose",{})
      .transforms("((a)|(b))->((b)|(a))")
      .withChanges([['insert', 7, "(a)"],['remove', 1, 3]]);

    edit("transpose",{}).transforms("(|yyy)->(yyy)").withChanges(null);

    edit("transpose",{}).transforms("( | )->(  )").withChanges(null);

    edit("transpose",{})
      .transforms("aaaaa|\n\nbbb->bbb|\n\naaaaa")
      .withChanges([['insert', 10, "\n\naaaaa"],['remove', 0, 7]]);
  });

  describe("rewrite ast", function() {
    it("replaces nodes and constructs new ast", function() {
      var ast = parse("(a (c d) d) e"),
          target = ast.children[0].children[1].children[0], // c
          replacement = [{start: 4,end: 7,source: "ccc",type: "symbol"}],
          actual = ed.rewrite(ast, target, replacement),
          expected = parse("(a (ccc d) d) e");
      expect(actual).to.containSubset(expected, d(actual));
    });
  });

  describe("indentation", function() {

    it("indents sexp parts on newline", function() {
      var src = "(foo\nbar)";
      var actual = ed.indentRange(parse(src), src, 6,6);
      expect(actual.changes).to.deep.equal([["insert", 5, " "]], d(actual.changes));
    });

    it("indents multiple lines", function() {
      var src = "(foo\nbar\n    baz)";
      var actual = ed.indentRange(parse(src), src, 6,15);
      var expected = [
        ["insert", 5, " "],
        ["remove", 10/*!not 9 since change of prev line*/, 3]]
      expect(actual.changes).to.deep.equal(expected, d(actual.changes));
    })

    it("indents according to parent list", function() {
      var src = "  (foo bar\n      baz)";
      var actual = ed.indentRange(parse(src), src, 17,17);
      var expected = [
        ["insert", 11, times("  (foo ".length - "      ".length, " ")]];
      expect(actual.changes).to.deep.equal(expected, d(actual.changes));
    });

    it("recognizes special forms", function() {
      expectIndent("(defn foo\n[]\n(let []\na))", "(defn foo\n  []\n  (let []\n    a))");
    });

    it("updates ast for empty form", function() {
      expectIndent("(\n)", "(\n )");
    });

    it("indents special forms correctly", function() {
      expectIndent("(defn\nx\ny)", "(defn\n  x\n  y)");
    });

    it("indents multiple toplevel sexps at once", function() {
      expectIndent("(\n)\n(\n)", "(\n )\n(\n )");
    });

    it("keeps indentation of strings", function() {
      expectIndent('(defn foo\n"hello\n      world"\n[]\n5)', '(defn foo\n  "hello\n      world"\n  []\n  5)');
    });

    it("keeps indentation of strings", function() {
      expectIndent('(defun foo (x)\n  (* x x))\n\n', '(defun foo (x)\n  (* x x))\n\n');
    });


    describe("with errors", function() {

      it("unclosed sexp", () =>
        expectIndent("[[[[[[\nx\ny\n]", "[[[[[[\n      x\n      y\n      ]"));

      it("unclosed sexp", () =>
        expectIndent("[[[[[[\nx\ny\n]", "[[[[[[\n      x\n      y\n      ]"));

    });

  });
});
