/*global process, beforeEach, afterEach, describe, it,module*/

var isNodejs = typeof module !== "undefined" && module.exports;
var paredit = isNodejs ? require("../index") : window.paredit;

var expect, i;
if (isNodejs) {
  var chai = require('chai');
  chai.use(require('chai-subset'));
  expect = chai.expect;
} else { expect = window.chai.expect; }

function pos(i,r,c) { return {idx: i, row: r, column: c}; };
function printPos(p) { return p.idx + ":" + p.row + ":" + p.column; }

var d = JSON.stringify;

describe('reading sexps', function() {

  var readSeq = paredit.reader.readSeq;
  var readSexp = paredit.reader.readSexp;

  it('reads symbol', function() {
    expect(readSeq("foo")).deep.equals(["foo"]);
  });

  it('reads next symbol', function() {
    expect(readSexp("foo bar")).eq("foo");
  });

  describe("sequences and nestedness", function() {

    it('reads sequence', function() {
      expect(readSeq("foo bar baz")).deep.equals(["foo", "bar", "baz"]);
    });

    it('reads empty sexp', function() {
      expect(readSeq("()")).deep.equals([[]], d(readSeq("()")));
    });

    it('reads simple list', function() {
      expect(readSeq("(foo bar)")).deep.equals([["foo", "bar"]]);
    });

    it('reads nested lists', function() {
      expect(readSeq("(foo bar) (baz (zzz)) zork"))
        .deep.equals([["foo", "bar"], ["baz", ["zzz"]], "zork"], d(readSeq("(foo bar) (baz (zzz)) zork")));
    });

    it('reads vector syntax', function() {
      expect(readSeq("(foo [bar])"))
        .deep.equals([["foo", ["bar"]]]);
    });

    it('reads map syntax', function() {
      expect(readSeq("{:foo bar :baz zork}"))
        .deep.equals([[":foo", "bar", ":baz", "zork"]]);
    });
  });

  describe("whitespace", function() {
    it('ignores whitespace', function() {
      expect(readSeq(" \n    foo   ")).deep.equals(["foo"]);
      expect(readSeq("barr   foo")).deep.equals(["barr", "foo"]);
      expect(readSeq("  (   bar   )  ")).deep.equals([["bar"]]);
    });
  });

  describe("strings", function() {

    it("reads strings", function() {
      expect(readSexp('"fooo"')).eq('"fooo"');
    });

    it("escapes", function() {
      expect(readSexp('"fo\\"oo"')).eq('"fo\\"oo"');
    });

  });

  describe("chars", function() {
    it("reads chars", function() {
      expect(readSeq('\\]')).deep.eq(["\\]"], d(readSeq('\\]')));
    });
  });

  describe("numbers", function() {
    it("reads number value", function() {
      expect(readSexp('123')).eq(123);
    });
  });

  describe("symbols", function() {
    it("reads quoted", function() {
      expect(readSexp("'foo")).eq("'foo");
    });

    it("reads slash", function() {
      expect(readSexp("foo/bar")).eq("foo/bar");
    });

    it("reads keywords", function() {
      expect(readSexp(":foo")).eq(":foo");
    });
  });

  describe("commas", function() {
    it("are ignored", function() {
      expect(readSexp("{:x 1, :y a, :z b}"))
        .deep.equals([":x",1,":y","a",":z","b"]);
    });
  });

  describe("comments", function() {

    it("aren't ignored", function() {
      expect(readSeq("; foo\n(baz ;; bar  \n  zork)"))
        .deep.equals(["; foo\n", ["baz", ";; bar  \n", "zork"]])
    });

    it("multiple lines are merged", function() {
      expect(readSeq("; foo\n  ; bar\n  a"))
        .deep.equals(["; foo\n  ; bar\n", "a"])
    });

    it("are indexed correctly", function() {
      expect(paredit.parse("(xx ;foo\n())"))
        .to.containSubset({
          errors: [],start: 0,end: 12,type: "toplevel",
          children: [{
            close: ")",open: "(",start: 0,end: 12,type: "list",
            children: [
              {start: 1,end: 3,type: "symbol"},
              {start: 4,end: 9,type: "comment"},
              {children: [], close: ")",open: "(",start: 9,end: 11,type: "list"}]
          }]
        })
    });
  });

  describe("macro syntax", function() {
    it("syntax quotes", function() {
      expect(readSeq("`x")).deep.equals(["`", "x"])
    });

    it("reads it", function() {
      expect(readSeq("`(fred x ~x lst ~@lst 7 8 :nine)"))
        .deep.equals(["`", ["fred", "x", "~", "x", "lst", "~", "@", "lst", 7, 8, ":nine"]]);
    });

  });

  describe('specials', function() {
    it("reads @s", function() {
      expect(readSeq("@@a"))
        .deep.equals(["@", "@", "a"]);
    });
  });

  describe("examples", function() {
    it("reads threaded", function() {
      expect(readSeq("(-> foo->bar @baz .*)"))
        .deep.equals([["->", "foo->bar", "@", "baz", ".*"]]);
    });

    it("deref sexp", function() {
      expect(readSeq("@(foo)")).deep.equals(["@", ["foo"]]);
    });

    it("annotation ", function() {
      expect(readSeq("(def ^private foo)"))
        .deep.equals([["def", "^", "private", "foo"]]);
    });

    it("no space", function() {
      expect(readSeq("foo\"bar\""))
        .deep.equals(["foo", '"bar"']);
    });

    it("var quote ", function() {
      expect(readSeq("#'foo")).deep.equals(["#", "'foo"]);
    });

    it("anonym fun literal ", function() {
      expect(readSeq("#(foo %)")).deep.equals(["#", ["foo", "%"]]);
    });

    it("map with string", function() {
      expect(readSeq("{:doc \"A\"}")).deep.equals([[":doc", '"A"']], d(readSeq("{:doc \"A\"}")));
    });

    it("nested map with number", function() {
      expect(readSeq("({2})")).deep.equals([[[2]]], d(readSeq("({2})")));
    });
  });

  describe("tagging positions", function() {
    it("correctly tracks sexps", function() {
      var p = [];
      function xform(type, read, start, end) {
        if (type === 'list')
          p.push(printPos(start) + "-" + printPos(end));
      }
      readSeq("(a (bb\nc))", xform);
      expect(p).deep.equals([ '3:0:3-9:1:2', '0:0:0-10:1:3' ]);
    })
  });

  describe("transforming results", function() {

    it("transform function gets read data", function() {
      var log = [];
      function xform(type, read, start, end) { log.push([type, read, start, end]); }
      readSexp("foo", xform);
      expect(log).deep.equals([['symbol', "foo", pos(0,0,0), pos(3,0,3)]]);
    });

    it("transforms the tree", function() {
      var counter = 0;
      function xform(type, read, start, end) {
        return type !== "list" ? counter++ : read;
      }
      var res = readSeq('(foo ("bar" (baz) 23))', xform);
      expect(res).deep.equals([[0, [1, [2], 3]]]);
    });

    it("transforms the tree to get locations", function() {
      var counter = 0;
      function xform(type, read, start, end) {
        var result = {type: type, start: start.idx, end: end.idx}
        if (type === "list") result.children = read;
        return result;
      }
      var res = readSeq('foo (bar "xyz"\n(12) \'z)', xform);
      var expected = [
        {start: 0,end: 3,type: "symbol"},
        {start: 4,end: 23,type: "list", children: [
          {start:5,end:8,type: "symbol"},
          {start:9,end:14,type: "string"},
          {start:15,end:19,type: "list", children: [
            {start:16,end:18,type: "number"}
          ]},
          {start:20,end:22,type: "symbol"},
        ]}
      ];
      expect(res).deep.equals(expected, d(res));
    });

  });

  function diff(read, expected) {
    return "\n" + JSON.stringify(read) +"\nvs.\n" + JSON.stringify(expected) + '\n';
  }

  describe("read errors", function() {
    it("embeds error infos for premature ending", function() {
      var actual = readSexp("(a(b)"),
          expected = {
            error: "Expected ')' but reached end of input at line 1 column 5",
            start: pos(0,0,0), end: pos(5,0,5),
            children: ["a", ["b"]]
          };
      expect(actual).deep.equals(expected, diff(actual,expected));
    });

    it("unmatched square bracket 1", function() {
      var actual = readSeq("(a)(x(let[bar y)z)(b)"),
          expected = [
            ['a'],
            {error: "Expected ')' but reached end of input at line 1 column 21"}];
      expect(actual).to.containSubset(expected);
    });

    it("unmatched square bracket 2", function() {
      var actual = readSeq("(a (b)](x y)"),
          expected = [
            {error: "Expected ')' but got ']' at line 1 column 6"},
            ['x', 'y']];
      expect(actual).to.containSubset(expected);
    });

    it("closed too often 1", function() {
      expect(readSeq("(a))(x y)")).to.containSubset([
        ["a"],
        {error: "Unexpected input: ')' at line 1 column 4"},
        ["x", "y"]]);
    });

    it("closed too often 2", function() {
      var actual = readSeq("a)"),
          expected = [
            "a",
            {error: "Unexpected input: ')' at line 1 column 2"}]
      console.log(diff(actual,expected));
      expect(actual).to.containSubset(expected);
    });

  });

});

describe("parser", function() {

  it("can parse code into an AST", function() {
    var ast = paredit.parse(
      "(aaa bbb [cc dddd e])",
      {addSourceForLeafs: true});
    var expected = {
      type: "toplevel", start: 0, end: 21,
      errors: [],
      children: [
        {start: 0,end: 21, type: "list", open: '(', close: ')',
         children: [
          {end: 4, start: 1, source: "aaa", type: "symbol"},
          {end: 8, start: 5, source: "bbb", type: "symbol"},
          {start: 9, end: 20, type: "list", open: '[', close: ']',
           children: [
             {end: 12, start: 10, source: "cc", type: "symbol"},
             {end: 17, start: 13, source: "dddd", type: "symbol"},
             {end: 19, start: 18, source: "e", type: "symbol"}]
        }],
      }]
    };
    expect(ast).deep.equals(expected, d(ast));
  });

  it("gives access to errors", function() {
    var ast = paredit.parse("a(", {addSourceForLeafs: true});
    var expected = {
      type: "toplevel", start: 0, end: 2,
      errors: [],
      children: [
        {},
        {error: "Expected ')' but reached end of input at line 1 column 2"}
      ]
    };
    expect(ast).to.containSubset(expected, d(ast));
  });

  it("can read a lot of code", function() {
    var code = "(in-package :cl-user)\n\n"
             + "(defmethod really-big-function ()";
    for (var i = 0; i < 3000; i++)
      code += "\n  (do-something-else '" + i + ")";
    code += "\n  (finally-the-end))";
    var ast = paredit.parse(code);
    expect(ast.children[1].children).to.have.length(3004);
  });
});
