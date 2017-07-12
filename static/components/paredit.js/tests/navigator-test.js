/*global process, beforeEach, afterEach, describe, it*/

var isNodejs = typeof module !== "undefined" && module.exports;
var paredit = isNodejs ? require("../index") : window.paredit;

var expect, i;
if (isNodejs) {
  var chai = require('chai');
  chai.use(require('chai-subset'));
  expect = chai.expect;
} else { expect = window.chai.expect; }

var d = JSON.stringify;

var nav = paredit.navigator;

var parse = function(src) { return paredit.parse(src); };

describe('paredit navigator', function() {

  var ast = parse("(aaa bbb [cc dddd e]) ()");

  describe("basic movements", function() {

    describe("forwardSexp", function() {

      it("|(...)->(...)|", function() {
        expect(nav.forwardSexp(ast, 0)).eq(21);
        expect(nav.forwardSexp(ast, 1)).eq(4);
      });

      it("| (...)->(...)|", function() {
        expect(nav.forwardSexp(ast, 4)).eq(8);
      });

      it("a|aa->aaa|", function() {
        expect(nav.forwardSexp(ast, 2)).eq(4);
      });

    });

    describe("backwardSexp", function() {

      it("(...)|->|(...)", function() {
        expect(nav.backwardSexp(ast, 24)).eq(22);
        expect(nav.backwardSexp(ast, 21)).eq(0);
        expect(nav.backwardSexp(ast, 4)).eq(1);
      });

      it("(...) |->|(...)", function() {
        expect(nav.backwardSexp(ast, 5)).eq(1);
      });

      it("aa|a->|aaa", function() {
        expect(nav.backwardSexp(ast, 3)).eq(1);
      });

    });

    describe("forwardDown", function() {

      it("|(...)->(|...)", function() {
        expect(nav.forwardDownSexp(ast, 0)).eq(1);
        expect(nav.forwardDownSexp(ast, 1)).eq(10);
        expect(nav.forwardDownSexp(ast, 22)).eq(23);
      });

    });

    describe("backwardUp", function() {

      it("(..|.)->|(...)", function() {
        expect(nav.backwardUpSexp(ast, 8)).eq(0);
        expect(nav.backwardUpSexp(ast, 15)).eq(9);
        expect(nav.backwardUpSexp(ast, 21)).eq(21);
      });

      it('(.".|.")->|(.|"..")', function() {
        expect(nav.backwardUpSexp(parse('(a"xx")'), 4)).eq(2);
      });

    });
  });

  describe("list navigation", function() {

    describe(")", function() {

      it("(..|.)->(...)|", function() {
        expect(nav.closeList(parse("(a b c)"), 2)).eq(7);
      });

      it("|(...)->|(...)", function() {
        expect(nav.closeList(parse("(a b c)"), 0)).eq(0);
      });

      it("(.[.|.])->(.[..]|)", function() {
        expect(nav.closeList(parse("(a [b c])"), 5)).eq(8);
      });

      it("(.{.|.})->(.{..}|)", function() {
        expect(nav.closeList(parse("(a {b c})"), 5)).eq(8);
      });

      it("(.\".|.\")->(.\".)|.\")", function() {
        expect(nav.closeList(parse("(a \"foo\")"), 6)).eq(undefined);
      });

    });
  });

  describe("sexp boundaries", function() {

    describe('range for idx', function() {
      it("...xxx|...->...*xxx*...", function() {
        expect(nav.sexpRange(parse("  aaa  "), 5)).deep.eq([2,5]);
      });

      it("(xxx|)->(*xxx*)", function() {
        expect(nav.sexpRange(parse("(aa)"), 3)).deep.eq([1,3]);
        expect(nav.sexpRange(parse("(aa bbb)"), 3)).deep.eq([1,3]);
      });

      it(".(xxx.|.)..->..(*xxx..*)..", function() {
        expect(nav.sexpRange(parse(" (aaa  ) "), 6)).deep.eq([2,7]);
      });

      it("(.|.)->(*...*)", function() {
        expect(nav.sexpRange(parse("(   )"), 2)).deep.eq([1,4]);
      });

      it("|()->*()*", function() {
        expect(nav.sexpRange(parse("()"), 0)).deep.eq([0,2]);
        expect(nav.sexpRange(parse("()"), 2)).deep.eq([0,2]);
      });

      it('".|."->"*...*"', function() {
        expect(nav.sexpRange(parse('"foo"'), 2)).deep.eq([1,4]);
      });

      it("ignores toplevel", function() {
        expect(nav.sexpRange(parse("a  a"), 2)).deep.eq(null);
      });

    })

    describe("expansion", function() {

      it(".(*xxx*)..->..*(xxx)*..", function() {
        expect(nav.sexpRangeExpansion(parse(" (aaa) "), 2,5)).deep.eq([1,6]);
      });

      it(".(xx*x)*..->..*(xxx)*..", function() {
        expect(nav.sexpRangeExpansion(parse(" (aaa) "), 4,6)).deep.eq([1,6]);
      });

      it('."*xxx*")..->..*"xxx"*..', function() {
        expect(nav.sexpRangeExpansion(parse(' "aaa" '), 2,5)).deep.eq([1,6]);
      });

      it('."*xx*x")..->.."*xxx*"..', function() {
        expect(nav.sexpRangeExpansion(parse(' "aaa" '), 2,4)).deep.eq([2,5]);
      });

      it("(*x* x)->(*x xx*)", function() {
        expect(nav.sexpRangeExpansion(parse("(a aa)"), 1,2)).deep.eq([1,5]);
      });

      it("@@*xxx*->@*@xxx*", function() {
        expect(nav.sexpRangeExpansion(parse("@@aaa"), 2,5)).deep.eq([1,5]);
      });

      it("@*@xxx*->*@@xxx*", function() {
        expect(nav.sexpRangeExpansion(parse("@@aaa"), 1,5)).deep.eq([0,5]);
      });

      it("dont expand to toplevel", function() {
        expect(nav.sexpRangeExpansion(parse(" (a) a"), 1,4)).deep.eq(null);
      });

    });

    describe("defun", function() {
      it("rangeForDefun", function() {
        expect(nav.rangeForDefun(parse("(y) (defn x [args] 23) (23)"), 14))
          .deep.eq([4,22]);
      });
    });

  });
});
