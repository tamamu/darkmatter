define("ace/mode/common_lisp_highlight_rules", ["require", "exports", "module",
  "ace/lib/oop", "ace/mode/text_highlight_rules"
], function(e, t, n) {
  "use strict";
  var r = e("../lib/oop"),
    i = e("./text_highlight_rules").TextHighlightRules,
    s = function() {
      var e =
        "switch|let*|if-let|let1|case|do|let|loop|if|else|when-let*|when-let|when|cond|unless",
        t = ">=|>|<=|<|string/=|string=|=|/=|equalp|equal|eq|neq|and|or",
        n = "null|nil",
        r =
        "cons|car|cdr|cond|lambda|format|setq|setf|quote|eval|append|list|listp|memberp|t|load|progn",
        i = this.createKeywordMapper({
          "keyword.control": e,
          "keyword.operator": t,
          "constant.language": n,
          "support.function": r
        }, "identifier", !0);
      this.$rules = {
        start: [{
          token: "comment",
          regex: ";.*$"
        }, {
          token: ["storage.type.function-type.common_lisp", "text",
            "entity.name.function.common_lisp"
          ],
          regex: "(?:\\b(?:(defun|defmethod|defmacro|defparameter|defvar))\\b)(\\s+)((?:\\w|\\-|\\!|\\?)*)"
        }, {
          token: [
            "punctuation.definition.constant.character.common_lisp",
            "constant.character.common_lisp"
          ],
          regex: "(#)((?:\\w|[\\\\+-=<>'\"&#])+)"
        }, {
          token: ["punctuation.definition.variable.common_lisp",
            "variable.other.global.common_lisp",
            "punctuation.definition.variable.common_lisp"
          ],
          regex: "(\\*)(\\S*)(\\*)"
        }, {
          token: "constant.numeric",
          regex: "0[xX][0-9a-fA-F]+(?:L|l|UL|ul|u|U|F|f|ll|LL|ull|ULL)?\\b"
        }, {
          token: "constant.numeric",
          regex: "[+-]?\\d+(?:(?:\\.\\d*)?(?:[eE][+-]?\\d+)?)?(?:L|l|UL|ul|u|U|F|f|ll|LL|ull|ULL)?\\b"
        }, {
          token: i,
          regex: "[a-zA-Z_$][a-zA-Z0-9_$]*\\b"
        }, {
          token: "string",
          regex: '"(?=.)',
          next: "qqstring"
        }],
        qqstring: [{
          token: "constant.character.escape.common_lisp",
          regex: "\\\\."
        }, {
          token: "string",
          regex: '[^"\\\\]+'
        }, {
          token: "string",
          regex: "\\\\$",
          next: "qqstring"
        }, {
          token: "string",
          regex: '"|$',
          next: "start"
        }]
      }
    };
  r.inherits(s, i), t.CommonLispHighlightRules = s
}), define("ace/mode/common_lisp", ["require", "exports", "module",
  "ace/lib/oop", "ace/mode/text", "ace/mode/common_lisp_highlight_rules"
], function(e, t, n) {
  "use strict";
  var r = e("../lib/oop"),
    i = e("./text").Mode,
    s = e("./common_lisp_highlight_rules").CommonLispHighlightRules,
    o = function() {
      this.HighlightRules = s, this.$behaviour = this.$defaultBehaviour
    };
  r.inherits(o, i),
    function() {
      this.lineCommentStart = ";", this.$id = "ace/mode/common_lisp"
    }.call(o.prototype), t.Mode = o
})
