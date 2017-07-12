/*global window, process, global, module*/

"format global";

// If not on nodejs: concat or load lib files after loading this files.

(function() {
  var isNodejs = typeof module !== "undefined" && module.exports,
      exports = isNodejs ? module.exports : (window.paredit = {});

  if (isNodejs) {
    exports.reader       = require("./lib/reader").reader;
    exports.navigator    = require("./lib/navigator").navigator;
    exports.editor       = require("./lib/editor").editor;
    exports.specialForms = require("./lib/editor").specialForms;
  }

  exports.parse = function(src, options) {
    options = options || {};
    var addSrc = options.hasOwnProperty('addSourceForLeafs') ?
       options.addSourceForLeafs : true;
    var errors = [];

    var nodes = exports.reader.readSeq(src, function xform(type, read, start, end, args) {
      var result = {type: type, start: start.idx, end: end.idx};
      if (type === "error") {
        result.error = read.error;
        if (read.children) result.children = read.children;
        errors.push(result);
      } else if (addSrc && type !== 'list')
        result.source = src.slice(result.start, result.end);
      if (type === "list") result.children = read;
      if (type === "list" || type === "string" || (type === "error" && args)) {
        result.open = args.open;
        result.close = args.close;
      }
      return result;
    });

    return {
      type: "toplevel", start: 0,
      end: (nodes && nodes.length && nodes[nodes.length-1].end) || 0,
      errors: errors,
      children: nodes
    };
  };

})();
