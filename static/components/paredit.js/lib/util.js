/*global window, process, global,module*/

"format global";

;(function(run) {
  var isNodejs = typeof module !== "undefined" && module.exports;
  var exports = isNodejs ? module.exports : window.paredit;
  run(exports);

})(function(exports) {

  var util = exports.util = {
    merge: function (objs) {
      if (arguments.length > 1) {
        return util.merge(Array.prototype.slice.call(arguments));
      }

      if (Array.isArray(objs[0])) { // test for all?
        return Array.prototype.concat.apply([], objs);
      }

      return objs.reduce(function(merged, ea) {
        for (var name in ea)
          if (ea.hasOwnProperty(name))
              merged[name] = ea[name];
        return merged;
      }, {});
    },

    mapTree: function (treeNode, mapFunc, childGetter) {
      // Traverses the tree and creates a structurally identical tree but with
      // mapped nodes
      var mappedNodes = (childGetter(treeNode) || []).map(function(n) {
        return util.mapTree(n, mapFunc, childGetter);
      })
      return mapFunc(treeNode, mappedNodes);
    },

    flatFilterTree: function (treeNode, testFunc, childGetter) {
      // Traverses a `treeNode` recursively and returns all nodes for which
      // `testFunc` returns true. `childGetter` is a function to retrieve the
      // children from a node.
      var result = [];
      if (testFunc(treeNode)) result.push(treeNode);
      return result.concat(
        (childGetter(treeNode) || []).reduce(function(filtered, node) {
          return filtered.concat(util.flatFilterTree(node, testFunc, childGetter));
        }, []));
    },

    last: function(a) { return a[a.length-1]; },

    times: function(n, ch) { return new Array(n+1).join(ch); },

    clone: function(obj) {
      // Shallow copy
      if (Array.isArray(obj)) return Array.prototype.slice.call(obj);
      var clone = {};
      for (var name in obj) { clone[name] = obj[name]; }
      return clone;
    }
  }

});
