/*
  fswatch -0 -r . | xargs -0 -I{} bash -c \
  "[[ \"{}\" =~ .js$ ]] && [[ ! \"{}\" =~ .bundle. ]] && node build.js;"
*/

var pkg = JSON.parse(require("fs").readFileSync("package.json"));
var target = "paredit-bundle.js";
require('concat')(pkg.bundle.files, target).then(function(e){
  require("fs").writeFileSync(
    target.replace(/\.js$/,".min.js"),
    require("uglify-js").minify(target).code);
  console.log("bundled");
});
