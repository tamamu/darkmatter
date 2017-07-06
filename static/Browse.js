
function createFile() {
  let fname = document.getElementById('new-file-name').value;
  location.assign(window.location.href+fname+".dm.lisp");
}
