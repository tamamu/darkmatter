
function getEditCells() {
  return document.getElementsByClassName('editcell');
}

function getCurrentCellId() {
  if (window.currentCell) {
    return window.currentCell;
  } else {
    return null;
  }
}

function addCellToCurrentAbove() {
  let container = document.getElementById('dm-container');
  let current = getCurrentCellId();
  if (current) {
    window.editcells[current].addCellToAbove(window.ls, container);
  }
}

function addCellToCurrentBelow() {
  let container = document.getElementById('dm-container');
  let current = getCurrentCellId();
  if (current) {
    window.editcells[current].addCellToBelow(window.ls, container);
  }
}

function evalAllCells() {
  let cells = document.getElementsByClassName('editcell');
  if (cells.length <= 0) return;

  let first = window.editcells[cells[0].id];
  console.log(first);
  first.eval(evalNextCell);
}

function evalNextCell(cell) {
  if (cell.next !== '' && cell.editor.getValue().trim() !== '') {
    window.editcells[cell.next].eval(evalNextCell);
  }
}

function swapCell(cell1, cell2) {
  let next2 = cell2.dataset.next;
  let prev1 = cell1.dataset.before;
  cell1.dataset.before = cell2.id;
  cell2.dataset.next = cell1.id;
  cell1.dataset.next = next2;
  cell2.dataset.before = prev1;
  let parent = cell1.parentElement;
  parent.removeChild(cell2);
  parent.insertBefore(cell2, cell1);
}

function adjustScroll(cell) {
  document.getElementById('dm-container').scrollTop =
    cell.offsetTop - Math.min(window.innerHeight/2 + cell.clientHeight, 200);
}

function swapNextCell() {
  let current;
  if (current = getCurrentCellId()) {
    let cells = getEditCells();
    let cell = cells[current];
    if (cell.dataset.next !== '') {
      let next = cells[cell.dataset.next];
      swapCell(cell, next);
      window.editcells[cell.id].editor.focus();
      adjustScroll(cell);
    }
  }
}

function swapPrevCell() {
  let current;
  if (current = getCurrentCellId()) {
    let cells = getEditCells();
    let cell = cells[current];
    if (cell.dataset.before !== '') {
      let prev = cells[cell.dataset.before];
      swapCell(prev, cell);
      window.editcells[cell.id].editor.focus();
      adjustScroll(cell);
    }
  }
}

function removeCurrentCell() {
  let current;
  if (current = getCurrentCellId()) {
    let cells = getEditCells();
    let cell = cells[current];
    let prev = cell.dataset.before;
    let next = cell.dataset.next;
    if (next && cells[next]) {
      cells[next].dataset.before = prev;
      window.editcells[next].editor.focus();
    }
    if (prev && cells[prev]) {
      cells[prev].dataset.next = next;
      window.editcells[prev].editor.focus();
    }
    let parent = cell.parentElement;
    parent.removeChild(cell);
  }
}

function focusNextCell() {
  let current;
  if (current = getCurrentCellId()) {
    let cell = getEditCells()[current];
    if (cell.dataset.next) {
      let next = window.editcells[cell.dataset.next];
      next.editor.focus();
      adjustScroll(next.element);
    }
  }
}
function focusPrevCell() {
  let current;
  if (current = getCurrentCellId()) {
    let cell = getEditCells()[current];
    if (cell.dataset.before) {
      let prev = window.editcells[cell.dataset.before];
      prev.editor.focus();
      adjustScroll(prev.element);
    }
  }
}

class EditCell {

  constructor() {
    this.cell = null;
    this.caches = {};
    this.container = null;
    this.editor = null;
    this.output = null;
  }

  static createEditor(elm) {
    let editor = ace.edit(elm);
    editor.setTheme('ace/theme/monokai');
    editor.setOptions({maxLines: Infinity});
    editor.setFontSize(14);
    editor.$blockScrolling = Infinity;
    let session = editor.getSession();
    session.setMode('ace/mode/lisp');
    session.setUseSoftTabs(true);
    session.setUseWrapMode(true);
    return editor;
  }

  static fromElement(elm) {
    let obj = new EditCell();
    obj.cell = elm;
    if (obj.cell.dataset.next === undefined) {
      obj.cell.dataset.next = '';
    }
    if (obj.cell.dataset.before == undefined) {
      obj.cell.dataset.before = '';
    }
    obj.caches = {};
    obj.caches['lisp'] = elm.querySelector('#lisp');
    obj.caches['md'] = elm.querySelector('#md');
    obj.container = elm.querySelector('#editor');
    if (!obj.container) {
      obj.container = document.createElement('div');
      obj.container.id = 'editor';
      elm.appendChild(obj.container);
    }
    obj.output = elm.querySelector('#output');
    if (!obj.output) {
      obj.output = document.createElement('div');
      obj.output.id = 'output';
      elm.appendChild(obj.output);
    }
    obj.editor = EditCell.createEditor(obj.container);
    obj.editor.addEventListener('focus', (e) => {window.currentCell = obj.cell.id;});
    let lang = elm.dataset.lang;
    if (lang) {
      obj.editor.getSession().setValue(obj.caches[lang].dataset.content);
    }
    window.editcells[elm.id] = obj;
    return obj;
  }

  static createElement(id, before = '') {
    let obj = new EditCell();
    obj.cell = document.createElement('div');
    obj.cell.id = id;
    obj.cell.className = 'editcell';
    obj.cell.dataset.lang = 'lisp';
    obj.cell.dataset.next = '';
    obj.cell.dataset.before = before;
    let lispCache = document.createElement('div');
    lispCache.id = 'lisp';
    lispCache.className = 'cache';
    lispCache.dataset.content = '';
    obj.caches['lisp'] = lispCache;
    let mdCache = document.createElement('div');
    mdCache.id = 'md';
    mdCache.className = 'cache'
    mdCache.dataset.content = '';
    obj.caches['md'] = mdCache;
    obj.container = document.createElement('div');
    obj.container.id = 'editor';
    obj.editor = EditCell.createEditor(obj.container);
    obj.editor.addEventListener('focus', (e) => {window.currentCell = obj.cell.id;});
    obj.output = document.createElement('div');
    obj.output.id = 'output';

    obj.cell.appendChild(obj.caches['lisp']);
    obj.cell.appendChild(obj.caches['md']);
    obj.cell.appendChild(obj.container);
    obj.cell.appendChild(obj.output);
    window.editcells[id] = obj;
    return obj;
  }

  changeLang() {
    switch (this.cell.dataset.lang) {
      case 'lisp':
        this.cell.dataset.lang = 'md';
        this.editor.getSession().setMode('ace/mode/markdown');
        break;
      case 'md':
        this.cell.dataset.lang = 'lisp';
        this.editor.getSession().setMode('ace/mode/lisp');
        break;
      default:
        this.cell.dataset.lang = 'lisp';
        this.editor.getSession().setMode('ace/mode/lisp');
        break;
    }
  }

  get lang() {
    return this.cell.dataset.lang;
  }

  get value() {
    return this.editor.getValue();
  }

  setValue(v) {
    this.editor.getSession().setValue(v);
  }

  get element() {
    return this.cell;
  }

  get outputElement() {
    return this.output;
  }

  get before() {
    return this.cell.dataset.before;
  }

  get next() {
    return this.cell.dataset.next;
  }

  get id() {
    return this.cell.id;
  }

  eval(callback = null) {
    let contents = this.editor.getValue();
    switch (this.lang) {
      case 'lisp':
        ls.eval(contents, this.outputElement, callback, this);
        break;
      case 'md':
        ls.markdown(contents, this.outputElement, callback, this);
        break;
    }
  }

  addCellToAbove(ls, parent) {
    let instance = EditCell.createElement(Date.now().toString(), this.before);
    instance.element.dataset.next = this.id;
    let before = null;
    if (this.before !== '') {
      before = document.getElementById(this.before);
      before.dataset.next = instance.id;
    }
    this.element.dataset.before = instance.id;
    parent.insertBefore(instance.element, this.element);
    instance.editor.focus();
    EditCell.connect(ls, instance, parent);
  }

  addCellToBelow(ls, parent) {
    let instance = EditCell.createElement(Date.now().toString(), this.id);
    let next = null;
    if (this.next !== '') {
      instance.element.dataset.next = this.next;
      next = document.getElementById(this.next);
      next.dataset.before = instance.id;
    }
    this.element.dataset.next = instance.id;
    parent.insertBefore(instance.element, this.element.nextSibling);
    instance.editor.focus();
    EditCell.connect(ls, instance, parent);
  }

  static connect(ls, ec, parent) {
    ec.editor.addEventListener('change', (e) => {
      ls.modified = true;
    });
			ec.container.addEventListener('keydown', (e) => {
				if (e.keyCode === 13) {
          if (e.ctrlKey || e.shiftKey) {
            ec.output.className = "show";
  					let contents = ec.editor.getValue();
            console.log('send: '+ contents);
  					if (contents.trim() === '') {
  						if (ec.next && e.shiftKey) {
  							document.getElementById(ec.next).editor.focus();
  						}
  					} else {
              ec.eval();
              if (e.shiftKey) {
                if (ec.next === undefined || ec.next === '') {
                  let instance = EditCell.createElement(Date.now().toString(), ec.before);
                  parent.appendChild(instance.element);
                  EditCell.connect(ls, instance, parent);
                  ec.cell.dataset.next = instance.id;
                  instance.editor.focus();
                  parent.scrollTop = instance.offsetTop - 82 - 200;
                } else {
                  let cell = window.editcells[ec.next];
                  cell.editor.focus();
                  parent.scrollTop = cell.element.offsetTop - 82 - 200;
                }
              }
            }
					  e.preventDefault();
          }
				} else if (e.ctrlKey && e.keyCode === 73) {
          console.log(ec.caches);
          console.log(ec.lang);
          ec.caches[ec.lang].dataset.content = ec.value;
          ec.changeLang();
          ec.setValue(ec.caches[ec.lang].dataset.content);
          e.preventDefault();
        } else if (e.keyCode === 38 && e.ctrlKey) {
          e.preventDefault();
          if (e.shiftKey) {
            swapPrevCell();
            ls.modified = true;
          } else {
            focusPrevCell();
          }
        } else if (e.keyCode === 40 && e.ctrlKey) {
          e.preventDefault();
          if (e.shiftKey) {
            swapNextCell();
            ls.modified = true;
          } else {
           focusNextCell();
          }
        } else if (e.keyCode === 80 && e.ctrlKey) {
          e.preventDefault();
          addCellToCurrentAbove();
        } else if (e.keyCode === 66 && e.ctrlKey) {
          e.preventDefault();
          addCellToCurrentBelow();
        } else if (e.keyCode === 46 && e.ctrlKey) {
          e.preventDefault();
          removeCurrentCell();
        }
			}, true);
		}
}


