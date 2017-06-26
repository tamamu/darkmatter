
let Cells = {};

class Cell {
  constructor(renderer) {
    this.element = null;
    this.sources = {};
    this.editorElement = null;
    this.editor = null;
    this.output = null;
    this.renderer = renderer;
  }

  attachRenderer(renderer) {
    this.renderer = renderer;
  }

  eval() {
    if (this.renderer) {
      return this.renderer
                 .render(this.lang, this.value)
                 .then((obj) => {
                   this.output.innerHTML = obj.returnValue?`<div id="return">${obj.returnValue}</div>`:"";
                   this.output.innerHTML += obj.result;
                 });
    } else {
      return new Promise((resolve, reject) => {
        resolve(this.output.innerHTML = src);
      });
    }
  }

  static createEditor(elm) {
    let editor = ace.edit(elm);
    editor.setTheme('ace/theme/monokai');
    editor.setOptions({maxLines: Infinity, tabSize: 2});
    editor.setFontSize(14);
    editor.$blockScrolling = Infinity;
    let session = editor.getSession();
    session.setMode('ace/mode/lisp');
    session.setUseSoftTabs(true);
    session.setUseWrapMode(true);
    return editor;
  }

  static fromElement(elm) {
    let obj = new Cell();
    obj.element = elm;
    if (obj.element.dataset.next === undefined) {
      obj.element.dataset.next = '';
    }
    if (obj.element.dataset.prev == undefined) {
      obj.element.dataset.prev = '';
    }
    obj.sources = {};
    obj.sources['lisp'] = elm.querySelector('#lisp');
    obj.sources['md'] = elm.querySelector('#md');
    obj.editorElement = elm.querySelector('#editor');
    if (!obj.editorElement) {
      obj.editorElement = document.createElement('div');
      obj.editorElement.id = 'editor';
      elm.appendChild(obj.editorElement);
    }
    obj.output = elm.querySelector('#output');
    if (!obj.output) {
      obj.output = document.createElement('div');
      obj.output.id = 'output';
      elm.appendChild(obj.output);
    }
    obj.editor = Cell.createEditor(obj.editorElement);
    obj.editor.addEventListener('focus', (e) => {window.currentCell = obj.element.id;});
    let lang = elm.dataset.lang;
    if (lang) {
      obj.editor.getSession().setValue(obj.sources[lang].dataset.content);
    }
    Cells[elm.id] = obj;
    return obj;
  }

  static createElement(id, prev = '') {
    let obj = new Cell();
    obj.element = document.createElement('div');
    obj.element.id = id;
    obj.element.className = 'editelement';
    obj.element.dataset.lang = 'lisp';
    obj.element.dataset.next = '';
    obj.element.dataset.prev = prev;
    let lispCache = document.createElement('div');
    lispCache.id = 'lisp';
    lispCache.className = 'cache';
    lispCache.dataset.content = '';
    obj.sources['lisp'] = lispCache;
    let mdCache = document.createElement('div');
    mdCache.id = 'md';
    mdCache.className = 'cache'
    mdCache.dataset.content = '';
    obj.sources['md'] = mdCache;
    obj.editorElement = document.createElement('div');
    obj.editorElement.id = 'editor';
    obj.editor = Cell.createEditor(obj.editorElement);
    obj.editor.addEventListener('focus', (e) => {window.currentCell = obj.element.id;});
    obj.output = document.createElement('div');
    obj.output.id = 'output';

    obj.element.appendChild(obj.sources['lisp']);
    obj.element.appendChild(obj.sources['md']);
    obj.element.appendChild(obj.editorElement);
    obj.element.appendChild(obj.output);
    Cells[id] = obj;
    return obj;
  }

  changeLang() {
    switch (this.element.dataset.lang) {
      case 'lisp':
        this.element.dataset.lang = 'md';
        this.editor.getSession().setMode('ace/mode/markdown');
        break;
      case 'md':
        this.element.dataset.lang = 'lisp';
        this.editor.getSession().setMode('ace/mode/lisp');
        break;
      default:
        this.element.dataset.lang = 'lisp';
        this.editor.getSession().setMode('ace/mode/lisp');
        break;
    }
  }

  get lang() {
    return this.element.dataset.lang;
  }

  get value() {
    return this.editor.getValue();
  }

  setValue(v) {
    this.editor.getSession().setValue(v);
  }

  get outputElement() {
    return this.output;
  }

  get prev() {
    return this.element.dataset.prev;
  }

  get next() {
    return this.element.dataset.next;
  }

  get id() {
    return this.element.id;
  }



}
