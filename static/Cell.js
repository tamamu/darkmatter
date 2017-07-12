
let Cells = {};
let CurrentCell = null;
ace.config.loadModule('ace/keybinding/vim', (m) => {
  let VimApi = require('ace/keyboard/vim').Vim;
  VimApi.defineEx('write', 'w', () => {
    saveFile();
  });
});

class Cell {
  constructor(renderer) {
    this.element = null;
    this.sources = {};
    this.editorElement = null;
    this.editor = null;
    this.output = null;
    this.renderer = renderer;
    this.keyboardHandler = null;
  }

  attachRenderer(renderer) {
    this.renderer = renderer;
  }

  render(returnValue, result) {
    console.log(result);
    this.output.innerHTML = returnValue?`<div id="result">${returnValue}</div>`:"";
    if (result instanceof Element) {
      this.output.appendChild(result);
    } else {
      this.output.innerHTML += result;
    }
    if (this.output.innerHTML.length>0) {
      this.output.className = 'show';
      this.element.classList.add('success');
    }
  }

  eval() {
    Modified = true;
    this.element.classList.remove('success');
    this.element.classList.add('oneval');
    if (this.renderer) {
      return new Promise((resolve, reject) => {
        this.renderer
            .render(this.lang, this.value, this.id)
            .then((obj) => {
              if (obj.rendering) {
                this.render(obj.returnValue, obj.result);
              }
              this.element.classList.remove('oneval');
              resolve(this);
            });
      });
    } else {
      return new Promise((resolve, reject) => {
        this.element.classList.remove('oneval');
        resolve(this.output.innerHTML = src);
      });
    }
  }

  static createEditor(elm, lang = 'lisp') {
    let editor = ace.edit(elm);
    editor.setTheme('ace/theme/solarized_dark');
    editor.setOptions({maxLines: Infinity, tabSize: 2});
    editor.setFontSize(14);
    editor.$blockScrolling = Infinity;
    editor.addEventListener('change', () => {Modified=true;});
    for (bind of KeyBindings) {
      if (bind.global) editor.commands.bindKey(bind.scheme, null);
    }
    let session = editor.getSession();
    switch (lang) {
      case 'lisp':
        session.setMode('ace/mode/common_lisp');
        break;
      case 'md':
        session.setMode('ace/mode/markdown');
        break;
      default:
        session.setMode('ace/mode/common_lisp');
        break;
    }
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
    obj.editor = Cell.createEditor(obj.editorElement, obj.element.dataset.lang);
    obj.editor.addEventListener('focus', (e) => {CurrentCell = obj.element.id;});
    obj.keyboardHandler = obj.editor.getKeyboardHandler();
    let lang = elm.dataset.lang;
    if (lang) {
      obj.editor.getSession().setValue(obj.sources[lang].dataset.content);
    }
    Cells[elm.id] = obj;
    return obj;
  }

  static createElement(id, prev = '', lang = 'lisp') {
    let obj = new Cell();
    obj.element = document.createElement('div');
    obj.element.id = id;
    obj.element.className = 'cell';
    obj.element.dataset.lang = lang;
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
    obj.editor = Cell.createEditor(obj.editorElement, lang);
    obj.editor.addEventListener('focus', (e) => {CurrentCell = obj.element.id;});
    obj.keyboardHandler = obj.editor.getKeyboardHandler();
    obj.output = document.createElement('div');
    obj.output.id = 'output';

    obj.element.appendChild(obj.sources['lisp']);
    obj.element.appendChild(obj.sources['md']);
    obj.element.appendChild(obj.editorElement);
    obj.element.appendChild(obj.output);
    Cells[id] = obj;
    return obj;
  }

  changeKeyBind(mode) {
    switch (mode) {
      case 'vim':
        this.editor.setKeyboardHandler('ace/keyboard/vim');
        break;
      case 'emacs':
        this.editor.setKeyboardHandler('ace/keyboard/emacs');
        break;
      default:
        this.editor.setKeyboardHandler(this.keyboardHandler);;
        break;
    }
  }

  changeLang() {
    this.sources[this.lang].innerHTML = this.value;
    switch (this.lang) {
      case 'lisp':
        this.element.dataset.lang = 'md';
        this.editor.getSession().setMode('ace/mode/markdown');
        break;
      case 'md':
        this.element.dataset.lang = 'lisp';
        this.editor.getSession().setMode('ace/mode/common_lisp');
        break;
      default:
        this.element.dataset.lang = 'lisp';
        this.editor.getSession().setMode('ace/mode/common_lisp');
        break;
    }
    this.editor.getSession().setValue(this.sources[this.lang].innerHTML);
  }

  prependCell() {
    let instance = Cell.createElement(Date.now().toString(), this.prev, this.lang);
    instance.attachRenderer(this.renderer);
    instance.element.dataset.next = this.id;
    let prev = null;
    if (this.prev !== '') {
      prev = document.getElementById(this.prev);
      prev.dataset.next = instance.id;
    }
    this.element.dataset.prev = instance.id;
    this.element.parentElement.insertBefore(instance.element, this.element);
    return instance;
  }

  appendCell() {
    let instance = Cell.createElement(Date.now().toString(), this.id, this.lang);
    instance.attachRenderer(this.renderer);
    let next = null;
    if (this.next !== '') {
      instance.element.dataset.next = this.next;
      next = document.getElementById(this.next);
      next.dataset.prev = instance.id;
    }
    this.element.dataset.next = instance.id;
    this.element.parentElement.insertBefore(instance.element, this.element.nextSibling);
    return instance;
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
