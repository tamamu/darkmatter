
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
    let lang = elm.dataset.lang;
    if (lang) {
      obj.editor.getSession().setValue(obj.caches[lang].dataset.content);
    }
    window.editcells[elm.id] = obj;
    return obj;
  }

  static createElement(id) {
    let obj = new EditCell();
    obj.cell = document.createElement('div');
    obj.cell.id = id;
    obj.cell.className = 'editcell';
    obj.cell.dataset.lang = 'lisp';
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
        break;
      case 'md':
        this.cell.dataset.lang = 'lisp';
        break;
      default:
        this.cell.dataset.lang = 'lisp';
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

  get next() {
    return this.cell.dataset.next;
  }

  get id() {
    return this.cell.id;
  }

  static connect(ls, ec, parent) {
			ec.container.onkeydown = (e) => {
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
              switch (ec.cell.dataset.lang) {
                case 'lisp':
                  console.log('lisp evaluated');
                  ls.eval(contents, ec.output);
                  break;
                case 'md':
                  console.log('katex evaluated');
                  katex.render(contents, ec.output, {
                    displayMode: true
                  });
                  break;
              }
              if (e.shiftKey) {
                if (ec.next === undefined || ec.next === '') {
                  let instance = EditCell.createElement(Date.now().toString());
                  parent.appendChild(instance.element);
                  EditCell.connect(ls, instance, parent);
                  ec.cell.dataset.next = instance.id;
                  instance.editor.focus();
                } else {
                  window.editcells[ec.next].editor.focus();
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
        }
        if (!(e.ctrlKey && e.keyCode === 83)) {
          ls.modified = true;
        }
			};
		}
}
