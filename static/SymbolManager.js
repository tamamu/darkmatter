
class SymbolManager {
  constructor(container) {
    this.symbolFunction = {};
    this.symbolMacro = {};
    this.symbolClass = {};
    this.symbolVariable = {};
    this.function = container.querySelector('#function');
    this.macro = container.querySelector('#macro');
    this.class = container.querySelector('#class');
    this.variable = container.querySelector('#variable');
  }

  static createElement(name) {
    let element = document.createElement('li');
    element.dataset.name = name;
    let label = document.createElement('label');
    label.innerText = name;
    element.appendChild(label);
    let check = document.createElement('input');
    check.className = "hidden";
    check.name = "symbol";
    check.type = "checkbox";
    label.appendChild(check);
    let content = document.createElement('div');
    label.appendChild(content);
    return [element, content];
  }

  appendFunction(name, symbol) {
    if (!this.symbolFunction[name]) {
      this.symbolFunction[name] = symbol;
    }
    let obj = this.symbolFunction[name];
    let element = obj.element;
    let content = obj.content;
    if (!element) {
      [element, content] = SymbolManager.createElement(name);
      obj.element = element;
      obj.content = content;
      this.function.appendChild(element);
    }
    let doc = content.querySelector('#doc');
    if (!doc) {
      doc = document.createElement('p');
      doc.id = "doc";
      content.appendChild(doc);
    }
    doc.innerText = symbol.doc;
    let args = content.querySelector('#args');
    if (!args) {
      args = document.createElement('ul');
      args.id = "args";
      content.appendChild(args);
    }
    while (args.firstChild) {
      args.removeChild(args.firstChild);
    }
    for (let arg of symbol.arguments) {
      let argument = document.createElement('li');
      argument.innerText = arg;
      args.appendChild(argument);
    }
  }

  appendMacro(name, symbol) {
    if (!this.symbolMacro[name]) {
      this.symbolMacro[name] = symbol;
    }
    let obj = this.symbolMacro[name];
    let element = obj.element;
    let content = obj.content;
    if (!element) {
      [element, content] = SymbolManager.createElement(name);
      obj.element = element;
      obj.content = content;
      this.macro.appendChild(element);
    }
    let doc = content.querySelector('#doc');
    if (!doc) {
      doc = document.createElement('p');
      doc.id = "doc";
      content.appendChild(doc);
    }
    doc.innerText = symbol.doc;
    let args = content.querySelector('#args');
    if (!args) {
      args = document.createElement('ul');
      args.id = "args";
      content.appendChild(args);
    }
    while (args.firstChild) {
      args.removeChild(args.firstChild);
    }
    for (let arg of symbol.arguments) {
      let argument = document.createElement('li');
      argument.innerText = arg;
      args.appendChild(argument);
    }
  }

  appendClass(name, symbol) {
    if (!this.symbolClass[name]) {
      this.symbolClass[name] = symbol;
    }
    let obj = this.symbolClass[name];
    let element = obj.element;
    let content = obj.content;
    if (!element) {
      [element, content] = SymbolManager.createElement(name);
      obj.element = element;
      obj.content = content;
      this.class.appendChild(element);
    }
    let doc = content.querySelector('#doc');
    if (!doc) {
      doc = document.createElement('p');
      doc.id = "doc";
      content.appendChild(doc);
    }
    doc.innerText = symbol.doc;
  }

  appendVariable(name, symbol) {
    if (!this.symbolVariable[name]) {
      this.symbolVariable[name] = symbol;
    }
    let obj = this.symbolVariable[name];
    let element = obj.element;
    let content = obj.content;
    if (!element) {
      [element, content] = SymbolManager.createElement(name);
      obj.element = element;
      obj.content = content;
      this.variable.appendChild(element);
    }
    let doc = content.querySelector('#doc');
    if (!doc) {
      doc = document.createElement('p');
      doc.id = "doc";
      content.appendChild(doc);
    }
    doc.innerText = symbol.doc;
  }

  updateSymbols(symbols) {
    for (let key in symbols) {
//      let symbol = Object.assign({element: null, content: null}, symbols[key]);
      let symbol = symbols[key];
      switch (symbol.type) {
        case 'function':
          this.appendFunction(key, symbol);
          break;
        case 'macro':
          this.appendMacro(key, symbol);
          break;
        case 'class':
          this.appendClass(key, symbol);
          break;
        case 'variable':
          this.appendVariable(key, symbol);
          break;
      }
    }
  }
}
