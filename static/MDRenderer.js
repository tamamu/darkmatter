class MDRenderer {
  constructor() {
    this.marked = new marked.Renderer();
    this.lispRenderer = null;
    this.init();
  }

  attachLispRenderer(lispRenderer) {
    this.lispRenderer = lispRenderer;
  }

  init() {
    let originalCode = this.marked.code.bind(this.marked);
    this.marked.code = (code, lang, escaped) => {
      let math;
      if (!lang && (math = this.renderLatex(code))) {
        return math;
      }
      return originalCode(code, lang, escaped);
    };
    let originalCodeSpan = this.marked.codespan.bind(this.marked);
    this.marked.codespan = (text) => {
      let math;
      if (math = this.renderLatex(text)) {
        return math;
      }
      return originalCodeSpan(text);
    };
    let originalImage = this.marked.image.bind(this.marked);
    let originalLink = this.marked.link.bind(this.marked);
    let get_uri = (href) => {
      if (href.startsWith('/')) {
        return `${HTTP_URI}/browse${href}`;
      } else if (href.startsWith('http://')) {
        return href;
      } else {
        return `${CD_URI}${href}`;
      }
    }
    this.marked.image = (href, title, text) => {
      return originalImage(get_uri(href), title, text);
    }
    this.marked.link = (href, title, text) => {
      return originalLink(get_uri(href), title, text);
    }
  }

  renderLisp(expr, cellId) {
    return replaceAsyncAll(expr, /%%(.*?)%%/, (match) => {
      return new Promise((resolve, reject) => {
        this.lispRenderer.render(match, cellId).then(obj => resolve(obj.returnValue));
      });
    });
  }

  renderLatex(expr) {
    if (expr[0] === '$' && expr[expr.length-1] === '$') {
      let displayStyle = false;
      expr = expr.substr(1, expr.length-2);
      if (expr[0] === '$' && expr[expr.length-1] === '$') {
        displayStyle = true;
        expr = '\\displaystyle ' + expr.substr(1, expr.length-2);
      }
      let html;
      try {
        html = katex.renderToString(expr, {throwOnError: false});
      } catch (e) {
        html = e;
      }
      if (displayStyle) {
        html = html.replace(/class=\"katex\"/g, 'class="katex katex-block" style="display: block;"');
      }
      return html;
    } else {
      return null;
    }
  }

  renderMarkdown(expr) {
    return new Promise((resolve, reject) => {
      let result = marked(expr, {breaks: true, renderer: this.marked});
      resolve({rendering: true, returnValue: null, result: result});
    });
  }

  render(src, cellId) {
    return this.renderLisp(src, cellId).catch(expr => this.renderMarkdown(expr));
  }
}

function replaceAsync(src, regex, replacer) {
  return new Promise((resolve, reject) => {
    let match = regex.exec(src);
    if (match) {
      replacer(match[1]).then(replaced => {
        resolve(src.replace(match[0], replaced));
      });
    } else {
      reject(src);
    }
  });
}

function replaceAsyncAll(src, regex, replacer) {
  return replaceAsync(src, regex, replacer).then(replaced => replaceAsyncAll(replaced, regex, replacer));
}
