class Renderer {
  constructor() {
    this.methods = {};
  }

  registRenderMethod(name, method) {
    this.methods[name] = method;
  }

  render(methodName, src) {
    return this.methods[methodName].render(src);
  }
}
