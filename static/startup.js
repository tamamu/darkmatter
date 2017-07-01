
let Socket = new LispSocket(HTTP_URI, LS_URI, FILE_PATH, TOKEN);
let Loaded = false;


window.onload = () => {
  let container = document.getElementById('dm-container');
  let renderer = new Renderer();
  let lispRenderer = new LispRenderer(Socket);
  let mdRenderer = new MDRenderer();
  mdRenderer.attachLispRenderer(lispRenderer);
  renderer.registRenderMethod('lisp', lispRenderer);
  renderer.registRenderMethod('md', mdRenderer);
  fetchKeyBind('keybind');
  window.addEventListener('keydown', HandlingKeyBind, true);
  initCellAll(renderer).then(() => {
    appendLastCell(renderer, container);
    Loaded = true;
  });
  attachCloseFunction();

}

function loadCellsProgress(max, drawer, resolve) {
  return (self) => {
    let per = Math.floor(Object.keys(Cells).length / max * 100);
    drawer(per);
    if (per < 100) {
      setTimeout(self, 100, self);
    } else {
      resolve();
    }
  }
}

function initCell(id, renderer) {
  let element = document.getElementById(id);
  let instance = Cell.fromElement(element);
  instance.attachRenderer(renderer);
  Cells[id] = instance;
}

function initCellAll(renderer) {
  return new Promise((resolve, reject) => {
  let elements = document.getElementsByClassName('cell');
  let prev = null;
  let delay = 50;
  let progress = loadCellsProgress(elements.length, console.log, resolve);
  setTimeout(progress, 100, progress);
  if (LAZY_LOAD) {
    for (let i = 0; i < elements.length; i++) {
      let id = elements[i].id;
      setTimeout(initCell, delay*i, id, renderer);
    }
  } else {
    for (let i = 0; i < elements.length; i++) {
      let cell = elements[i];
      let instance = Cell.fromElement(cell);
      instance.attachRenderer(renderer);
      if (prev && instance.prev === '') {
        instance.element.dataset.prev = prev.id;
      }
      if (instance.next === '' && i < elements.length-1) {
        instance.element.dataset.next = elements[i+1].id;
      }
      Cells[cell.id] = instance;
      prev = instance;
    }
  }
  });
}

function appendLastCell(renderer, container) {
  let elements = document.getElementsByClassName('cell');
  if (elements.length > 0) {
    let lastcell = Cells[elements[elements.length-1].id];
    if (lastcell.sources.lisp.innerHTML !== ''
     || lastcell.sources.md.innerHTML !== ''
     || lastcell.output.innerHTML !== '') {
      let instance = Cell.createElement(Date.now().toString());
      instance.attachRenderer(renderer);
      lastcell.element.dataset.next = instance.id;
      instance.element.dataset.prev = lastcell.id;
      container.appendChild(instance.element);
    }
  } else {
    let instance = Cell.createElement(Date.now().toString());
    instance.attachRenderer(renderer);
    container.appendChild(instance.element);
  }
}
