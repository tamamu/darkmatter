window.onload = () => {
  let connector = document.getElementById('indicator');
  window.ls = new LispSocket(LS_URI, FILE_PATH, connector);
  window.onkeydown = (e) => {
    if (e.keyCode === 83 && e.ctrlKey) {
      e.preventDefault();
      window.ls.save(document.getElementsByClassName('editcell'));
      return false;
    } else if (e.keyCode === 82 && e.ctrlKey) {
      e.preventDefault();
      window.ls.recall();
    } else if (e.keyCode === 116 && e.ctrlKey) {
      e.preventDefault();
      startSlideMode();
    } else if (e.keyCode === 120 && e.ctrlKey) {
      e.preventDefault();
      evalAllCells();
    }
  }
  window.editcells = {};
  let cells = document.getElementsByClassName('editcell');
  let container = document.getElementById('dm-container');
  let before = null;
  for (let i = 0; i < cells.length; i++) {
    let cell = cells[i];
    let instance = EditCell.fromElement(cell);
    if (before && instance.before === '') {
      instance.element.dataset.before = before.id;
    }
    if (instance.next === '' && i < cells.length-1) {
      instance.element.dataset.next = cells[i+1].dataset.id;
    }
    window.editcells[cell.id] = instance;
    EditCell.connect(window.ls, instance, container);
    before = instance;
  }
  if (cells.length > 0) {
    let lastcell = cells[cells.length-1];
    if (lastcell.querySelector('#lisp').innerHTML !== ''
     || lastcell.querySelector('#md').innerHTML !== ''
     || lastcell.querySelector('#output').innerHTML !== '') {
      let instance = EditCell.createElement(Date.now().toString());
      cells[cells.length-1].dataset.next = instance.id;
      instance.element.dataset.before = cells[cells.length-1].id;
      container.appendChild(instance.element);
      EditCell.connect(window.ls, instance, container);
    }
  } else {
    let instance = EditCell.createElement(Date.now().toString());
    container.appendChild(instance.element);
    EditCell.connect(window.ls, instance, container);
  }
}
