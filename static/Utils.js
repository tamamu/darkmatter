
function closeAllSubMenu() {
  let submenus = document.getElementsByName('menuitem');
  for (let check of submenus) {
    check.checked = false;
  }
}

function attachCloseFunction() {
  let menuitems = document.querySelectorAll('.submenu li');
  for (let item of menuitems) {
    item.addEventListener('click', closeAllSubMenu);
  }
}

function saveFile() {
  Socket.save(document.getElementsByClassName('cell'));
}

function evalCurrent(forward = false) {
  if (CurrentCell) {
    Cells[CurrentCell].eval().then((cell) => {
      if (cell.next === undefined || cell.next === '') {
        let nextCell = cell.appendCell();
        if (forward) {
          nextCell.editor.focus();
          adjustScroll(nextCell);
        }
      } else {
        let nextCell = Cells[cell.next];
        if (forward) {
          nextCell.editor.focus();
          adjustScroll(nextCell);
        }
      }
    });
  }
}

function evalCurrentForward() {
  evalCurrent(true);
}

function changeLangCurrent() {
  if (CurrentCell) {
    Cells[CurrentCell].changeLang();
  }
}

function prependCellCurrent() {
  if (CurrentCell) {
    Cells[CurrentCell].prependCell();
  }
}

function appendCellCurrent() {
  if (CurrentCell) {
    Cells[CurrentCell].appendCell();
  }
}

function evalAllCells() {
  let elements = document.getElementsByClassName('cell');
  if (elements.length <= 0) return;

  let first = Cells[elements[0].id];
  first.eval().then((cell) => {
    evalNextCell(first);
  });
}

function evalNextCell(cell) {
  if (cell.next !== '' && cell.editor.getValue().trim() !== '') {
    Cells[cell.next].eval().then((cell) => {
      evalNextCell(cell);
    });
  }
}

function swapCell(cell1, cell2) {
  let next2 = cell2.dataset.next;
  let prev1 = cell1.dataset.prev;
  cell1.dataset.prev = cell2.id;
  cell2.dataset.next = cell1.id;
  cell1.dataset.next = next2;
  cell2.dataset.prev = prev1;
  let parent = cell1.parentElement;
  parent.removeChild(cell2);
  parent.insertBefore(cell2, cell1);
}

function adjustScroll(cell) {
  let e = cell.element;
  e.parentElement.scrollTop =
    e.offsetTop - Math.min(window.innerHeight/2 + e.clientHeight, 200);
}

function swapToNextCell() {
  if (CurrentCell) {
    let cell = Cells[CurrentCell];
    if (cell.next !== '') {
      let nextCell = Cells[cell.next];
      swapCell(cell.element, nextCell.element);
      cell.editor.focus();
      adjustScroll(cell);
    }
  }
}

function swapToPrevCell() {
  if (CurrentCell) {
    let cell = Cells[CurrentCell];
    if (cell.prev !== '') {
      let prevCell = Cells[cell.prev];
      swapCell(prevCell.element, cell.element);
      cell.editor.focus();
      adjustScroll(cell);
    }
  }
}

function removeCurrent() {
  if (CurrentCell) {
    let cell = Cells[CurrentCell];
    let prev = cell.prev;
    let next = cell.next;
    if (next && Cells[next]) {
      Cells[next].element.dataset.prev = prev;
      Cells[next].editor.focus();
    }
    if (prev && Cells[prev]) {
      Cells[prev].element.dataset.next = next;
      Cells[prev].editor.focus();
    }
    cell.element.parentElement.removeChild(cell.element);
  }
}

function focusNextCell() {
  if (CurrentCell) {
    let cell = Cells[CurrentCell];
    if (cell.next) {
      let nextCell = Cells[cell.next];
      nextCell.editor.focus();
      adjustScroll(nextCell);
    }
  }
}
function focusPrevCell() {
  if (CurrentCell) {
    let cell = Cells[CurrentCell];
    if (cell.prev) {
      let prevCell = Cells[cell.prev];
      prevCell.editor.focus();
      adjustScroll(prevCell);
    }
  }
}

function setKeyBind(mode) {
  for (let cell in Cells) {
    Cells[cell].changeKeyBind(mode);
  }
}
