function showSlideController() {
  let controllers = document.querySelectorAll('.slide-controller');
  for (c of controllers) {
    c.className = 'slide-controller show';
  }
}

function hideSlideController() {
  let controllers = document.querySelectorAll('.slide-controller');
  for (c of controllers) {
    c.className = 'slide-controller';
  }
}

function slideKeyListener(e) {
  if (e.keyCode === 27) {
    exitSlideMode();
    e.preventDefault();
  } else if (e.keyCode === 37) {
    prevSlide();
    e.preventDefault();
  } else if (e.keyCode === 39 || e.keyCode === 32 || e.keyCode === 13) {
    nextSlide();
    e.preventDefault();
  }
}

function startSlideMode() {
  if (!window.slideMode) {
    window.slideMode = true;
    let editcells = getEditCells();
    window.slideId = getCurrentCellId();
    if (window.slideId === null || window.slideId === '') {
      if (editcells.length > 0) {
        window.slideId = editcells[0].id;
      } else {
        return;
      }
    }
    window.prevActiveElement = document.activeElement;
    if (window.prevActiveElement) {
      window.prevActiveElement.blur();
    }
    showSlideController();
    let cell = editcells[window.slideId];
    cell.querySelector('#output').className = 'show slide';
    window.addEventListener('keydown', slideKeyListener);
  }
}

function exitSlideMode() {
  if (window.slideMode) {
    window.slideMode = false;
    window.prevActiveElement.focus();
    window.removeEventListener('keydown', slideKeyListener);
    hideSlideController();
    let cells = getEditCells();
    for (cell of cells) {
      exitCellOutput(cell);
    }
  }
}

function nextSlide() {
  if (window.slideMode) {
    let cell = getEditCells()[window.slideId];
    window.slideId = cell.dataset.next;
    exitCellOutput(cell);
    if (window.slideId !== null && window.slideId !== '') {
      let next = window.editcells[window.slideId];
      let nextOutput = next.outputElement;
      if (nextOutput.className.includes('show')) {
        nextOutput.className = 'show slide';
      } else {
        nextSlide();
      }
    } else {
      exitSlideMode();
    }
  }
}

function prevSlide() {
  if (window.slideMode) {
    let cells = getEditCells();
    let cell = cells[window.slideId];
    window.slideId = cell.dataset.before;
    if (window.slideId === null || window.slideId === '') {
      window.slideId = cell.id;
      return;
    }
    exitCellOutput(cell);
    let prevCell = cells[window.slideId];
    let output = prevCell.querySelector('#output');
    if (output.className.includes('show')) {
      output.className = 'show slide';
    } else {
      prevSlide();
    }
  }
}

function exitCellOutput(cell) {
  let output = cell.querySelector('#output');
  if (output.className.includes('show')) {
    output.className = 'show';
  } else {
    output.className = '';
  }
}

