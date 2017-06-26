
let SlideMode = false;

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

function startSlideMode() {
  if (!SlideMode) {
    SlideMode = true;
    if (CurrentCell === null || CurrentCell === '') {
      let elements = document.getElementsByClassName('cell');
      if (elements.length > 0) {
        CurrentCell = elements[0].id;
      } else {
        SlideMode = false;
        return;
      }
    }
    if (document.activeElement) {
      document.activeElement.blur();
    }
    showSlideController();
    let cell = Cells[CurrentCell];
    cell.output.className = 'show slide';
  }
}

function exitSlideMode() {
  if (SlideMode) {
    let cell = Cells[CurrentCell];
    SlideMode = false;
    cell.editor.focus();
    hideSlideController();
    for (id in Cells) {
      closeCellOutput(Cells[id]);
    }
    adjustScroll(cell);
  }
}

function nextSlide() {
  if (SlideMode) {
    let cell = Cells[CurrentCell];
    CurrentCell = cell.next;
    closeCellOutput(cell);
    if (CurrentCell !== null && CurrentCell !== '') {
      let next = Cells[CurrentCell];
      let nextOutput = next.outputElement;
      if (nextOutput.className.includes('show')) {
        nextOutput.className = 'show slide';
      } else {
        if (next.next === null || next.next === '') {
          exitSlideMode();
        } else {
          nextSlide();
        }
      }
    } else {
      CurrentCell = cell.id;
      exitSlideMode();
    }
  }
}

function prevSlide() {
  if (SlideMode) {
    let cell = Cells[CurrentCell];
    CurrentCell = cell.prev;
    if (CurrentCell === null || CurrentCell === '') {
      CurrentCell = cell.id;
      return;
    }
    closeCellOutput(cell);
    let prevCell = Cells[CurrentCell];
    let output = prevCell.output;
    if (output.className.includes('show')) {
      output.className = 'show slide';
    } else {
      prevSlide();
    }
  }
}

function closeCellOutput(cell) {
  let output = cell.output;
  if (output.className.includes('show')) {
    output.className = 'show';
  } else {
    output.className = '';
  }
}

