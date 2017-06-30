
let KeyBindings = [];

function HandlingKeyBind(e) {
  let input = {Alt: e.altKey, Shift: e.shiftKey, Ctrl: e.ctrlKey || e.metaKey,
               name: e.key.toUpperCase(), code: e.code};
  for (bind of KeyBindings) {
    if (bind.key.Alt   == input.Alt &&
        bind.key.Shift == input.Shift &&
        bind.key.Ctrl  == input.Ctrl &&
        (bind.key.name == input.name ||
        bind.key.code == input.code)) {
      e.preventDefault();
      bind.handler.click();
      break;
    }
  }
}

function fetchKeyBind(className) {
  let binds = document.getElementsByClassName(className);
  for (bind of binds) {
    let kb = parseKeyBind(bind.dataset.key);
    KeyBindings.push({scheme: bind.dataset.key, handler: bind,
                      key: kb, global: bind.dataset.global===undefined});
  }
}

function parseKeyBind(bind) {
  let keys = bind.split('-');
  let result = {Alt: false, Shift: false, Ctrl: false, name: '', code: ''};
  keys.map(s => {
    let u = s.toUpperCase();
    switch (u) {
      case 'SHIFT':
        result.Shift = true;
        break;
      case 'CTRL':
        result.Ctrl = true;
        break;
      case 'ALT':
        result.Alt = true;
        break;
      case 'SPACE':
        result.code = 'Space';
        break;
      case 'UP':
        result.code = 'ArrowUp';
        break;
      case 'DOWN':
        result.code = 'ArrowDown';
        break;
      case 'LEFT':
        result.code = 'ArrowLeft';
        break;
      case 'RIGHT':
        result.code = 'ArrowRight';
        break;
      case 'DELETE':
        result.code = 'Delete';
        break;
      case 'BS':
        result.code = 'Backspace';
        break;
     case 'ESC':
        result.code = 'Escape';
        break;
      case 'INSERT':
        result.code = 'Insert';
        break;
      case 'TAB':
        result.code = 'Tab';
        break;
      case 'ENTER':
        result.code = 'Enter';
        break;
      default:
        result.name = s.toUpperCase();
    }
  });
  return result;
}
