
let KeyBindings = [];

function HandlingKeyBind(e) {
  let input = {Alt: e.altKey, Shift: e.shiftKey, Ctrl: e.ctrlKey || e.metaKey,
               char: e.charCode, code: e.keyCode};
  for (bind of KeyBindings) {
    if (bind.key.Alt   == input.Alt &&
        bind.key.Shift == input.Shift &&
        bind.key.Ctrl  == input.Ctrl &&
        bind.key.char == input.char &&
        bind.key.code == input.code) {
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
  let result = {Alt: false, Shift: false, Ctrl: false, char: 0, code: 0};
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
        result.char = 32;
        break;
      case 'UP':
        result.code = 38;
        break;
      case 'DOWN':
        result.code = 40;
        break;
      case 'LEFT':
        result.code = 37;
        break;
      case 'RIGHT':
        result.code = 39;
        break;
      case 'DELETE':
        result.code = 46;
        break;
      case 'BS':
        result.code = 8;
        break;
      case 'F1':
        result.code = 112;
        break;
      case 'F2':
        result.code = 113;
        break;
      case 'F3':
        result.code = 114;
        break;
      case 'F4':
        result.code = 115;
        break;
      case 'F5':
        result.code = 116;
        break;
      case 'F6':
        result.code = 117;
        break;
      case 'F7':
        result.code = 118;
        break;
      case 'F8':
        result.code = 119;
        break;
      case 'F9':
        result.code = 120;
        break;
      case 'F10':
        result.code = 121;
        break;
      case 'F11':
        result.code = 122;
        break;
      case 'F12':
        result.code = 123;
        break;
      case 'ESC':
        result.code = 27;
        break;
      case 'INSERT':
        result.code = 45;
        break;
      case 'TAB':
        result.code = 9;
        break;
      case 'ENTER':
        result.code = 13;
        break;
      default:
        if (result.Shift) {
          result.char = s.toUpperCase().charCodeAt(0);
        } else {
          result.char = s.charCodeAt(0);
        }
    }
  });
  return result;
}
