/* Darkmatter.LispSocket */

Modified = false;
Alerts = {};
CellOutputObject = {};

class LispSocket {
  constructor(http_uri, ws_uri, filepath, token) {
    this.wsUri = ws_uri;
    this.httpUri = http_uri;
    this.token = token;
    this.connected = false;
    this.attempts = 1;
    this.socket = null;
    this.parser = null;
    this.filepath = filepath;
    this.updateSymbols = (symbols) => {};
    this.showAlert = (text) => {console.log(text);};
  }

  static generateInterval(k) {
    let maxInterval = (Math.pow(2, k) - 1) * 1000;

    if (maxInterval > 30 * 1000) {
      maxInterval = 30 * 1000;
    }

    return Math.random() * maxInterval;
  }

  attachAlert(sa) {
    this.showAlert = sa;
  }

  attachSymbolManager(sm) {
    this.updateSymbols = sm.updateSymbols.bind(sm);
  }

  attachParser(parser) {
    this.parser = parser;
  }

  onOpen(callback, connection) {
    return function() {
      this.attempts = 1;
      this.connected = true;
      if (callback) {
        callback(connection);
      }
    }
  }

  onClose(callback) {
    return function() {
      this.connected = false;
      this.attempts += 1;
      let time = LispSocket.generateInterval(this.attempts);
      setTimeout(() => {
        this.attempts += 1;
        this.open(callback);
      }, time);
    }
  }

  onError(e) {
    this.connected = false;
    console.log(e);
  }

  open(callback = null) {
    let connection = new WebSocket(this.wsUri);
    connection.addEventListener('open', this.onOpen(callback, connection).bind(this), false);
    connection.addEventListener('error', this.onError.bind(this), false);
  }

  spawnAlert(cell, id) {
    let alertSocket = new AlertSocket(this.wsUri, cell, id, 1000);
    Alerts[cell] = alertSocket;
    CellOutputObject[cell] = null;
    alertSocket.onupdate = (cell, output) => {
      // OutputRenderer exists?
      if (CellOutputObject[cell]) {
        // If it exists and update object has come, update the element.
        let converted = Cells[cell].renderer
          .getRenderMethod('lisp').convert(output);
        // Check the type equality between the output and previous output
        if (converted.length > 0 &&
            typeof converted[0] === 'object' &&
            converted[0].constructor === CellOutputObject[cell].constructor) {
          CellOutputObject[cell].update(converted[0]);
        } else {
          Cells[cell].render(null, converted);
        }

      } else {
        let converted = Cells[cell].renderer
          .getRenderMethod('lisp').convert(output);
        // Check the output type
        if (converted.length > 0 &&
            typeof converted[0] === 'object' &&
            converted[0].element) {
          CellOutputObject[cell] = converted[0];
          Cells[cell].render(null, converted[0].element);
        } else {
          Cells[cell].render(null, converted);
        }
      }
    };
    alertSocket.open();
  }

  eval(src, cellId) {
    return new Promise(
      (resolve, reject) => {
        let parser = this.parser;
        let onMessage = (json) => {
          let mes = json.message;
          this.updateSymbols(json.symbols);
          if (mes === 'alert_start') {
            let id = json['id'];
            if (Alerts[cellId] && Alerts[cellId].exited === false) {
              Alerts[cellId].kill().then(() => {
                this.spawnAlert(cellId, id);
              });
              Alerts[cellId].kill();
            } else {
              this.spawnAlert(cellId, id);
            }
            resolve({rendering: false});
          } else {
            let returnVal = json['return'];
            let result = json['output'];
            resolve({rendering: true, returnValue: returnVal, result: result});
          }
        };
        let sender = JSON.stringify({
          "message": "eval",
          "data": src || "",
          "file": this.filepath,
          "cell": cellId,
          "token": this.token
        });
        $put(this.httpUri, sender).then(onMessage);
      });
  }

  save(cells) {
    if (Modified) {
      let data = [];
      for (let cell of cells) {
        let ec = Cells[cell.id];
        let d = {
          "id": cell.id,
          "next": cell.dataset.next || '',
          "prev": cell.dataset.prev || '',
          "count": cell.dataset.count || 0,
          "lisp": cell.querySelector('#lisp').dataset.content,
          "md": cell.querySelector('#md').dataset.content,
          "lang": cell.dataset.lang,
          "output": cell.querySelector('#output').innerHTML
        };
        d[cell.dataset.lang] = ec.value;
        data.push(d);
      }
      let onmessage = (json) => {
        console.log(`Result:${json.return}`);
        this.showAlert(`Saved: ${json.return} (${(new Date()).toString()})`, 'save');
        Modified = false;
      };
      let sender = JSON.stringify({
        "message": "save",
        "file": this.filepath,
        "data": data,
        "token": this.token,
        "cell": ''
      });
      $put(this.httpUri, sender).then(onmessage);
    } else {
      console.log("Can't save the code.");
    }
  }

  recall() {
    let onmessage = (json) => {
      this.showAlert(`Recall: new package created at ${(new Date()).toString()})`, 'recall');
    }
    let sender = JSON.stringify({
      "message": "recall",
      "file": this.filepath,
      "token": this.token,
      "cell": ''
    });
    $put(this.httpUri, sender).then(onmessage);
  }
}

function test() {
  let sock = new LispSocket('ws://localhost:8888', '/home/tamamu/foo.lisp');
  sock.attachParser(new Parser());
  sock.open();
  setTimeout(()=>{
    sock.eval("(print \"foo\")").then((v, result) => {console.log('return:'+v+'\n'+result);});
  }, 1000);
}

function $put(uri, data) {
  return new Promise((resolve, reject) => {
    let xhr = new XMLHttpRequest();
    xhr.responseType = 'json';
    xhr.onload = () => {
      if (xhr.status >= 200 && this.status < 300) {
        resolve(xhr.response);
      } else {
        reject(xhr.statusText);
      }
    };
    xhr.onerror = () => {
      reject(xhr.statusText);
    };
    xhr.open('PUT', uri);
    xhr.send(data);
  });
}

