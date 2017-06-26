/* Darkmatter.LispSocket */

Modified = false;

class LispSocket {
  constructor(http_uri, ws_uri, filepath) {
    this.wsUri = ws_uri;
    this.httpUri = http_uri;
    this.connected = false;
    this.attempts = 1;
    this.socket = null;
    this.parser = null;
    this.indicator = null;
    this.filepath = filepath;
  }

  static generateInterval(k) {
    let maxInterval = (Math.pow(2, k) - 1) * 1000;

    if (maxInterval > 30 * 1000) {
      maxInterval = 30 * 1000;
    }

    return Math.random() * maxInterval;
  }

  attachParser(parser) {
    this.parser = parser;
  }

  attachIndicator(indicator) {
    this.indicator = indicator;
  }

  onOpen(callback, connection) {
    return function() {
      this.socket = connection;
      this.attempts = 1;
      this.connected = true;
      if (this.indicator) {
        this.indicator.className = 'connected';
      }
      if (callback) {
        callback();
      }
    }
  }

  onClose(callback) {
    return function() {
      this.connected = false;
      if (this.indicator) {
        this.indicator.className = 'disconnected';
      }
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
    connection.addEventListener('close', this.onClose(callback).bind(this), false);
    connection.addEventListener('error', this.onError.bind(this), false);
  }

  eval(src) {
    if (this.connected) {
      return new Promise(
        (resolve, reject) => {
          let parser = this.parser;
          let onMessage = (message) => {
            let json = JSON.parse(message);
            let returnVal = json['return'];
            let result = json['output'];
            resolve({'returnValue':returnVal, 'result':result});
          };
          let sender = JSON.stringify({
            "message": "eval",
            "data": src || "",
            "file": this.filepath
          });
          $put(this.httpUri, sender).then(onMessage);
        });
    }
  }

  save(cells) {
    if (this.connected && Modified) {
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
      let onmessage = (message) => {
        let json = JSON.parse(message);
        console.log(`Result:${json['return']}`);
        let show = document.getElementById('alert');
        show.innerText = `Saved: ${json['return']} (${(new Date()).toString()})`;
        Modified = false;
      };
      let sender = JSON.stringify({
        "message": "save",
        "file": this.filepath,
        "data": data
      });
      console.log(sender.length);
      $put(this.httpUri, sender).then(onmessage);
    } else {
      console.log("Can't save the code.");
    }
  }

  recall() {
    if (this.connected) {
      let onmessage = (message) => {
        let json = JSON.parse(message);
        let show = document.getElementById('alert');
        show.innerText = `Recall: new package created at ${(new Date()).toString()})`;
      }
      let sender = JSON.stringify({
        "message": "recall",
        "file": this.filepath
      });
      $put(this.httpUri, sender).then(onmessage);
    } else {
      console.log("Can't recall the package.");
    }
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

