
class AlertSocket {

  static generateInterval(k) {
    let maxInterval = (Math.pow(2, k) - 1) * 1000;

    if (maxInterval > 30*1000) {
      maxInterval = 30*1000;
    }

    return Math.random() * maxInterval;
  }

  constructor(ws_uri, cell, id, interval) {
    this.uri = ws_uri;
    this.connection = null;
    this.cell = cell;
    this.id = id;
    this.attempts = 1;
    this.wantToKill = false;
    this.exited = false;
    this.updateInterval = interval;
    this.timer = null;
    this.onupdate = (cell, output) => {console.log(cell, output);};
    this.onexit = () => {console.log('exited')};
  }

  requestInit(conn) {
    console.log('init');
    let sender = JSON.stringify({
      message: 'init',
      cell: this.cell,
      id: this.id
    });
    conn.send(sender);
  }

  requestRecv() {
    console.log('recv')
    let sender = JSON.stringify({
      message: 'recv',
      id: this.id
    });
    this.connection.send(sender);
  }

  requestKill() {
    console.log('kill');
    let sender = JSON.stringify({
      message: 'kill',
      id: this.id
    });
    this.connection.send(sender);
  }

  open() {
    if (this.connection === null) {
      let conn = new WebSocket(this.uri);
      conn.onopen = () => {
        this.connection = conn;
        this.wantToKill = false;
        this.requestInit(conn);
      }
      conn.onclose = this.retryInit.bind(this);
      conn.onmessage = this.init.bind(this);
    }
  }

  retryInit() {
    this.attempts += 1;
    if (this.id === null) {
      setTimeout(this.open, AlertSocket.generateInterval(this.attempts));
    } else {
      this.attempts = 1;
      this.connection.onclose = null;
      this.connection = null;
    }
  }

  init(e) {
    let json = JSON.parse(e.data);
    if (json.message === 'init') {
      this.id = json.id;
      this.connection.onmessage = this.recv.bind(this);
      this.connection.onclose = this.retryRecv.bind(this);
      this.requestRecv()
      this.timer = setInterval(() => {
        if (this.connection) {
          if (this.wantToKill) {
            this.requestKill();
            this.wantToKill = false;
          } else {
            this.requestRecv();
          }
        }
        }, this.updateInterval);
    } else {
      this.connection.onmessage = null;
      this.connection.onclose = null;
      this.connection = null;
      throw Error("Can't receive init message");
    }
  }

  retryRecv() {
    this.attempts += 1;
    if (!this.wantToKill) {
      setTimeout(this.open, AlertSocket.generateInterval(this.attempts));
    } else {
      this.attempts = 1;
      this.connection.onclose = null;
      this.connection = null;
    }
  }

  recv(e) {
    let json = JSON.parse(e.data);
    console.log(json);
    this.attempts = 1;
    switch (json.message) {
      case 'exit':
        console.log('exited');
        this.clearTimer();
        this.id = null;
        this.wantToKill = true;
        this.connection.onmessage = null;
        this.connection.onclose = null;
        this.connection.close();
        this.connection = null;
        this.exited = true;
        this.onexit();
        break;
      case 'update':
          this.onupdate(this.cell, json.output);
        break;
      default:
        this.id = null;
        this.wantToKill = true;
        this.connection.onmessage = null;
        this.connection.onclose = null;
        this.connection.close();
        this.connection = null;
        throw Error("Can't receive update message");
        break;
    }
  }

  clearTimer() {
    if (this.timer !== null) {
      clearInterval(this.timer);
    }
    this.timer = null;
  }

  kill() {
    return new Promise((resolve, reject) => {
      this.wantToKill = true;
      this.requestKill();
      this.onexit = resolve;
    });
  }

}

