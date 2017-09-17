// EvalClient.js
//
// Copyright (c) Eddie.
// Distributed under the terms of the MIT License.

const EvalClientError = {
  UNMAKABLE_SERVER: Symbol("Unmakable server"),
  DEAD_SERVER: Symbol("Dead server")
};

const SERVER_HOST = 'localhost';

class EvalClient {

  constructor(masterURI, clientId, descripter, websocket = false) {
    this.id = clientId;
    this.descripter = descripter;
    this.enableWebSocket = websocket;
    this.ws = null;
    this.evalURI = null;
    this.masterURI = masterURI;
    this.token = null;
  }

  static makeRequest(method, params, clientId = null, descripter = "default") {
    let object = {
      "jsonrpc": "2.0",
      "method": method,
      "params": params,
      "descripter": descripter
    };
    if (clientId != null) {
      object.id = clientId;
    }
    return JSON.stringify(object);
  }

  static makeServerMessage(clientId, descripter, websocket) {
    return JSON.stringify({
      "jsonrpc": "2.0",
      "id": clientId,
      "method": "darkmatter/makeServer",
      "params": {
        "enableWebSocket": websocket,
        "descripter": descripter
      }
    });
  }

  makeServer() {
    console.log("[->] Make eval-server... " + this.masterURI);
    let message = EvalClient.makeServerMessage(this.id, this.descripter, this.enableWebSocket);
    return new Promise((resolve, reject) => {
      http.put(this.masterURI, message)
        .then(json => {
          let result = json.result;
          this.evalURI = `ws://${SERVER_HOST}:${result.port}`;
          this.token = json.token;
          resolve();
        })
        .catch(err => {
          console.log(err);
          reject(EvalClientError.UNMAKABLE_SERVER);
        });
    });
  }

  connect() {
    return new Promise((resolve, reject) => {
      if (this.evalURI) {
        this.ws = new WebSocket(this.evalURI);
        this.ws.addEventListener('open', event => {
          console.log("[->WS] Open successful");
          resolve(event);
        });
        this.ws.addEventListener('error', event => {
          console.log(`[Error] failed connect (${event})`);
          reject(EvalClientError.DEAD_SERVER);
        });
      } else {
        reject(EvalClientError.DEAD_SERVER);
      }
    });
  }

  /** DEPRECATED
    * Note:
    * JSONRPC server don't support Cross-Origin Resource Sharing.
    * We should use WebSocket instead. */
  sendTCP(request) {
    console.log("[->TCP] " + request);
    return new Promise((resolve, reject) => {
      http.put(this.masterURI + '/eval/', request).then(json => {
        console.log("[<-TCP] Resolve " + json);
        resolve(json);
      }).catch(err => {
        console.log("[<-TCP] Reject (DEAD_SERVER)");
        reject(EvalClientError.DEAD_SERVER);
      });
    });
  }

  sendWS(request) {
    console.log("[->WS] "+ request);
    return new Promise((resolve, reject) => {
      this.connect()
        .then(() => {
          this.ws.addEventListener('message', event => {
            console.log("[<-WS] Resolve");
            resolve(JSON.parse(event.data));
          });
          this.ws.send(request);
        })
        .catch(err => {
          console.log("[<-WS] Reject (DEAD_SERVER)");
          reject(EvalClientError.DEAD_SERVER);
        });
    });
  }

  requestUntilSuccessful(request, trueResolve, trueReject) {
    console.log("Try request...");
    let proc = this.enableWebSocket ? this.sendWS.bind(this) : this.sendTCP.bind(this) ;
    proc(request)
      .then(json => {
        if (json.id && json.id !== this.id) {
          trueReject(new Error("INVALID_ID"));
        } else {
          trueResolve(json);
        }
      })
      .catch(err => {
        console.log(`[Reject] Request receive failed (${String(err)})`);
        if (err == EvalClientError.DEAD_SERVER && this.enableWebSocket) {
          this.makeServer()
            .then(() => {
              console.log("[Success] Make eval server at " + this.evalURI);
              //setTimeout(100, this.requestUntilSuccessful, request, trueResolve, trueReject);
              this.requestUntilSuccessful(request, trueResolve, trueReject);
            })
            .catch(err => {
              console.log("[Error] Unmakable server");
              setTimeout(1000, this.requestUntilSuccessful, request, trueResolve, trueReject);
              //trueReject(EvalClientError.UNMAKABLE_SERVER);
            });
        }
      });
  }

  request(request) {
    return new Promise((resolve, reject) => {
      this.requestUntilSuccessful(request, resolve, reject);
    });
  }

  initialize(plugins, defaultPackage, trace) {
    const request = EvalClient.makeRequest('darkmatter/initialize', {
      initializeOptions: {
        plugins: plugins || [],
        defaultPackage: defaultPackage
      },
      trace: trace
    }, this.id, this.descripter);
    return this.request(request);
  }

  eval(code, cellId, outputRendering = true, optional = null) {
    const request = EvalClient.makeRequest('darkmatter/eval', {
      code: code,
      outputRendering: outputRendering,
      cellId: cellId,
      optional: optional
    }, this.id, this.descripter);
    return this.request(request);
  }
}
