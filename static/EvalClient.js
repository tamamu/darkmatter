// EvalClient.js
//
// Copyright (c) Eddie.
// Distributed under the terms of the MIT License.

const EvalClientError = {
  UNMAKABLE_SERVER: 0,
  DEAD_SERVER: 1
};

class EvalClient {

  constructor(masterURI, clientId, websocket = false) {
    this.id = clientId;
    this.enableWebSocket = websocket;
    this.ws = null;
    this.evalURI = null;
    this.masterURI = masterURI;
    this.token = null:
  }

  static makeRequest(method, params, id) {
    return JSON.stringify({
      "jsonrpc": "2.0",
      "method": method,
      "params": params,
      "id": id
    });
  }

  static makeServerMessage(clientId, websocket) {
    return JSON.stringify({
      "method": "makeServer",
      "params": {
        "enableWebSocket": websocket,
        "clientId": clientId
      }
    });
  }

  makeServer() {
    return new Promise((resolve, reject) => {
      $put(this.masterURI, EvalClient.makeServerMessage(this.id, this.enableWebSocket))
        .then(json => {
          this.evalURI = json.uri;
          this.token = json.token;
          resolve();
        })
        .catch(err => {
          reject(EvalClientError.UNMAKABLE_SERVER);
        });
    }
  }

  connect() {
    return new Promise((resolve, reject) => {
      if (this.uri) {
        this.ws = new WebSocket(this.evalURI);
        this.ws.addEventListener('open', event => {
          resolve(event);
        });
        this.ws.addEventListener('error', event => {
          reject(EvalClientError.DEAD_SERVER);
        });
      } else {
        reject(EvalClientError.DEAD_SERVER);
      }
    });
  }

  sendTCP(request) {
    return new Promise((resolve, reject) => {
      $post(this.evalURI, request).then(json => {
        resolve(json);
      }).catch(err => {
        reject(EvalClientError.DEAD_SERVER);
      });
    }
  }

  sendWS(request) {
    return new Promise((resolve, reject) => {
      this.connect()
        .then(() => {
          this.ws.addEventListener('message', event => {
            resolve(JSON.parse(event));
          });
          this.ws.send(request);
        })
        .catch(err => {
          reject(EvalClientError.DEAD_SERVER);
        });
    });
  }

  requestUntilSuccessful(request, trueResolve, trueReject) {
    return new Promise((resolve, reject) => {
      let proc = this.enableWebSocket ? this.sendWS : this.sendTCP ;
      this.proc(request)
        .then(json => {
          trueResolve(json);
        })
        .catch(err => {
          if (err == EvalClientError.DEAD_SERVER) {
            this.makeServer()
              .then(() => {
                setTimeout(100, this.requestUntilSuccessful, request, trueResolve, trueReject);
              })
              .catch(err => {
                trueReject(EvalClientError.UNMAKABLE_SERVER);
              });
          }
        });
    }
  }

  request(request) {
    return new Promise((resolve, reject) =>
      this.requestUntilSuccessful(request, resolve, reject));
  }
}
