/// HttpRequest.js
//
// Copyright (c) Eddie.
// Distributed under the terms of the MIT License.

const http = (function () {

  function __http__(method, uri, data) {
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
      xhr.open(method, uri);
      xhr.send(data);
    });
  }

  return {
    post: function(uri, data) {
      return __http__('POST', uri, data);
    },
    put: function(uri, data) {
      return __http__('PUT', uri, data);
    }
  }
})();
