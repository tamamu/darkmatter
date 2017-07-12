function loadUncached(urls, thenDo) {
  if (!urls.length) { thenDo && thenDo(); return; }
  var url = urls.shift();
  var script = document.createElement('script');
  script.src = url + (url.indexOf('?') > -1 ? '&' : '?' + Date.now());
  document.head.appendChild(script);
  script.addEventListener('load', function() { loadUncached(urls, thenDo); });
}
