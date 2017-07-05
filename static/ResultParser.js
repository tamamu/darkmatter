
const NumberExp = /[+-]?[0-9]+[.]?[0-9]([eE][+-])?[0-9]*/;
const WhiteSpaceExp = /\s/;

class ResultParser {

  static parse(src, idx) {
    if (idx === undefined) idx = 0;
    let acc = "";
    let res = null;

    for (; src.length > idx; idx++) {
      switch (src[idx]) {
        case '#':
          if (acc !== '') return [acc, idx];
          [res, idx] = ResultParser.parseSharp(src, idx+1);
          return [res, idx];
          break;
        case '(':
          if (acc !== '') return [acc, idx];
          [res, idx] = ResultParser.parseList(src, idx+1);
          return [res, idx];
          break;
        case '"':
          if (acc !== '') return [acc, idx];
          [res, idx] = ResultParser.parseString(src, idx+1);
          return [res, idx];
          break;
        default:
          acc += src[idx];
          break;
      }
    }

    if (acc !== '') res = ResultParser.parseSymbol(acc);

    return [res, idx];
  }

  static parseSymbol(src) {
    let n = src.match(NumberExp);
    if (n && n[0].length === src.length) {
      return parseFloat(src);
    } else {
      return src;
    }
  }

  static parseList(src, idx) {
    let acc = "";
    let res = [];
    let tmp = "";

    while (src.length > idx) {
      switch (src[idx]) {
        case '#':
          [tmp, idx] = ResultParser.parseSharp(src, idx+1);
          res.push(tmp);
          break;
        case "'":
          idx++;
          break;
        case '(':
          [tmp, idx] = ResultParser.parseList(src, idx+1);
          res.push(tmp);
          break;
        case '"':
          [tmp, idx] = ResultParser.parseString(src, idx+1);
          res.push(tmp);
          break;
        case ')':
          if (acc !== '') res.push(ResultParser.parseSymbol(acc));
          return [res, idx+1];
          break;
        default:
          if (src[idx].match(WhiteSpaceExp)) {
            if (acc !== '') {
              res.push(ResultParser.parseSymbol(acc));
              acc = '';
            }
          } else {
            acc += src[idx]
          }
          idx++;
          break;
      }
    }

    console.log('uncorrespond');
    return [res, idx];
  }

  static plistToObject(plist) {
    let obj = {};
  	for (let i=0; i < plist.length; i+=2) {
			obj[plist[i]] = plist[i+1];
		}
    return obj;
  }

  static parseSharp(src, idx) {
    let acc = "";
    let res = {mark: null, children: []};

    for (; src.length > idx; idx++) {
      switch (src[idx]) {
        case '(':
          if (acc === '') {
            return ResultParser.parseList(src, idx+1);
          } else if (acc === '2A') {
            let inner; [inner, idx] = ResultParser.parseList(src, idx+1);
            res = {$type: '$<MATRIX>', data: inner, dim: [inner.length, inner[0].length]};
            return [res, idx];
          } else if (acc === 'S') {
            let inner; [inner, idx] = ResultParser.parseList(src, idx+1);
            let data = ResultParser.plistToObject(inner.slice(1));
            res = {$type: inner[0]};
            for (let key in data) {
              res[key] = data[key];
            }
            return [res, idx];
          } else {
            res.mark = '#'+acc;
            [res.children, idx] = ResultParser.parseList(src, idx+1);
            return [res, idx];
          }
          break;
        default:
          if (src[idx].match(WhiteSpaceExp)) {
            return ['#'+acc, idx+1];
          }
          acc += src[idx];
          break;
      }
    }

    return ['#'+acc, idx];
  }

  static parseString(src, idx) {
    let acc = "";

    for (; src.length > idx; idx++) {
      switch (src[idx]) {
        case '"':
          return [acc, idx+1];
          break;
        default:
          acc += src[idx];
          break;
      }
    }

    return [acc, idx];
  }
}
