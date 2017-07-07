StructRenderer = [];

class LispRenderer {
  constructor(socket) {
    this.socket = socket;
  }

	renderStruct(obj) {
		let structName = obj.$type;
    let res = null;
    for (let struct of StructRenderer) {
      if (structName === struct.name || structName === struct.short) {
        res = new struct.renderer(obj);
        break;
      }
    }
    return res;
	}

	plotArray(vec) {

		for (let idx in vec) {
			vec[idx] = [idx].concat(vec[idx]);
		}

		let table = document.createElement('table');
		d3.select(table)
			.append('thead')
			.append('tr')
			.selectAll('th')
			.data(['Idx', 'Data'])
			.enter()
			.append('th')
			.text(d => d);
		d3.select(table)
			.append('tbody')
			.selectAll('tr')
				.data(vec)
				.enter()
				.append('tr')
				.selectAll('td')
					.data((row) => {return d3.entries(row);})
					.enter()
					.append('td')
					.text((d) => {return d.value;});
		return table.outerHTML;
	}

  convert(src) {
    let res = [];
    let idx = 0;
    let beforeIdx = 0;
    let contents = null;
    while (src.length > idx) {
      [contents, idx] = ResultParser.parse(src, idx)
      if (typeof(contents) === 'object') {
        if (Array.isArray(contents)) {
          res.push(this.plotArray(contents));
        } else if (contents.$type) {
          let struct = this.renderStruct(contents);
          if (struct) {
            res.push(struct);
          } else {
            res.push(src.substring(beforeIdx, idx));
          }
        } else {
          res.push(src.substring(beforeIdx, idx));
        }
      } else {
        res.push(src.substring(beforeIdx, idx));
      }
      beforeIdx = idx;
    }
    return res;
  }

  onEval(src, cellId) {
    return (resolve, reject) => {
      this.socket.eval(src, cellId).then((obj) => {
        let rendering = obj.rendering;
        if (rendering) {
          let val = obj['returnValue'];
          let evaluated = obj['result'];
          resolve({rendering: true, returnValue: val, result: this.convert(evaluated)});
        } else {
          resolve({rendering: false});
        }
      });
    };
  }

	render(src, cellId) {
    let e = this.onEval(src, cellId).bind(this);
    return new Promise(e);
	}
}

