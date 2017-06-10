
class LispSocket {
	constructor(uri, filepath, indicator) {
    this.uri = uri;
		this.socket = null;
		this.state = false;
    this.attempts = 1;
    this.file = filepath;
    this.modified = false;
    window.addEventListener('beforeunload', (e) => {
      if (this.modified) {
        e.returnValue = "If you don't save the note, changes from the last minute will be lost.";
      }
    }, false);
    this.indicator = indicator;
    this.renderer = new marked.Renderer();
    let originalCode = this.renderer.code.bind(this.renderer);
    this.renderer.code = (code, lang, escaped) => {
      let math;
      if (!lang && (math = this.renderLatex(code))) {
        return math;
      }
      return originalCode(code, lang, escaped);
    };
    let originalCodeSpan = this.renderer.codespan.bind(this.renderer);
    this.renderer.codespan = (text) => {
      let math;
      if (math = this.renderLatex(text)) {
        return math;
      }
      return originalCodeSpan(text);
    };
    let originalImage = this.renderer.image.bind(this.renderer);
    let originalLink = this.renderer.link.bind(this.renderer);
    let get_uri = (href) => {
      if (href.startsWith('/')) {
        return `${HTTP_URI}/${href}`;
      } else if (href.startsWith('http://')) {
        return href;
      } else {
        return `${CD_URI}${href}`;
      }
    }
    this.renderer.image = (href, title, text) => {
      return originalImage(get_uri(href), title, text);
    }
    this.renderer.link = (href, title, text) => {
      return originalLink(get_uri(href), title, text);
    }
    this.connect();
  }

  static generateInterval(k) {
    let maxInterval = (Math.pow(2, k) - 1) * 1000;

    if (maxInterval > 30 * 1000) {
      maxInterval = 30*1000;
    }

    return Math.random() * maxInterval;
  }

  connect(callback = null) {
    console.log('Connect...');
    let connection = new WebSocket(this.uri);
    connection.onopen = () => {
      this.socket = connection;
      this.indicator.className = 'connected';
      this.state = true;
      if (callback) {
        callback();
      }
    }
    connection.onclose = () => {
      this.indicator.className = 'notconnected';
      this.state = false;
      this.attempts += 1;
      let time = LispSocket.generateInterval(this.attempts);
      setTimeout(() => {
        this.attempts += 1;
        this.connect();
      }, time);
    }
    connection.onerror = (e) => {
      this.indicator.className = 'notconnected';
      this.state = false;
      console.log(e);
    }
  }

  renderLatex(expr) {
    console.log('render latex');
    if (expr[0] === '$' && expr[expr.length-1] === '$') {
      let displayStyle = false;
      expr = expr.substr(1, expr.length-2);
      if (expr[0] === '$' && expr[expr.length-1] === '$') {
        displayStyle = true;
        expr = '\\displaystyle ' + expr.substr(1, expr.length-2);
      }
      let html;
      try {
        html = katex.renderToString(expr, {throwOnError: false});
      } catch (e) {
        html = e;
      }
      if (displayStyle) {
        html = html.replace(/class=\"katex\"/g, 'class="katex katex-block" style="display: block;"');
      }
      return html;
    } else {
      return null;
    }
  }

  plotLine(dict) {
    let vec = arrayToList(dict[":DATA"]);
    let data = [];
    for (let i=0; i < vec.length; i++) {
      data.push([i, vec[i]]);
    }
    let container = document.createElement('div');
    let x = d3.scaleLinear()
              .range([0, 300]);
    let y = d3.scaleLinear()
              .range([300, 0]);
    x.domain(d3.extent(data, d => d[0])).nice();
    y.domain(d3.extent(data, d => d[1])).nice();
    let line = d3.line()
                 .x(d => x(d[0]))
                 .y(d => y(d[1]));
    let svg = d3.select(container)
                .append('svg')
                .attr('width', '400')
                .attr('height', '400')
                .append('g')
                  .attr('transform', 'translate(40, 40)');
    svg.append('g')
         .call(d3.axisLeft(y))
       .append('text')
         .attr('y', 6)
         .attr('dy', '0.71em')
         .attr('text-anchor', 'end')
         .text('Y');
    svg.append('path')
       .datum(data)
       .attr('class', 'line')
       .attr('fill', 'none')
       .attr('stroke', 'white')
       .attr('stroke-width', 1.5)
       .attr('d', line);

    return container.outerHTML;
  }

	plotScatter(dict) {
		let vec = arrayToList(dict[":DATA"]);
		let container = document.createElement('div');
		let x = d3.scaleLinear()
							.range([0, 300]);
		x.domain(d3.extent(d => d[0]));
		let y = d3.scaleLinear()
							.range([300, 0]);
		y.domain(d3.extent(d => d[1]));

		let svg = d3.select(container)
								.append('svg')
								.attr('wihth', '300')
								.attr('height', '300')
								.attr('style', 'overflow: visible; margin: 20px;')
								.append('g')
									.attr('transform', 'translate(40, 0)');

		let colorCategory = d3.schemeCategory10;

		svg.selectAll('circle')
			 .data(vec)
			 .enter()
			 .append('circle')
				.attr('class', 'mark')
				.attr('fill', d => {
					if (d.length>=2) {
						return colorCategory[d[2]];
					} else {
						return 'black';
					}})
				.attr('cx', d => d[0])
				.attr('cy', d => 300-d[1])
				.attr('r', 5);

		svg.append('g')
			 .attr('transform', 'translate(0,300)')
			 .call(d3.axisBottom(x));

		svg.append('text')
			 .attr('transform', 'translate(150,330)')
			 .style('text-anchor', 'middle')
			 .text(dict[':XLABEL'])

		svg.append('g')
			 .call(d3.axisLeft(y));

		svg.append('text')
			 .attr('transform', 'rotate(-90)')
			 .attr('y', -30)
			 .attr('x', -150)
			 .attr('dy', '1em')
			 .attr('text-anchor', 'middle')
			 .text(dict[':YLABEL']);

		return container.outerHTML;
	}

	parseStruct(obj) {
		let structName = obj.children[0];
		let dict = {};
		for (let i=1; i < obj.children.length; i+=2) {
			dict[obj.children[i]] = obj.children[i+1];
		}

		switch (structName) {
			case "SCATTER":
			case "DARKMATTER.PLOT:SCATTER":
				return this.plotScatter(dict);
				break;
      case "LINE":
      case "DARKMATTER.PLOT:LINE":
        return this.plotLine(dict);
        break;
			default:
				return structName;
		}
	}

	plotArray(obj) {
		let vec = arrayToList(obj);
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

	parse(src) {
		let res = "";
		let idx = 0;
		let beforeIdx = 0;
		let contents = null;

		while (src.length > idx) {
			[contents, idx] = parseLisp(src, idx)
			if (typeof(contents) === 'object') {
				if (contents.mark === '#S') {
					res += this.parseStruct(contents);
				} else if (contents.mark === '#') {
					res += this.plotArray(contents);
				} else {
					res += src.substring(beforeIdx, idx);
				}
			} else {
				res += src.substring(beforeIdx, idx);
			}
			beforeIdx = idx;
		}

		return res;
	}

  markdown(src, output, callback = null, arg = null) {
    let result = marked(src, {breaks: true, renderer: this.renderer});
    output.innerHTML = result;
    if (callback) callback(arg);
  }

	eval(src, output, callback = null, arg = null) {
		if (this.state) {
			this.socket.onmessage = (e) => {
				let json = JSON.parse(e.data);
				console.log(`Result:${json['return']}`);
        let returnVal = json['return'];
        if (returnVal.length > 70) {
          returnVal = returnVal.substr(0, 65);
          returnVal += '...';
        }
				let result = this.parse(json['output']);
				output.innerHTML = `<div id="result"> ${returnVal}</div>`;
				output.innerHTML += result;
        if (callback) callback(arg);
			}
      let sender = JSON.stringify({
        "message": "eval",
        "data": src,
        "file": this.file
      });
      console.log(sender);
			this.socket.send(sender);
		} else {
			console.log("Can't send the code.");
		}
	}

  save(cells) {
    if (this.state && this.modified) {
      let data = [];
      for (let cell of cells) {
        let ec = window.editcells[cell.id];
        let d = {
          "id": cell.id,
          "next": cell.dataset.next || '',
          "before": cell.dataset.before || '',
          "count": cell.dataset.count || 0,
          "lisp": cell.querySelector('#lisp').dataset.content,
          "md": cell.querySelector('#md').dataset.content,
          "lang": cell.dataset.lang,
          "output": cell.querySelector('#output').innerHTML
        };
        d[cell.dataset.lang] = ec.value;
        data.push(d);
      }
      this.socket.onmessage = (e) => {
        let json = JSON.parse(e.data);
        console.log(`Result:${json['return']}`);
        let show = document.getElementById('alert');
        show.innerText = `Saved: ${json['return']} (${(new Date()).toString()})`;
        window.ls.modified = false;
      }
      let sender = JSON.stringify({
        "message": "save",
        "file": this.file,
        "data": data
      });
      this.socket.send(sender);
    } else {
      console.log("Can't save the code.");
    }
  }

  recall() {
    if (this.state) {
      this.socket.onmessage = (e) => {
        let json = JSON.parse(e.data);
        let show = document.getElementById('alert');
        show.innerText = `Recall: new package created at ${(new Date()).toString()})`;
      }
      let sender = JSON.stringify({
        "message": "recall",
        "file": this.file
      });
      this.socket.send(sender);
    } else {
      console.log("Can't recall the package.");
    }
  }
}

