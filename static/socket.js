
class LispSocket {
	constructor(uri, filepath) {
		this.socket = new WebSocket(uri);
		this.state = false;
    this.file = filepath;
    this.modified = false;
		this.socket.onopen = (e) => {
			console.log('Connected.');
			this.state = true;
		};
		this.socket.onclose = (e) => {
			console.log('Closed.');
			this.state = false;
		}
		this.socket.onerror = (e) => {
			console.log(e);
		}
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

	eval(src, output) {
		if (this.state) {
			this.socket.onmessage = (e) => {
				let json = JSON.parse(e.data);
				console.log(`Result:${json['return']}`);
				let result = this.parse(json['output']);
				output.innerHTML = `<p>=> ${json['return']}</p>`;
				output.innerHTML += result;
			}
      let sender = JSON.stringify({
        "message": "eval",
        "data": src
      });
      console.log(sender);
			this.socket.send(sender);
		} else {
			console.log("Can't send the code.");
		}
	}

  save(editors) {
    if (this.state && this.modified) {
      let src = "";
      for (let editor of editors) {
        src += editor.$.editor.value + "\n";
        src += "#|OUTPUT\n" + editor.$.output.innerHTML + "\n|#\n";
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
        "data": src
      });
      this.socket.send(sender);
    } else {
      console.log("Can't save the code.");
    }
  }
}

window.onload = () => {
	window.ls = new LispSocket(LS_URI, FILE_PATH);
  window.onkeydown = (e) => {
    if (e.keyCode === 83 && e.ctrlKey) {
      e.preventDefault()
      window.ls.save(document.getElementsByTagName('custom-element'));
      return false;
    }
  }
	let container = document.getElementById('dm-container');
	let initial = document.getElementById('dm-initial');
	initial.connect(ls, container);
}
