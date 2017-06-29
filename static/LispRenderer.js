
class LispRenderer {
  constructor(socket) {
    this.socket = socket;
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
              .domain([0, d3.max(vec, d => d[0])])
							.range([0, 300]);
		let y = d3.scaleLinear()
              .domain([0, d3.max(vec, d => d[1])])
							.range([300, 0]);

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
				.attr('cx', d => x(d[0]))
				.attr('cy', d => y(d[1]))
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

	plotStruct(obj) {
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

  onEval(src, cellId) {
    return (resolve, reject) => {
      this.socket.eval(src, cellId).then((obj) => {
        let rendering = obj.rendering;
        console.log(obj);
        if (rendering) {
          let val = obj['returnValue'];
          let evaluated = obj['result'];
          let res = "";
          let idx = 0;
          let beforeIdx = 0;
          let contents = null;
          while (evaluated.length > idx) {
            [contents, idx] = ResultParser.parse(evaluated, idx)
            if (typeof(contents) === 'object') {
              if (contents.mark === '#S') {
                res += this.plotStruct(contents);
              } else if (contents.mark === '#') {
                res += this.plotArray(contents);
              } else {
                res += evaluated.substring(beforeIdx, idx);
              }
            } else {
              res += evaluated.substring(beforeIdx, idx);
            }
            beforeIdx = idx;
          }
          resolve({rendering: true, returnValue: val, result: res});
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

function arrayToList(obj) {
  if (typeof(obj) === 'object' && !Array.isArray(obj)) {
    return obj.children.map(arrayToList);
  }
  return obj;
}

