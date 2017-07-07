class LinePlot {
  constructor(obj) {
    this.vec = obj[":DATA"];
    let data = [];
    for (let i=0; i < this.vec.length; i++) {
      data.push([i, this.vec[i]]);
    }
    this.element = document.createElement('div');
    this.element.style.textAlign = 'center';
    this.x = d3.scaleLinear()
              .range([0, 300]);
    this.y = d3.scaleLinear()
              .range([300, 0]);
    this.x.domain(d3.extent(data, d => d[0])).nice();
    this.y.domain(d3.extent(data, d => d[1])).nice();
    this.xAxis = d3.axisBottom().scale(this.x);
    this.yAxis = d3.axisLeft().scale(this.y);

    this.line = d3.line()
                 .x(d => this.x(d[0]))
                 .y(d => this.y(d[1]));
    let svg = d3.select(this.element)
                .append('svg')
                .attr('width', '400')
                .attr('height', '400')
                .append('g')
                  .attr('transform', 'translate(40, 40)');
    svg.append('g')
       .attr('class', 'x axis')
       .attr('transform', 'translate(0,300)')
       .call(this.xAxis);
    svg.append('g')
       .attr('class', 'y axis')
       .call(this.yAxis);
    svg.append('path')
       .datum(data)
       .attr('class', 'line')
       .attr('fill', 'none')
       .attr('stroke', 'white')
       .attr('stroke-width', 1.5)
       .attr('d', this.line);
    this.element = this.element;
  }

  update(obj) {
    this.vec = obj.vec;
    let data = [];
    for (let i=0; i < this.vec.length; i++) {
      data.push([i, this.vec[i]]);
    }
    this.x.domain(d3.extent(data, d => d[0])).nice();
    this.y.domain(d3.extent(data, d => d[1])).nice();
    let svg = d3.select(this.element).transition();

    svg.select('.line')
       .duration(500)
       .attr('d', this.line(data));
    svg.select('.x.axis')
       .duration(500)
       .call(this.xAxis);
    svg.select('.y.axis')
       .duration(500)
       .call(this.yAxis);
  }

  toString() {
    return this.element.outerHTML;
  }
}

class ScatterPlot {
  static chooseColor(d) {
    if (d.length >= 2) {
      return d3.schemeCategory10[d[2]];
    } else {
      return 'white';
    }
  }

  constructor(obj) {
    this.vec = obj[":DATA"];
    if (this.vec.$type === '$<MATRIX>') this.vec = this.vec.data;
    this.element = document.createElement('div');
    this.element.style.textAlign = 'center';
    this.x = d3.scaleLinear()
              .domain([0, d3.max(this.vec, d => d[0])])
              .range([0, 300]);
    this.y = d3.scaleLinear()
              .domain([0, d3.max(this.vec, d => d[1])])
              .range([300, 0]);
    this.xAxis = d3.axisBottom(this.x);
    this.yAxis = d3.axisLeft(this.y);

    let svg = d3.select(this.element)
                .append('svg')
                .attr('wihth', '300')
                .attr('height', '300')
                .attr('style', 'overflow: visible; margin: 40px;')
                .append('g')
                  .attr('transform', 'translate(40, 0)');

    svg.selectAll('circle')
       .data(this.vec)
       .enter()
       .append('circle')
        .attr('class', 'mark')
        .attr('fill', ScatterPlot.chooseColor)
        .attr('cx', d => this.x(d[0]))
        .attr('cy', d => this.y(d[1]))
        .attr('r', 5);

    svg.append('g')
       .attr('class', 'x axis')
       .attr('transform', 'translate(0,300)')
       .call(this.xAxis);

    svg.append('text')
       .attr('transform', 'translate(150,340)')
       .style('text-anchor', 'middle')
       .text(obj[':XLABEL'])

    svg.append('g')
       .attr('class', 'y axis')
       .call(this.yAxis);

    svg.append('text')
       .attr('transform', 'rotate(-90)')
       .attr('y', -40)
       .attr('x', -150)
       .attr('dy', '1em')
       .attr('text-anchor', 'middle')
       .text(obj[':YLABEL']);
  }

  update(obj) {
    this.vec = obj.vec;
    if (this.vec.$type === '$<MATRIX>') this.vec = this.vec.data;
    this.x = d3.scaleLinear()
              .domain([0, d3.max(this.vec, d => d[0])])
              .range([0, 300]);
    this.y = d3.scaleLinear()
              .domain([0, d3.max(this.vec, d => d[1])])
              .range([300, 0]);

    let svg = d3.select(this.element);
    svg.selectAll('circle')
       .data(this.vec)
       .transition()
       .duration(500)
       .attr('fill', ScatterPlot.chooseColor)
       .attr('cx', d => this.x(d[0]))
       .attr('cy', d => this.y(d[1]));
    svg.select('.x.axis')
       .transition()
       .duration(500)
       .call(this.xAxis);
    svg.select('.y.axis')
       .transition()
       .duration(500)
       .call(this.yAxis);
  }

  toString() {
    return this.element.outerHTML;
  }
}

class MatrixPlot {
  constructor(obj) {
    let vec = obj.data;
    for (let i=0; i < obj.dim[0]; i++) {
      vec[i] = [i].concat(vec[i]);
    }
    let cols = ['Idx'];
    for (let i=0; i < obj.dim[1]; i++) {
      cols[i+1] = i;
    }
    vec = [cols].concat(vec);

    let table = document.createElement('table');
    d3.select(table)
      .selectAll('tr')
        .data(vec)
        .enter()
        .append('tr')
        .selectAll('td')
          .data(row => d3.entries(row))
          .enter()
          .append('td')
          .text(d => d.value);
    this.element = table;
  }

  toString() {
    return this.element.outerHTML;
  }
}

StructRenderer = StructRenderer.concat([
    {name: '$<MATRIX>', short: null, renderer: MatrixPlot},
    {name: 'DARKMATTER.PLOT:SCATTER', short: 'SCATTER', renderer: ScatterPlot},
    {name: 'DARKMATTER.PLOT:LINE', short: 'LINE', renderer: LinePlot}
  ]);

