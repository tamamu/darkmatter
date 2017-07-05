function plotLine(obj) {
  let vec = obj[":DATA"];
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

function plotScatter(obj) {
  let vec = obj[":DATA"];
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
     .text(obj[':XLABEL'])

  svg.append('g')
     .call(d3.axisLeft(y));

  svg.append('text')
     .attr('transform', 'rotate(-90)')
     .attr('y', -30)
     .attr('x', -150)
     .attr('dy', '1em')
     .attr('text-anchor', 'middle')
     .text(obj[':YLABEL']);

  return container.outerHTML;
}

function plotMatrix(obj) {
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
  return table.outerHTML;
}

StructRenderer = StructRenderer.concat([
    {name: '$<MATRIX>', short: null, render: plotMatrix},
    {name: 'DARKMATTER.PLOT:SCATTER', short: 'SCATTER', render: plotScatter},
    {name: 'DARKMATTER.PLOT:LINE', short: 'LINE', render: plotLine}
  ]);

