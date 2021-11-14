d3.csv('https://raw.githubusercontent.com/jshi361/Data608/main/Assignment6/ue_industry.csv', data => {

    // Define your scales and generator here.
    console.log(data);

    
    const xScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.index))
        .range([1180, 20]);
    
    const yScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.Agriculture))
        .range([580, 20]);
    
    let line10 = d3.line()
        .x(d => xScale(d.index))
        .y(d => yScale(d.Agriculture));
    

    d3.select('#answer1')
        // append more elements here
        .selectAll('path')
        .data(data)
        .enter()        
        .append('path')
        .attr('d', line10(data))
        .attr("stroke", "steelblue")
        .attr("stroke-width", 1.5)



});
