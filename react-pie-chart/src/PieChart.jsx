import * as d3 from "d3";

export const PieChart = ({ width, height, data }) => {
  const radius = Math.min(width, height) / 2;

  const pieGenerator = d3.pie().value((d) => d.value);
  const pie = pieGenerator(data);
  const arcPathGenerator = d3.arc();

  const arcs = pie.map((p) =>
    arcPathGenerator({
      innerRadius: 0,
      outerRadius: radius,
      startAngle: p.startAngle,
      endAngle: p.endAngle,
    })
  );

  const colorScale = d3.scaleOrdinal(d3.schemeTableau10);

  return (
    <div>
      <svg width={width} height={height}>
        <g transform={`translate(${width / 2}, ${height / 2})`}>
          {arcs.map((arc, i) => (
            <path key={i} d={arc} fill={colorScale(i)} />
          ))}
        </g>
      </svg>
    </div>
  );
};
