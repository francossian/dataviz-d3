import { PieChart } from "./PieChart";

const data = [
  { name: "Mark", value: 90 },
  { name: "Robert", value: 12 },
  { name: "Emily", value: 34 },
  { name: "Marion", value: 53 },
  { name: "Nicolas", value: 98 },
];

function App() {
  return (
    <div style={{ display: "flex", justifyContent: "center", marginTop: 40 }}>
      <PieChart width={400} height={400} data={data} />
    </div>
  );
}

export default App
