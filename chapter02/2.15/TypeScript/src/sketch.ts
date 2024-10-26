import p5 from "p5";

const size = window.innerHeight * 4 / 5;

let inside = 0;
let outside = 0;
let total = 0;
let piApprox = 0;

const sketch = (p: p5) => {
  p.setup = () => {
    p.createCanvas(size, size);
    p.background(0, 0, 0);
    p.textFont("monospace");
  };


  p.draw = () => {
    drawPoint();
    drawPi();
  };

  const drawPoint = () => {
    const x = p.random(0, 1);
    const y = p.random(0, 1);
    total += 1;
    if (x * x + y * y < 1) {
      inside++;
      p.stroke(0, 0, 255);
    } else {
      outside++;
      p.stroke(255, 0, 0);
    }
    p.strokeWeight(1);
    p.point(x * size, y * size);
    piApprox = inside / total * 4;
  };

  const drawPi = () => {
    p.strokeWeight(1);
    p.stroke(128);
    p.fill(50);
    p.rect(0, 0, 300, 50);
    p.color(255);
    p.strokeWeight(0.2);
    p.fill(255);
    p.stroke(255);

    p.text(`PI    = ${piApprox}`, 10, 20);
    p.text(`total = ${total}`, 10, 35);
  };
};

export default sketch;
