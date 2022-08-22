import * as React from "react";
import Sketch from "react-p5";
import p5Types from "p5";

const finalCircleColors = [
  "#eab308",
  "#65a30d",
  "#14b8a6",
  "#3b82f6",
  "#8b5cf6",
  "#c026d3",
  "#be123c",
];

class FinalCircle {
  location: Vector;
  d: number;
  color: string;
  constructor(locaction: Vector, d: number) {
    this.location = locaction;
    this.d = d;
    this.color =
      finalCircleColors[Math.floor(Math.random() * finalCircleColors.length)];
  }
  update() {
    const target = Math.max(window.innerWidth, window.innerHeight) * 1.5;
    const error = target - this.d;
    const force = 0.05 * error;
    this.d += force;
  }
  draw(p5: p5Types) {
    p5.fill(this.color);
    p5.circle(this.location.x, this.location.y, this.d);
  }
}

type Vector = {
  x: number;
  y: number;
};

type AnimationProps = {
  confirmed: boolean;
  onFinal: () => void;
};

type TrailType = {
  location: Vector;
  color: string;
  d: number;
};

const addVectors = (vector1: Vector, vector2: Vector): Vector => ({
  x: vector1.x + vector2.x,
  y: vector1.y + vector2.y,
});

const scalarMult = (scalar: number, vector: Vector): Vector => ({
  x: vector.x * scalar,
  y: vector.y * scalar,
});

const calcErrorP = (vector1: Vector, vector2: Vector): Vector => ({
  x: vector1.x - vector2.x,
  y: vector1.y - vector2.y,
});

const calcErrorD = (vector1: Vector): Vector => ({
  x: -vector1.x,
  y: -vector1.y,
});

type ParticleType = {
  actual: Vector;
  target: Vector;
  d: number;
  color: string;
};

const steer = (object: any): Vector => {
  const errorP = calcErrorP(object.target, object.actual);
  const errorD = calcErrorD(object.actual);
  const force = addVectors(
    scalarMult(object.change, errorP),
    scalarMult(0, errorD) // this one has no effect, leave it in for now in case I make use of it later
  );
  return addVectors(object.actual, force);
};

const newTarget = (): Vector => ({
  x: Math.random() * window.innerWidth,
  y: Math.random() * window.innerHeight,
});

const trail: Array<TrailType> = [];
const colors = [
  "magenta",
  "green",
  "blue",
  "yellow",
  "red",
  "orange",
  "purple",
];

let currentLocation: Vector = { x: window.innerWidth, y: window.innerHeight };
let target: Vector = newTarget();
let loopCount = 0;
let finalTimer = 0;
let finalFlag = false;

let imgSize = 20;

const particles: Array<ParticleType> = [];

const finalCircle = new FinalCircle(
  { x: window.innerWidth / 2, y: window.innerHeight / 2 },
  10
);

// constants
const TRAIL_COUNT = 300;
const NEW_TARGET_INTERVAL = 20;
const PARTICLE_COUNT = 50;
const FINAL_COUNT_DOWN = 700;

export const BerryAnimation = ({ confirmed, onFinal }: AnimationProps) => {
  const setup = (p5: p5Types, canvasParentRef: Element) => {
    p5.createCanvas(window.innerWidth, window.innerHeight).parent(
      canvasParentRef
    );
    for (let i = 0; i < TRAIL_COUNT; i++) {
      trail.push({
        location: currentLocation,
        color: colors[Math.floor(Math.random() * colors.length)],
        d: 0,
      });
    }
    for (let i = 0; i < PARTICLE_COUNT; i++) {
      particles.push({
        actual: { x: window.innerWidth / 2, y: window.innerHeight / 2 },
        target: newTarget(),
        d: 40,
        color: colors[Math.floor(Math.random() * colors.length)],
      });
    }
  };

  const draw = (p5: p5Types) => {
    p5.clear();

    if (confirmed) {
      finalTimer += 1;
    }

    let isFinal = finalTimer >= FINAL_COUNT_DOWN;

    if (isFinal && !finalFlag) {
      onFinal();
      finalFlag = true;
    }

    // update
    const newLocation = steer({
      actual: currentLocation,
      target: target,
      change: 0.01,
    });
    currentLocation = newLocation;
    if (confirmed) {
      target = { x: window.innerWidth / 2, y: window.innerHeight / 2 };
    } else if (loopCount % NEW_TARGET_INTERVAL === 0) {
      target = newTarget();
    }
    if (!isFinal) {
      trail.push({
        location: currentLocation,
        color: colors[Math.floor(Math.random() * colors.length)],
        d: 0,
      });
    }
    if (trail.length >= TRAIL_COUNT || isFinal) {
      trail.shift();
    }

    if (isFinal) {
      for (let i = 0; i < particles.length; i++) {
        const particle = particles[i];
        if (particle.d === 0) {
          particles.splice(i, 1);
          continue;
        }
        const newParticleLocation = steer({
          actual: particle.actual,
          target: particle.target,
          change: 0.05,
        });
        particle.actual = newParticleLocation;
        particle.d -= 1;
      }

      finalCircle.update();
      if (imgSize < 400) {
        const error = 400 - imgSize;
        const force = 0.2 * error;
        imgSize += force;
      }
    }

    // draw

    if (isFinal) {
      finalCircle.draw(p5);

      for (let i = 0; i < particles.length; i++) {
        const particle = particles[i];
        p5.noStroke();
        p5.fill(particle.color);
        p5.circle(particle.actual.x, particle.actual.y, particle.d);
      }
    }

    for (let i = 0; i < trail.length; i++) {
      const isHead = i == trail.length - 1;
      if (isHead) {
        p5.fill("magenta");
      } else {
        p5.fill(trail[i].color);
      }
      // since diameter is set in here in draw, we need to update the diameter here also when in final phase
      trail[i].d = isFinal ? Math.max(trail[i].d - 1, 0) : i - TRAIL_COUNT;
      p5.noStroke();
      p5.circle(
        isHead ? trail[i].location.x : trail[i].location.x + Math.random() * 10,
        isHead ? trail[i].location.y : trail[i].location.y + Math.random() * 10,
        trail[i].d
      );
    }

    loopCount++;
  };

  const windowResized = (p5: p5Types) => {
    p5.resizeCanvas(window.innerWidth, window.innerHeight);
  };

  return <Sketch setup={setup} draw={draw} windowResized={windowResized} />;
};
