import * as helper from "./Hazy/Helper.mjs";
export * from "./Hazy/Helper.mjs";

export function done(thunk, value) {
  Object.defineProperty(thunk, "v", { value });
  return value;
}

export function abort() {
  throw new Error("bottom");
}

export const placeholder = {
  get v() {
    throw new Error("placeholder");
  },
};

const _undefined = {
  get v() {
    throw new Error("undefined");
  },
};
export { _undefined as "undefined" };

export const error = {
  v: (message) => {
    throw new Error(message.v);
  },
};

// todo perform replacement on invalid scalar values
export const pack = {
  v: (list) => {
    let buffer = "";
    let current = list.v;
    while (current.a) {
      buffer += String.fromCodePoint(current.b.v);
      current = current.c.v;
    }
    return buffer;
  },
};

export const putStrLn = {
  v: (text) => () => {
    console.log(text.v);
  },
};

export const trace = {
  v: (text) => (thunk) => {
    console.log(text.v);
    return thunk.v;
  },
};

export const intLessThenEqual = {
  v: (x) => (y) => ({ a: +(x.v <= y.v) }),
};

export const integerLessThenEqual = {
  v: (x) => (y) => ({ a: +(x.v <= y.v) }),
};

export const boundedIntMinBound = {
  v: -2147483648,
};

export const boundedIntMaxBound = {
  v: 2147483647,
};

export const numInt = {
  a: { v: (x) => (y) => (x.v + y.v) | 0 },
  b: { v: (x) => (y) => (x.v - y.v) | 0 },
  c: { v: (x) => (y) => Math.imul(x.v, y.v) },
  d: { v: (x) => -x.v | 0 },
  e: { v: (x) => -Math.abs(x.v) | 0 },
  f: { v: (x) => Math.sign(x.v) },
  g: { v: (x) => Number(BigInt.asIntN(32, x.v)) },
};

export const numInteger = {
  a: { v: (x) => (y) => x.v + y.v },
  b: { v: (x) => (y) => x.v - y.v },
  c: { v: (x) => (y) => x.v * y.v },
  d: { v: (x) => -x.v },
  e: {
    v: (x) => {
      x = x.v;
      return x >= 0n ? x : -x;
    },
  },
  f: {
    v: (x) => {
      x = x.v;
      return x === 0n ? 0n : x > 0n ? 1n : -1n;
    },
  },
  g: { v: (x) => x.v },
};

export const enumInt = {
  a: helper.enumIntSucc,
  b: helper.enumIntPred,
  c: helper.enumIntToEnum,
  d: helper.enumIntFromEnum,
  e: helper.enumIntEnumFrom,
  f: helper.enumIntEnumFromThen,
  g: helper.enumIntEnumFromTo,
  h: helper.enumIntEnumFromThenTo,
};

export const enumInteger = {
  a: helper.enumIntegerSucc,
  b: helper.enumIntegerPred,
  c: { v: (x) => BigInt(x.v) },
  d: {
    v: (x) => {
      x = x.v;
      if (x > boundedIntMaxBound.v || x < boundedIntMinBound.v) {
        throw new Error("Enum Integer Overflow");
      }
      return Number(x);
    },
  },
  e: helper.enumIntegerEnumFrom,
  f: helper.enumIntegerEnumFromThen,
  g: helper.enumIntegerEnumFromTo,
  h: helper.enumIntegerEnumFromThenTo,
};

export const enumBool = {
  a: helper.enumBoolSucc,
  b: helper.enumBoolPred,
  c: helper.enumBoolToEnum,
  d: helper.enumBoolFromEnum,
  e: helper.enumBoolEnumFrom,
  f: helper.enumBoolEnumFromThen,
  g: helper.enumBoolEnumFromTo,
  h: helper.enumBoolEnumFromThenTo,
};

export const enumChar = {
  a: placeholder,
  b: placeholder,
  c: placeholder,
  d: placeholder,
  e: placeholder,
  f: placeholder,
  g: placeholder,
  h: placeholder,
};

export const eqBool = {
  a: helper.eqBoolEqual,
  b: helper.eqBoolNotEqual,
};

export const eqChar = {
  b: { v: (x) => (y) => ({ a: +(x.v !== y.v) }) },
  a: { v: (x) => (y) => ({ a: +(x.v === y.v) }) },
};

export const eqInt = {
  a: { v: (x) => (y) => ({ a: +(x.v === y.v) }) },
  b: { v: (x) => (y) => ({ a: +(x.v !== y.v) }) },
};

export const eqInteger = {
  a: { v: (x) => (y) => ({ a: +(x.v === y.v) }) },
  b: { v: (x) => (y) => ({ a: +(x.v !== y.v) }) },
};

export const functorList = {
  a: placeholder,
  b: placeholder,
};

export const applicativeList = {
  a: functorList,
  b: placeholder,
  c: placeholder,
  d: placeholder,
  e: placeholder,
  f: placeholder,
};

export const monadList = {
  a: applicativeList,
  b: placeholder,
  c: placeholder,
  d: placeholder,
};

export const monadFailList = {
  a: monadList,
  b: placeholder,
};
