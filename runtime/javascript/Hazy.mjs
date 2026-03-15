import * as helper from "./Hazy/Helper.mjs";
export * from "./Hazy/Helper.mjs";

function force(thunk) {
  return thunk.a ? thunk.b() : thunk.b;
}

export function abort() {
  throw new Error("bottom");
}

export const placeholder = {
  a: 1,
  b() {
    throw new Error("placeholder");
  },
};

export const undefined = {
  a: 1,
  b() {
    throw new Error("undefined");
  },
};

export const error = {
  a: 0,
  b: (message) => {
    throw new Error(force(message));
  },
};

// todo perform replacement on invalid scalar values
export const pack = {
  a: 0,
  b: (list) => {
    let buffer = "";
    let current = force(list);
    while (current.a) {
      buffer += String.fromCodePoint(force(current.b));
      current = force(current.c);
    }
    return buffer;
  },
};

export const putStrLn = {
  a: 0,
  b: (thunk) => () => {
    console.log(force(thunk));
  },
};

export const intLessThenEqual = {
  a: 0,
  b: (x) => (y) => ({ a: +(force(x) <= force(y)) }),
};

export const integerLessThenEqual = {
  a: 0,
  b: (x) => (y) => ({ a: +(force(x) <= force(y)) }),
};

export const boundedIntMinBound = {
  a: 0,
  b: -2147483648,
};

export const boundedIntMaxBound = {
  a: 0,
  b: 2147483647,
};

export const numInt = {
  a: { a: 0, b: (x) => (y) => (force(x) + force(y)) | 0 },
  b: { a: 0, b: (x) => (y) => (force(x) - force(y)) | 0 },
  c: { a: 0, b: (x) => (y) => Math.imul(force(x), force(y)) },
  d: { a: 0, b: (x) => -force(x) | 0 },
  e: { a: 0, b: (x) => -Math.abs(force(x)) | 0 },
  f: { a: 0, b: (x) => Math.sign(force(x)) },
  g: { a: 0, b: (x) => Number(BigInt.asIntN(32, force(x))) },
};

export const numInteger = {
  a: { a: 0, b: (x) => (y) => force(x) + force(y) },
  b: { a: 0, b: (x) => (y) => force(x) - force(y) },
  c: { a: 0, b: (x) => (y) => force(x) * force(y) },
  d: { a: 0, b: (x) => -force(x) },
  e: {
    a: 0,
    b: (x) => {
      x = force(x);
      return x >= 0n ? x : -x;
    },
  },
  f: {
    a: 0,
    b: (x) => {
      x = force(x);
      return x == 0n ? 0n : x > 0n ? 1n : -1n;
    },
  },
  g: { a: 0, b: (x) => force(x) },
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
  a: placeholder,
  b: placeholder,
  c: placeholder,
  d: placeholder,
  e: placeholder,
  f: placeholder,
  g: placeholder,
  h: placeholder,
};

export const enumBool = {
  a: placeholder,
  b: placeholder,
  c: placeholder,
  d: placeholder,
  e: placeholder,
  f: placeholder,
  g: placeholder,
  h: placeholder,
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
  a: { a: 0, b: (x) => (y) => ({ a: +(force(x) === force(y)) }) },
  b: { a: 0, b: (x) => (y) => ({ a: +(force(x) !== force(y)) }) },
};

export const eqInt = {
  a: { a: 0, b: (x) => (y) => ({ a: +(force(x) === force(y)) }) },
  b: { a: 0, b: (x) => (y) => ({ a: +(force(x) !== force(y)) }) },
};

export const eqInteger = {
  a: { a: 0, b: (x) => (y) => ({ a: +(force(x) === force(y)) }) },
  b: { a: 0, b: (x) => (y) => ({ a: +(force(x) !== force(y)) }) },
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
