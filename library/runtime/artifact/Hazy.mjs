function force(thunk) {
  return thunk.a ? thunk.b() : thunk.b;
}

function boolean(b) {
  return { a: b ? 1 : 0 };
}

export const placeholder = {
  a: 1,
  b() {
    throw new Error("placeholder");
  },
};

export function abort() {
  throw new Error("bottom");
}

export const numInt = {
  a: placeholder,
  b: placeholder,
  c: placeholder,
  d: placeholder,
  e: placeholder,
  f: placeholder,
  g: placeholder,
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
  a: placeholder,
  b: placeholder,
  c: placeholder,
  d: placeholder,
  e: placeholder,
  f: placeholder,
  g: placeholder,
  h: placeholder,
};

export const enumInteger = enumInt;

export const enumBool = enumInt;

export const enumChar = enumInt;

export const eqBool = {
  a: { a: 0, b: (x) => (y) => boolean(force(x).a === force(y).a) },
  b: { a: 0, b: (x) => (y) => boolean(force(x).a !== force(y).a) },
};

export const eqChar = {
  a: { a: 0, b: (x) => (y) => boolean(force(x) === force(y)) },
  b: { a: 0, b: (x) => (y) => boolean(force(x) !== force(y)) },
};

export const eqInt = eqChar;

export const eqInteger = eqChar;

const placeholderDefault = (_evidence) => {
  throw new Exception("Placeholder");
};

export const defaultPlus = placeholderDefault;
export const defaultMinus = placeholderDefault;
export const defaultMultiply = placeholderDefault;
export const defaultNegate = placeholderDefault;
export const defaultAbs = placeholderDefault;
export const defaultSignum = placeholderDefault;
export const defaultFromInteger = placeholderDefault;
export const defaultSucc = placeholderDefault;
export const defaultPred = placeholderDefault;
export const defaultToEnum = placeholderDefault;
export const defaultFromEnum = placeholderDefault;
export const defaultEnumFrom = placeholderDefault;
export const defaultEnumFromThen = placeholderDefault;
export const defaultEnumFromTo = placeholderDefault;
export const defaultEnumFromThenTo = placeholderDefault;
export const defaultEqual = placeholderDefault;
export const defaultNotEqual = placeholderDefault;

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
