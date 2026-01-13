function force(thunk) {
  return thunk.a ? thunk.b() : thunk.b;
}

function strict(b) {
  return {
    a: 0,
    b,
  };
}

function strict1(f) {
  return strict((x) => f(force(x)));
}

function strict2(f) {
  return strict((x) => (y) => f(force(x), force(y)));
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
  a: strict2((x, y) => x + y),
  b: strict2((x, y) => x - y),
  c: strict2((x, y) => x * y),
  d: strict2((x) => -x),
  e: strict1((x) => (x >= 0n ? x : -x)),
  f: strict1((x) => (x == 0n ? 0n : x > 0n ? 1n : -1n)),
  g: strict1((x) => x),
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
  a: strict2((x, y) => boolean(x === y)),
  b: strict2((x, y) => boolean(x !== y)),
};

export const eqChar = eqBool;

export const eqInt = eqBool;

export const eqInteger = eqBool;

// todo perform replacement on invalid scalar values
export const pack = strict((list) => {
  let buffer = "";
  let current = force(list);
  while (current.a) {
    buffer += force(current.b);
    current = force(current.c);
  }
  return buffer;
});

export const putStrLn = strict((thunk) => () => {
  console.log(force(thunk));
});
