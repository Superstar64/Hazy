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
    return { a: 0 };
  },
};

export const trace = {
  a: 0,
  b: (text) => (thunk) => {
    console.log(force(text));
    return force(thunk);
  },
};

export const primIntToChar = {
  a: 0,
  b: (c) => force(c),
};

export { primIntToChar as primCharToInt };

export const primEqualInt = {
  a: 0,
  b: (x) => (y) => ({ a: +(force(x) === force(y)) }),
};

export const primLessThenEqualInt = {
  a: 0,
  b: (x) => (y) => ({ a: +(force(x) <= force(y)) }),
};

export const primIntMinBound = {
  a: 0,
  b: -2147483648,
};

export const primIntMaxBound = {
  a: 0,
  b: 2147483647,
};

export const primIntAdd = { a: 0, b: (x) => (y) => (force(x) + force(y)) | 0 };
export const primIntMinus = {
  a: 0,
  b: (x) => (y) => (force(x) - force(y)) | 0,
};
export const primIntMultiply = {
  a: 0,
  b: (x) => (y) => Math.imul(force(x), force(y)),
};
export const primIntNegate = { a: 0, b: (x) => -force(x) | 0 };
export const primIntAbs = { a: 0, b: (x) => -Math.abs(force(x)) | 0 };
export const primIntSignum = { a: 0, b: (x) => Math.sign(force(x)) };

export const primIntToInteger = {
  a: 0,
  b: (x) => BigInt(force(x)),
};

export const primIntQuot = {
  a: 0,
  b: (x) => (y) => (force(x) / force(y)) | 0,
};

export const primIntRem = {
  a: 0,
  b: (x) => (y) => (force(x) % force(y)) | 0,
};

export const primIntegerCastToInt = {
  a: 0,
  b: (x_) => {
    const x = force(x_);
    if (x > force(primIntMaxBound) || x < force(primIntMinBound)) {
      throw new Error("Enum Integer Overflow");
    }
    return Number(x);
  },
};

export const primIntegerTruncateToInt = {
  a: 0,
  b: (x) => Number(BigInt.asIntN(32, force(x))),
};

export const primEqualInteger = {
  a: 0,
  b: (x) => (y) => ({ a: +(force(x) === force(y)) }),
};

export const primLessThenEqualInteger = {
  a: 0,
  b: (x) => (y) => ({ a: +(force(x) <= force(y)) }),
};

export const primIntegerAdd = { a: 0, b: (x) => (y) => force(x) + force(y) };
export const primIntegerMinus = { a: 0, b: (x) => (y) => force(x) - force(y) };
export const primIntegerMultiply = {
  a: 0,
  b: (x) => (y) => force(x) * force(y),
};
export const primIntegerNegate = { a: 0, b: (x) => -force(x) };
export const primIntegerAbs = {
  a: 0,
  b: (x_) => {
    const x = force(x_);
    return x >= 0n ? x : -x;
  },
};

export const primIntegerSignum = {
  a: 0,
  b: (x_) => {
    const x = force(x_);
    return x === 0n ? 0n : x > 0n ? 1n : -1n;
  },
};

export const primIntegerQuot = {
  a: 0,
  b: (x) => (y) => force(x) / force(y),
};

export const primIntegerRem = {
  a: 0,
  b: (x) => (y) => force(x) % force(y),
};

export const numInt = helper["instance Hazy.Num HelperInt"];
export const numInteger = helper["instance Hazy.Num HelperInteger"];
export const enumBool = helper["instance Hazy.Enum HelperBool"];
export const enumChar = helper["instance Hazy.Enum HelperChar"];
export const enumInt = helper["instance Hazy.Enum HelperInt"];
export const enumInteger = helper["instance Hazy.Enum HelperInteger"];
export const eqBool = helper["instance Hazy.Eq HelperBool"];
export const eqChar = helper["instance Hazy.Eq HelperChar"];
export const eqTuple =
  (unpack) =>
  (...evidences) => ({
    a: {
      a: 0,
      b: (x_) => (y_) => {
        const x = unpack(force(x_));
        const y = unpack(force(y_));
        for (let i = 0; i < evidences.length; i++) {
          const equal = force(evidences[i].a)(x[i])(y[i]);
          if (!equal.a) {
            return { a: 0 };
          }
        }
        return { a: 1 };
      },
    },
    b: {
      a: 0,
      b: (x_) => (y_) => {
        const x = unpack(force(x_));
        const y = unpack(force(y_));
        for (let i = 0; i < evidences.length; i++) {
          const equal = force(evidences[i].b)(x[i])(y[i]);
          if (equal.a) {
            return { a: 1 };
          }
        }
        return { a: 0 };
      },
    },
  });
export const eqInt = helper["instance Hazy.Eq HelperInt"];
export const eqInteger = helper["instance Hazy.Eq HelperInteger"];
export const eqList = helper["instance Hazy.Eq HelperList"];
export const eqOrdering = helper["instance Hazy.Eq HelperOrdering"];
export const ordInt = helper["instance Hazy.Ord HelperInt"];
export const ordInteger = helper["instance Hazy.Ord HelperInteger"];
export const ordBool = helper["instance Hazy.Ord HelperBool"];
export const ordList = helper["instance Hazy.Ord HelperList"];
export const ordOrdering = helper["instance Hazy.Ord Ordering"];
export const realInt = helper["instance Hazy.Real HelperInt"];
export const realInteger = helper["instance Hazy.Real HelperInteger"];
export const integralInt = helper["instance Hazy.Integral HelperInt"];
export const integralInteger = helper["instance Hazy.Integral HelperInteger"];
export const functorList = helper["instance Hazy.Functor HelperList"];
export const applicativeList = helper["instance Hazy.Applicative HelperList"];
export const monadList = helper["instance Hazy.Monad HelperList"];
export const monadFailList = helper["instance Hazy.MonadFail HelperList"];
