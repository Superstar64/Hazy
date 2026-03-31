import * as helper from "./Hazy/Helper.mjs";

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

export const errorText = {
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

export const putStrLnText = {
  a: 0,
  b: (thunk) => () => {
    console.log(force(thunk));
    return { a: 0 };
  },
};

export const traceText = {
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

export const primSTPure = {
  a: 0,
  b: (x) => () => force(x),
};

export const primSTBind = {
  a: 0,
  b: (m) => (f) => () => {
    let x = force(m)();
    return force(f)(x)();
  },
};
export const eqTuple =
  (unpack) =>
  (...evidences) => {
    const result = {
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
      b: undefined,
    };
    result.b = {
      a: 0,
      b: helper.defaultNotEqual(result),
    };
    return result;
  };

export const ordTuple =
  (unpack) =>
  (...evidences) => {
    const result = {
      a: eqTuple(unpack)(...evidences.map((x) => x.a)),
      b: {
        a: 0,
        b: (x_) => (y_) => {
          const x = unpack(force(x_));
          const y = unpack(force(y_));
          for (let i = 0; i < evidences.length; i++) {
            const result = force(evidences[i].b)(x[i])(y[i]);
            if (result.a == 0 || result.a == 2) {
              return { a: result.a };
            }
          }
          return { a: 1 };
        },
      },
      c: undefined,
      d: undefined,
      e: undefined,
      f: undefined,
      g: undefined,
      h: undefined,
    };
    result.c = {
      a: 0,
      b: helper.defaultLessThen(result),
    };
    result.d = {
      a: 0,
      b: helper.defaultLessThenEqual(result),
    };
    result.e = {
      a: 0,
      b: helper.defaultGreaterThen(result),
    };
    result.f = {
      a: 0,
      b: helper.defaultGreaterThenEqual(result),
    };
    result.g = {
      a: 0,
      b: helper.defaultMax(result),
    };
    result.h = {
      a: 0,
      b: helper.defaultMin(result),
    };
    return result;
  };

export {
  defaultPlus,
  defaultMinus,
  defaultMultiply,
  defaultNegate,
  defaultAbs,
  defaultSignum,
  defaultFromInteger,
  defaultSucc,
  defaultPred,
  defaultToEnum,
  defaultFromEnum,
  defaultEnumFrom,
  defaultEnumFromThen,
  defaultEnumFromTo,
  defaultEnumFromThenTo,
  defaultEqual,
  defaultNotEqual,
  defaultCompare,
  defaultLessThen,
  defaultLessThenEqual,
  defaultGreaterThen,
  defaultGreaterThenEqual,
  defaultMax,
  defaultMin,
  defaultToRational,
  defaultQuot,
  defaultRem,
  defaultDiv,
  defaultMod,
  defaultQuotRem,
  defaultDivMod,
  defaultToInteger,
  defaultDivide,
  defaultRecip,
  defaultFromRational,
  defaultFmap,
  defaultFconst,
  defaultPure,
  defaultAp,
  defaultLiftA2,
  defaultDiscardLeft,
  defaultDiscardRight,
  defaultBind,
  defaultThen,
  defaultReturn,
  defaultFail,
  "instance Hazy.Num HelperInt" as numInt,
  "instance Hazy.Num HelperInteger" as numInteger,
  "instance Hazy.Num HelperRatio" as numRatio,
  "instance Hazy.Enum HelperBool" as enumBool,
  "instance Hazy.Enum HelperChar" as enumChar,
  "instance Hazy.Enum HelperInt" as enumInt,
  "instance Hazy.Enum HelperInteger" as enumInteger,
  "instance Hazy.Enum HelperOrdering" as enumOrdering,
  "instance Hazy.Enum HelperUnit" as enumUnit,
  "instance Hazy.Enum HelperRatio" as enumRatio,
  "instance Hazy.Eq HelperBool" as eqBool,
  "instance Hazy.Eq HelperChar" as eqChar,
  "instance Hazy.Eq HelperInt" as eqInt,
  "instance Hazy.Eq HelperInteger" as eqInteger,
  "instance Hazy.Eq HelperList" as eqList,
  "instance Hazy.Eq HelperOrdering" as eqOrdering,
  "instance Hazy.Eq HelperRatio" as eqRatio,
  "instance Hazy.Ord HelperChar" as ordChar,
  "instance Hazy.Ord HelperInt" as ordInt,
  "instance Hazy.Ord HelperInteger" as ordInteger,
  "instance Hazy.Ord HelperBool" as ordBool,
  "instance Hazy.Ord HelperList" as ordList,
  "instance Hazy.Ord HelperOrdering" as ordOrdering,
  "instance Hazy.Ord HelperRatio" as ordRatio,
  "instance Hazy.Real HelperInt" as realInt,
  "instance Hazy.Real HelperInteger" as realInteger,
  "instance Hazy.Real HelperRatio" as realRatio,
  "instance Hazy.Integral HelperInt" as integralInt,
  "instance Hazy.Integral HelperInteger" as integralInteger,
  "instance Hazy.Fractional HelperRatio" as fractionalRatio,
  "instance Hazy.Functor HelperList" as functorList,
  "instance Hazy.Applicative HelperList" as applicativeList,
  "instance Hazy.Monad HelperList" as monadList,
  "instance Hazy.MonadFail HelperList" as monadFailList,
  "instance Hazy.Functor HelperST" as functorST,
  "instance Hazy.Applicative HelperST" as applicativeST,
  "instance Hazy.Monad HelperST" as monadST,
} from "./Hazy/Helper.mjs";
