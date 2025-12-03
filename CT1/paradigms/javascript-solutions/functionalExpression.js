const nary = operation => (...operands) => (...vars) =>
    operation(...operands.map(operand => operand(...vars)));

const cnst = value => () => value;
const tau = cnst(2 * Math.PI);
const phi = cnst((1 + Math.sqrt(5)) / 2);

const variable = name => (x, y, z, t) => {
    switch (name) {
        case "x": return x;
        case "y": return y;
        case "z": return z;
        case "t": return t;
        default: return NaN;
    }
};

const add = nary((a, b) => a + b);
const subtract = nary((a, b) => a - b);
const multiply = nary((a, b) => a * b);
const divide = nary((a, b) => a / b);
const negate = nary(a => -a);

const sinh = nary(a => Math.sinh(a));
const cosh = nary(a => Math.cosh(a));
const power = nary((a, b) => Math.pow(a, b));
const log = nary((a, b) => Math.log(Math.abs(b)) / Math.log(Math.abs(a)));

const testProg = add(
    subtract(
        multiply(variable("x"), variable("x")),
        multiply(cnst(2), variable("x"))
    ),
    cnst(1)
);

for (let x = 0; x <= 10; x++) {
    println(`f(${x}) = ${testProg(x)}`);
}