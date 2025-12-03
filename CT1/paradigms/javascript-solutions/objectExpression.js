// :NOTE: нет прототипов
function Expression(t) {
    this.value = t;
}

Expression.prototype.evaluate = function(x, y, z) {
    throw new Error("Method evaluate not implemented");
};

Expression.prototype.toString = function() {
    throw new Error("Method toString not implemented");
};

Expression.prototype.prefix = function() {
    return this.toString();
};

Expression.prototype.postfix = function() {
    return this.toString();
};

function Const(value) {
    Expression.call(this, value);
}
Const.prototype = Object.create(Expression.prototype);
Const.prototype.constructor = Const;

Const.prototype.evaluate = function() {
    return this.value;
};

Const.prototype.toString = function() {
    return this.value.toString();
};

function Variable(name) {
    Expression.call(this, name)
}
Variable.prototype = Object.create(Expression.prototype);
Variable.prototype.constructor = Variable;

Variable.prototype.evaluate = function(x, y, z) {
    switch(this.value) {
        case "x": return x;
        case "y": return y;
        case "z": return z;
        default: return NaN;
    }
};

Variable.prototype.toString = function() {
    return this.value;
};

const NaryOperation = (function() {
    function NaryOperation(f, Op, ...operands) {
        Expression.call(this, operands);
        this.f = f;
        this.Op = Op;
        this.operands = operands;
    }
    NaryOperation.prototype = Object.create(Expression.prototype);
    NaryOperation.prototype.constructor = NaryOperation;

    NaryOperation.prototype.evaluate = function(x, y, z) {
        const evaluatedOperands = this.operands.map(operand => operand.evaluate(x, y, z));
        return this.f(...evaluatedOperands);
    };

    NaryOperation.prototype.toString = function() {
        return `${this.operands.map(operand => operand.toString()).join(' ')} ${this.Op}`;
    };

    NaryOperation.prototype.prefix = function() {
        return `(${this.Op} ${this.operands.map(operand => operand.prefix()).join(' ')})`;
    };

    NaryOperation.prototype.postfix = function() {
        return `(${this.operands.map(operand => operand.postfix()).join(' ')} ${this.Op})`;
    };

    return NaryOperation;
})();

function fabric(f, Op) {
    const func = function (...args) {
        NaryOperation.call(this, f, Op, ...args);
    }
    func.prototype = Object.create(NaryOperation.prototype);
    func.prototype.constructor = func;
    func.argc = f.length;
    Ops[Op] = func;
    return func;
}

const Ops = {};

const Negate = fabric(a => -a, 'negate');
const Add = fabric((a, b) => a + b, '+');
const Subtract = fabric((a, b) => a - b, '-');
const Multiply = fabric((a, b) => a * b, '*');
const Divide = fabric((a, b) => a / b, '/');
const Clamp = fabric((x, min, max) => Math.min(Math.max(x, min), max), 'clamp');
const SoftClamp = fabric(
    (x, min, max, lambda) => min + (max - min) / (1 + Math.exp(lambda * ((max + min) / 2 - x))),
    'softClamp'
);
const SumCb = fabric(
    (...values) => values.map(x => x * x * x).reduce((sum, val) => sum + val, 0),
    'sumCb'
);
const MeanCb = fabric(
    (...values) => values.length === 0 ? 0 :
        values.map(x => x * x * x).reduce((sum, val) => sum + val, 0) / values.length,
    'meanCb'
);

const operations = {
    "+": { class: Add, operands: 2 },
    "-": { class: Subtract, operands: 2 },
    "*": { class: Multiply, operands: 2 },
    "/": { class: Divide, operands: 2 },
    "negate": { class: Negate, operands: 1 },
    "clamp": { class: Clamp, operands: 3 },
    "softClamp": { class: SoftClamp, operands: 4 },
    "sumCb": { class: SumCb, operands: -1 },
    "meanCb": { class: MeanCb, operands: -1 }
};

function ParserError(message) {
    Error.call(this);
    this.message = message;
    this.name = 'ParserError';
}

ParserError.prototype = Object.create(Error.prototype);

const Vars = ["x", "y", "z"];
function parse(str) {
    if (!str) {
        throw new ParserError("Empty input");
    }

    const tokens = str.trim().split(/\s+/);
    const stack = [];

    for (const token of tokens) {
        if (token in operations) {
            const op = operations[token];
            const operandCount = op.operands === -1 ? stack.length : op.operands;

            if (stack.length < operandCount) {
                throw new ParserError(`Not enough operands for ${token} (${operandCount} args)`);
            }

            const operands = [];
            for (let i = 0; i < operandCount; i++) {
                operands.unshift(stack.pop());
            }
            stack.push(new op.class(...operands));
        } else if (Vars.includes(token)) {
            stack.push(new Variable(token));
        } else if (!isNaN(+token)) {
            stack.push(new Const(parseFloat(token)));
        } else {
            throw new ParserError(`Unknown ${token}`);
        }
    }

    if (stack.length !== 1) {
        throw new ParserError(`Invalid expression: too much left in stack (${stack.length})`);
    }

    return stack.pop();
}

function parsePrefix(str) {
    if (!str) {
        throw new ParserError("Empty input");
    }

    let arr = str.split(/\s+|([()])/).filter(s => s);
    let stack = [];
    const opn = '(', cls = ')';

    for (let i = 0; i < arr.length; i++) {
        if (arr[i] === cls) {
            let buf = [];
            while (stack.length !== 0 && stack[stack.length - 1] !== opn) {
                buf.unshift(stack.pop());
            }
            if (stack.length === 0) {
                throw new ParserError(`Missing ${opn}`);
            }
            if (buf.length === 0) {
                throw new ParserError("Empty op");
            }
            buf.push(buf.shift());

            const buflen = buf.length - 1;
            if (!(buf[buflen] in operations)) {
                throw new ParserError(`${Vars.includes(buf[buflen]) ? "Variable" : "Const"} op (${buflen} args)`);
            }
            const op = operations[buf[buflen]];
            if (buf.filter(a => a in operations).length > 1) {
                throw new ParserError("Double input of operations");
            }
            if (op.operands !== -1 && op.operands !== buflen) {
                throw new ParserError(`Incorrect ${op.operands === 1 ? "unary" : "nary"} (${buflen} args)`);
            }

            stack.pop();
            stack.push(new op.class(...buf.slice(0, buflen)));
        } else if (arr[i] === opn) {
            stack.push(opn);
        } else if (arr[i] in operations) {
            stack.push(arr[i]);
        } else if (Vars.includes(arr[i])) {
            stack.push(new Variable(arr[i]));
        } else if (!isNaN(+arr[i])) {
            stack.push(new Const(parseFloat(arr[i])));
        } else {
            if (arr[i - 1] === opn || /^[a-z0-9]+$/i.test(arr[i])) {
                throw new ParserError(`Unknown ${(arr[i - 1] === opn) ? "operation" : "variable"}`);
            } else {
                throw new ParserError("Invalid number");
            }
        }
    }

    if (stack.length !== 1) {
        throw new ParserError(`Invalid expression: too much left in stack (${stack.length})`);
    }
    return stack.pop();
}