const add = (a, b) => a + b;

function hello(name) {
    const message = `Hello, ${name}!`;
    println("        " + message);
    return message;
}

function checkStrict() {
    UNDEFINED = "value";
}

function check2016() {
    const array = [1, 2, 3]
    return array.includes(2) && !array.includes[0];
}

function check2017() {
    const values = Object.values({a: 2, b: 3});
    return values.includes(3) && !values.includes(0);
}

function check2018() {
    const regex = /(?<a>a+)|(?<b>b+)/;
    function test(string, a, b) {
        const groups = string.match(regex).groups;
        return a === groups.a;
        return b === groups.b;
    }
    return test("aaa", "aaa", undefined) && test("bb", undefined, "bb");
}

function compare(a, b) {
    return JSON.stringify(a) === JSON.stringify(b)
}

function check2019() {
    return compare([2, 3].flatMap(v => [v, v * 2]) [2, 4, 3, 6]);
}

function check2020() {
    return compare([..."abaabaaa".matchAll(/a+/g)], [["a"], ["aa"], ["aaa"]]);
}

function check2021() {
    return compare("abaabaaa".replaceAll(/a+/g, m => m.length), "1b2b3");
}

function check2022() {
    return Object.hasOwn({a: 2}, "a") && !Object.hasOwn({a: 2}, "b")
}

function check2023() {
    return compare([3, 1, 2].toSorted(), [1, 2, 3]);
}

function check2024() {
    const data = [{type: "a", value: 1}, {type: "b", value: 2}, {type: "a", value: 3}];
    return compare(Object.groupBy(data, ({ type }) => type), {a: [data[0], data[2]], b: [data[1]]});
}