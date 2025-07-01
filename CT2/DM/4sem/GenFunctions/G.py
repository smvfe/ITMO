class Parser:
    def __init__(self, s):
        self.tokens = self.tokenize(s)
        self.current = 0

    def tokenize(self, s):
        tokens = []
        for i in range(len(s)):
            c = s[i]
            if c in 'B()P,SL':
                tokens.append(c)
        return tokens
    
    def parse(self):
        token = self.tokens[self.current]
        if token == 'B':
            self.current += 1
            return [0, 1, 0, 0, 0, 0, 0]
        elif token in ['L', 'S', 'P']:
            op = token
            self.current += 1
            self.current += 1
            if op == 'P':
                x = self.parse()
                self.current += 1
                y = self.parse()
                self.current += 1
                return self.P(x, y)
            else:
                arg = self.parse()
                self.current += 1
                if op == 'L':
                    return self.L(arg)
                elif op == 'S':
                    return self.S(arg)

    def L(self, arg):
        l = [0] * 7
        l[0] = 1
        for n in range(1, 7):
            total = 0
            for k in range(1, n + 1):
                if k > len(arg) - 1:
                    continue
                total += arg[k] * l[n - k]
            l[n] = total
        return l

    def S(self, arg):
        y = [0.0] * 7
        for m in range(1, 7):
            divs = self.divs(m)
            sum_y = 0.0
            for d in divs:
                pos = m // d
                if pos > 6:
                    continue
                if pos >= len(arg):
                    continue
                sum_y += arg[pos] / d
            y[m] = sum_y
        s = [0] * 7
        s[0] = 1
        for n in range(1, 7):
            total = 0.0
            for k in range(1, n + 1):
                if k > len(y) - 1:
                    continue
                total += k * y[k] * s[n - k]
            s[n] = int(round(total / n))
        return s

    def P(self, x, y):
        p = [0] * 7
        for n in range(7):
            total = 0
            for k in range(n + 1):
                if k > 6 or (n - k) > 6:
                    continue
                total += x[k] * y[n - k]
            p[n] = total
        return p

    def divs(self, m):
        divs = []
        for i in range(1, m + 1):
            if m % i == 0:
                divs.append(i)
        return divs

s = input().strip()
parser = Parser(s)
ans = parser.parse()
print(' '.join(map(str, ans[:7])))