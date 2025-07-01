from fractions import Fraction

r, k = map(int, input().split())
p = list(map(int, input().split()))

factorials = []
for i in range(k + 1):
    if i == 0:
        factorials.append(1)
    else:
        factorials.append(factorials[-1] * i)

ans = [Fraction(0) for _ in range(k + 1)] 

for i in range(k + 1):
    pi = p[i]
    if pi == 0:
        continue

    cur_poly = [Fraction(1)]
    for m in range(k):
        c = k - i - m
        new_poly = [Fraction(0) for _ in range(len(cur_poly) + 1)]
        for j in range(len(cur_poly)):
            coeff = cur_poly[j]
            new_poly[j] += coeff * c
            new_poly[j + 1] += coeff
        cur_poly = new_poly

    fact = factorials[k]
    cur_poly = [coeff / fact for coeff in cur_poly]

    factor = Fraction(pi, r ** i)
    cur_poly = [coeff * factor for coeff in cur_poly]

    for d in range(len(cur_poly)):
        if d <= k:
            ans[d] += cur_poly[d]

output = []
for coeff in ans:
    num = coeff.numerator
    denom = coeff.denominator
    if denom < 0:
        num = -num
        denom = -denom
    output.append(f"{num}/{denom}")

print(' '.join(output))