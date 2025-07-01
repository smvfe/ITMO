import math

def P_coeff(r, d, p_coeff):
    p = []
    for m in range(d + 1):
        sum_part = 0
        for s in range(m + 1):
            k = m - s
            if k < 0 or k > d:
                continue
            pk = 0
            for i in range(d + 1):
                pk += p_coeff[i] * (k ** i)
            comb_val = math.comb(d + 1, s)
            sign = (-1) ** s
            term = comb_val * sign * pk
            sum_part += term
        c_m = (r ** m) * sum_part
        p.append(c_m)
    return p

def Q_coeff(r, d):
    q = []
    for i in range(d + 2):
        coeff = math.comb(d + 1, i) * ((-r) ** i)
        q.append(coeff)
    return q


r = int(input())
d = int(input())
p_coeff = list(map(int, input().split()))

p = P_coeff(r, d, p_coeff)

while len(p) > 1 and p[-1] == 0:
    p.pop()

q = Q_coeff(r, d)

print(len(p) - 1)
print(' '.join(map(str, p)))

print(d + 1)
print(' '.join(map(str, q)))