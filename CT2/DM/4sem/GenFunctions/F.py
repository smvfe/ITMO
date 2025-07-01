mod = 10**9 + 7
inv_2 = (mod + 1) // 2

def sqrt_1_plus_p(p, m):
    res = [0] * m
    res[0] = 1
    for k in range(1, m):
        p_k = p[k] if k < len(p) else 0
        sum_inner = 0
        for i in range(1, k):
            if i < len(res) and (k - i) < len(res):
                sum_inner = (sum_inner + res[i] * res[k - i]) % mod
        sum_inner = (p_k - sum_inner) % mod
        res[k] = sum_inner * inv_2 % mod
    return res

k, m = map(int, input().split())
c = list(map(int, input().split()))

p_mul_minus_4 = [0] * (m + 1)
for ci in c:
    p_mul_minus_4[ci] = (-4) % mod

SqP = sqrt_1_plus_p(p_mul_minus_4, m + 1)

P = [0] * (m + 1)
P[0] = 2
for i in range(1, m + 1):
    P[i] = SqP[i]

B = [0] * (m + 1)
B[0] = inv_2 % mod

for i in range(1, m + 1):
    summ = 0
    for k in range(1, i + 1):
        if k <= m and (i - k) <= m:
            summ = (summ + P[k] * B[i - k]) % mod
    B[i] = (-summ * inv_2) % mod

C = [(2 * B[i]) % mod for i in range(m + 1)]

print(' '.join(map(str, C[1:m+1])))
# (1 - sqrt(1 - 4 * P(x))) / (2 * P(x))
# P(x) = sum x^{c_i}