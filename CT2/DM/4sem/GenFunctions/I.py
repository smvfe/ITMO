mod = 104857601

def poly_mul(p, q, mod):
    m = len(p) - 1
    n = len(q) - 1
    prod = [0] * (m + n + 1)
    for i in range(m + 1):
        for j in range(n + 1):
            prod[i + j] = (prod[i + j] + p[i] * q[j]) % mod
    while len(prod) > 1 and prod[-1] == 0:
        prod.pop()
    return prod

k, n = map(int, input().split())
n -= 1
a = list(map(int, input().split()))
while len(a) < 2 * k:
    a.append(0)
c = list(map(int, input().split()))
q = [1]
for ci in c:
    q.append((-ci) % mod)

while n >= k:
    for i in range(k, 2 * k):
        a[i] = 0
        for j in range(1, len(q)):
            if i - j >= 0:
                a[i] = (a[i] - q[j] * a[i - j]) % mod
    q_minus = []
    for i in range(len(q)):
        sign = (-1) ** i
        val = q[i] * sign
        q_minus.append(val % mod)
    r = poly_mul(q, q_minus, mod)
    new_a = []
    for idx in range(len(a)):
        if idx % 2 == n % 2:
            new_a.append(a[idx] % mod)
    a = new_a
    while len(a) < 2 * k:
        a.append(0)
    new_q = []
    for i in range(0, len(r), 2):
        if i < len(r):
            new_q.append(r[i] % mod)
        else:
            new_q.append(0)
    new_q = new_q[:k+1]
    while len(new_q) < k + 1:
        new_q.append(0)
    q = new_q
    n = n // 2

print(a[n] % mod)