mod = 998244353

k, n = map(int, input().split())

# Предвычисление биномиальных коэффициентов C(n, k) для n до max_n
max_c = max(k, n)
C = [[0] * (max_c + 1) for _ in range(max_c + 1)]
C[0][0] = 1
for i in range(1, max_c + 1):
    C[i][0] = 1
    for j in range(1, i + 1):
        C[i][j] = (C[i-1][j] + C[i-1][j-1]) % mod

# Построение функции A
sizeA = (k - 1 + 1) // 2  # Эквивалентно ((k-1) // 2) + ((k-1) % 2 != 0)
A = []
for i in range(sizeA):
    n_comb = (k - 1) - i - 1
    k_comb = i
    if n_comb < 0 or k_comb < 0 or k_comb > n_comb:
        a = 0
    else:
        a = C[n_comb][k_comb]
    sign = 1 if i % 2 == 0 else -1
    A.append((a * sign) % mod)

# Построение функции B
sizeB = (k + 1) // 2
B = []
for i in range(sizeB):
    n_comb = k - i - 1
    k_comb = i
    if n_comb < 0 or k_comb < 0 or k_comb > n_comb:
        b = 0
    else:
        b = C[n_comb][k_comb]
    sign = 1 if i % 2 == 0 else -1
    B.append((b * sign) % mod)

# Вычисление обратной функции InvB
if not B:
    InvB = []
else:
    inv_B0 = pow(B[0], mod - 2, mod)
    InvB = [inv_B0]
    for i in range(1, n + 1):
        total = 0
        for j in range(1, i + 1):
            if j < len(B):
                b_j = B[j]
            else:
                b_j = 0
            if i - j < len(InvB):
                invb = InvB[i - j]
            else:
                invb = 0
            total = (total + b_j * invb) % mod
        new_coef = (-total * inv_B0) % mod
        InvB.append(new_coef)

# Умножение A и InvB
result = [0] * (n + 1)
for i in range(n + 1):
    total = 0
    for j in range(len(A)):
        if i - j >= 0 and i - j < len(InvB):
            total = (total + A[j] * InvB[i - j]) % mod
    result[i] = total

# Выводим результат для i от 1 до n
for i in range(0, n ):
    print(result[i] % mod)