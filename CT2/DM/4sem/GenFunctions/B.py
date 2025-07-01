mod = 998244353
inv_2 = (mod + 1) // 2

def sqrt_1_plus_p(p, m):
    res = [0] * m
    res[0] = 1
    for k in range(1, m):
        p_k = p[k] if k < len(p) else 0
        sum_inner = 0
        for i in range(1, k):
            if i <= len(res) and (k - i) < len(res):
                sum_inner = (sum_inner + res[i] * res[k - i]) % mod
        sum_inner = (p_k - sum_inner) % mod
        res[k] = sum_inner * inv_2 % mod
    return res

def exp_p(p, m):
    res = [0] * m
    res[0] = 1
    for k in range(1, m):
        sum_inner = 0
        for i in range(1, k + 1):
            if i >= len(p):
                p_i = 0
            else:
                p_i = p[i]
            term = i * p_i % mod
            term = term * res[k - i] % mod
            sum_inner = (sum_inner + term) % mod
        inv_k = pow(k, mod - 2, mod)
        res[k] = sum_inner * inv_k % mod
    return res

def ln_1_plus_p(p, m):
    q = [0] * m
    q[0] = 1
    for i in range(1, len(p)):
        if i < m:
            q[i] = p[i] % mod

    R = [0] * m
    R[0] = 1
    for k in range(1, m):
        sum_r = 0
        for i in range(1, k + 1):
            if i < len(q) and (k - i) < len(R):
                sum_r = (sum_r + q[i] * R[k - i]) % mod
        R[k] = (-sum_r) % mod

    p_prime = [0] * m
    for i in range(1, len(p)):
        if i - 1 < m:
            p_prime[i - 1] = (i * p[i]) % mod

    l_prime = [0] * m
    for k in range(m):
        sum_lp = 0
        for i in range(k + 1):
            if i < len(p_prime) and (k - i) < len(R):
                sum_lp = (sum_lp + p_prime[i] * R[k - i]) % mod
        l_prime[k] = sum_lp % mod

    l = [0] * m
    for k in range(1, m):
        if k - 1 < len(l_prime):
            inv_k = pow(k, mod - 2, mod)
            l[k] = l_prime[k - 1] * inv_k % mod
    return l

n, m = map(int, input().split())
p = list(map(int, input().split()))
p = [x % mod for x in p]

sqrt_series = sqrt_1_plus_p(p, m)
exp_series = exp_p(p, m)
log_series = ln_1_plus_p(p, m)

print(' '.join(map(str, sqrt_series[:m])))
print(' '.join(map(str, exp_series[:m])))
print(' '.join(map(str, log_series[:m])))