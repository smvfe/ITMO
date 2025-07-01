mod = 998244353

def poly_sum(p, q, mod):
    max_len = max(len(p), len(q))
    ans = []
    for i in range(max_len):
        pi = p[i] if i < len(p) else 0
        qi = q[i] if i < len(q) else 0
        ans.append((pi + qi) % mod)
    while len(ans) > 1 and ans[-1] == 0:
        ans.pop()
    return ans

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

def poly_div(p, q, mod, end_ind=1000):
    a = [0] * end_ind
    m_q = len(q) - 1
    for k in range(end_ind):
        p_k = p[k] if k < len(p) else 0
        sum_qa = 0
        for i in range(1, m_q + 1):
            if k - i >= 0 and k - i < end_ind:
                sum_qa = (sum_qa + q[i] * a[k - i]) % mod
        a[k] = (p_k - sum_qa) % mod
    return a

n, m = map(int, input().split())
p = list(map(int, input().split()))
q = list(map(int, input().split()))

p = [x % mod for x in p]
q = [x % mod for x in q]

sum_poly = poly_sum(p, q, mod)
prod_poly = poly_mul(p, q, mod)

deg_prod = len(prod_poly) - 1
deg_sum = len(sum_poly) - 1

series = poly_div(p, q, mod)

print(deg_sum)
print(' '.join(map(str, sum_poly)))

print(deg_prod)
print(' '.join(map(str, prod_poly)))

print(' '.join(map(str, series[:1000])))