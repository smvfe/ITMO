k = int(input())
a = list(map(int, input().split()))
c = list(map(int, input().split()))

Q = [1] + [-ci for ci in c]

P = []
for j in range(k):
    pj = a[j]
    for i in range(1, j + 1):
        if j - i >= 0:
            pj -= c[i - 1] * a[j - i]
    P.append(pj)

deg_p = len(P) - 1
while deg_p > 0 and P[deg_p] == 0:
    deg_p -= 1
P_trimmed = P[:deg_p + 1]

deg_Q = k
Q_trimmed = Q[:k + 1]

print(deg_p)
print(' '.join(map(str, P_trimmed)))
print(deg_Q)
print(' '.join(map(str, Q_trimmed)))