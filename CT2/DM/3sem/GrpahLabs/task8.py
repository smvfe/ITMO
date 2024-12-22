def chromatic_polynomial(n, edges):
     
    if len(edges) == 0:
        polynomial = [1] + [0] * n
        return polynomial
 
    if n == 1:
        return [1, 0]
 
    u, v = edges[0]
    edges_rest = edges[1:]
 
    poly_without_edge = chromatic_polynomial(n, edges_rest)
 
    new_edges = []
    for x, y in edges_rest:
        if x == v:
            x = u
        if y == v:
            y = u
        if x != y and (x, y) not in new_edges and (y, x) not in new_edges:
            new_edges.append((x, y))
     
    poly_with_edge = chromatic_polynomial(n - 1, new_edges)
 
    result = [0] * max(len(poly_without_edge), len(poly_with_edge) + 1)
    for i in range(len(poly_without_edge)):
        result[i] += poly_without_edge[i]
    for i in range(1, len(poly_with_edge) + 1):
        result[i] -= poly_with_edge[i - 1]
 
    return result
 
n, m = map(int, input().split())
edges = [tuple(map(int, input().split())) for _ in range(m)]
 
coefficients = chromatic_polynomial(n, edges)
degree = len(coefficients) - 1
 
print(degree)
print(" ".join(map(str, coefficients)))