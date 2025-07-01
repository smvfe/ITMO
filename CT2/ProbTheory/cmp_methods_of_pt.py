from scipy import stats
import math
from tabulate import tabulate
from scipy.special import gammaln

def binomial_log_probability(n, p, k):
    log_comb = gammaln(n + 1) - gammaln(k + 1) - gammaln(n - k + 1)
    log_prob = log_comb + k * math.log(p) + (n - k) * math.log(1 - p)
    return log_prob

def binomial_probability(n, p, k):
    if n < 10000:
        prob = math.comb(n, k) * (p ** k) * ((1 - p) ** (n - k))
    else:
        prob = binomial_log_probability(n, p, k)
        return math.exp(prob) if prob > -700 else 0.0
    return prob

def binomial_leq_k(n, p, k):
    return sum(binomial_probability(n, p, i) for i in range(k + 1))

def most_likely_success(n, p):
    lower_bound = p * (n + 1) - 1
    upper_bound = p * (n + 1)
    if upper_bound.is_integer():
        k1 = int(lower_bound)
        k2 = int(upper_bound)
        
        if 0 < k1 <= n and 0 < k2 <= n:
            p1 = binomial_probability(n, p, k1)
            p2 = binomial_probability(n, p, k2)
            return k1 if p1 >= p2 else k2
        elif 0 < k1 <= n:
            return k1
        else:
            return k2
    else:
        k_star = math.ceil(lower_bound)
        return min(max(1, k_star), n)

def poisson_approximation(n, p, k):
    lam = n * p 
    log_prob = -lam + k * math.log(lam) - sum(math.log(i) for i in range(1, k + 1))
    return math.exp(log_prob) if log_prob > -700 else 0.0

def poisson_leq_k(n, p, k, strt = 0):
    return sum(poisson_approximation(n, p, i) for i in range(strt, k + 1))

def local_moivre_laplace(n, p, k):
    q = 1 - p
    npq = n * p * q
    mean = n * p
    return (1 / math.sqrt(2 * math.pi * npq)) * math.exp(-0.5 * ((k - mean) ** 2) / npq)

def integral_moivre_laplace(n, p, a, b):
    q = 1 - p
    npq = n * p * q
    a_approx = a - 0.5
    b_approx = b + 0.5
    
    z_a = (a_approx - n*p) / math.sqrt(npq)
    z_b = (b_approx - n*p) / math.sqrt(npq)
    
    return stats.norm.cdf(z_b) - stats.norm.cdf(z_a)

def analyze_bernoulli(n_values, p_values):
    results = []
    for n in n_values:
        for p in p_values:
            q = 1 - p
            npq = n * p * q
            
            lower = max(0, math.floor(n/2 - math.sqrt(npq)))
            upper = min(n, math.ceil(n/2 + math.sqrt(npq)))
            k_star = most_likely_success(n, p)
            
            exact_interval = binomial_leq_k(n, p, upper) - binomial_leq_k(n, p, lower - 1)    
            exact_leq_5 = binomial_leq_k(n, p, 5)
            exact_k_star = binomial_probability(n, p, k_star)
            
            poisson_interval = poisson_leq_k(n, p, upper, lower)
            poisson_le_5 = poisson_leq_k(n, p, 5)
            poisson_k_star = poisson_approximation(n, p, k_star)
            
            local_interval = sum(local_moivre_laplace(n, p, k) 
                                for k in range(lower, upper+1))
            local_le_5 = sum(local_moivre_laplace(n, p, k) 
                            for k in range(6) if not isinstance(local_moivre_laplace(n, p, k), str))
            local_k_star = local_moivre_laplace(n, p, k_star)

            integral_interval = integral_moivre_laplace(n, p, lower, upper)
            integral_le_5 = integral_moivre_laplace(n, p, 0, 5)
            integral_k_star = integral_moivre_laplace(n, p, k_star, k_star)

            results.append({
                'n': n,
                'p': p,
                'interval': f"[{lower}, {upper}]",
                'k_star': k_star,
                'exact_interval': exact_interval,
                'exact_leq_5': exact_leq_5,
                'exact_k_star': exact_k_star,
                'poisson_interval': poisson_interval,
                'poisson_le_5': poisson_le_5,
                'poisson_k_star': poisson_k_star,
                'local_interval': local_interval,
                'local_le_5': local_le_5,
                'local_k_star': local_k_star,
                'integral_interval': integral_interval,
                'integral_le_5': integral_le_5,
                'integral_k_star': integral_k_star
            })
    return results

def format_value(value):
    if abs(value) < 1e-10:
        return f"{value:.10e}"
    return round(float(value), 10)

def print_results(results, filename="bernoulli_analysis_results.txt"):
    headers = ["n", "p", "Interval", "k*", 
               "Exact P(interval)", "Poisson P(interval)", "Local ML P(interval)", "Integral ML P(interval)",
               "Exact P(<=5)", "Poisson P(<=5)", "Local ML P(<=5)", "Integral ML P(<=5)",
               "Exact P(k*)", "Poisson P(k*)", "Local ML P(k*)", "Integral ML P(k*)??"]
    
    table_data = []
    for r in results:
        row = [
            r['n'], r['p'], r['interval'], r['k_star'],
            format_value(r['exact_interval']),
            format_value(r['poisson_interval']),
            format_value(r['local_interval']),
            format_value(r['integral_interval']),
            format_value(r['exact_leq_5']),
            format_value(r['poisson_le_5']),
            format_value(r['local_le_5']),
            format_value(r['integral_le_5']),
            format_value(r['exact_k_star']),
            format_value(r['poisson_k_star']),
            format_value(r['local_k_star']),
            format_value(r['integral_k_star'])
        ]
        table_data.append(row)
    
    table_output = tabulate(table_data, headers=headers, tablefmt="grid", 
                           floatfmt=".12f", 
                           numalign="right")
    with open(filename, 'w', encoding='utf-8') as f:
        f.write(table_output)
    print(f"Saved to {filename}")

if __name__ == "__main__":
    n_values = [100, 1000, 10000]
    p_values = [0.001, 0.01, 0.1, 0.25, 0.5]
    
    results = analyze_bernoulli(n_values, p_values)
    print_results(results)