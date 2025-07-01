from scipy import stats
import numpy as np
from time import time

class CustomDistribution(stats.rv_continuous):
    def _pdf(self, x):
        return np.exp(-np.abs(x)) / (2 * (1 - np.exp(-1))) * (np.abs(x) <= 1)
    
    def _cdf(self, x):
        result = np.zeros_like(x, dtype=float)
        
        cond_first = x < -1
        result[cond_first] = 0
        
        cond_second = (x >= -1) & (x <= 0)
        result[cond_second] = (np.exp(x[cond_second])) / (2 * (1 - np.exp(-1)))
    
        cond_third = (x > 0) & (x <= 1)
        result[cond_third] = -(np.exp(-x[cond_third])) / (2 * (1 - np.exp(-1)))
        
        cond_fourth = x > 1
        result[cond_fourth] = 1
        
        return result
    
def inverse_cdf_sampling(n):
    """делаем семплс по обратной функции распределения"""
    u = np.random.uniform(0, 1, size=n)
    result = np.zeros_like(u)
    
    cond1 = u <= 0.5
    result[cond1] = np.log(2 * u[cond1] * (1 - np.exp(-1)) + np.exp(-1))

    cond2 = u > 0.5
    result[cond2] = -np.log(1 - 2 * (u[cond2] - 0.5) * (1 - np.exp(-1)))
        
    return result

def rejecting_sampling(n):
    """делаем семплс rejecting sampling"""
    samples = []
    pdf_bound = 1 / (1 - np.exp(-1))
    
    while len(samples) < n:
        batch_size = min(2 * (n - len(samples)), n)
        x_proposals = np.random.uniform(0, 1, size=batch_size)
        pdf_values = np.exp(-np.abs(x_proposals)) / (2 * (1 - np.exp(-1)))
        u = np.random.uniform(0, pdf_bound, size=batch_size)
        accepted = x_proposals[u <= pdf_values]

        samples.extend(accepted[:n - len(samples)])
    
    return np.array(samples)


custom_dist = CustomDistribution()
def method1_test(n_values):
    """проверяем ООП"""
    results = {}
    
    for n in n_values:
        start_time = time()
        samples = custom_dist.rvs(size=n)
        elapsed = time() - start_time
        
        results[n] = {'samples': samples,
                      'time': elapsed}
    
    return results

def method2_test(n_values):
    """проверяем обратную функцию распределения"""
    results = {}
    
    for n in n_values:
        start_time = time()
        samples = inverse_cdf_sampling(n)
        elapsed = time() - start_time
        
        results[n] = {'samples': samples,
                      'time': elapsed}
    
    return results

def method3_test(n_values):
    """проверяем rejecting_sampling"""
    results = {}
    
    for n in n_values:
        start_time = time()
        samples = rejecting_sampling(n)
        elapsed = time() - start_time
        
        results[n] = {'samples': samples, 
                      'time': elapsed}
    
    return results

def eherements():
    """эксперементы"""
    n_values_method1 = [10, 500, 1000]
    n_values_method23 = [10, 500, 1000, 5000, 10000, 100000]
    
    results_method1 = method1_test(n_values_method1)
    results_method2 = method2_test(n_values_method23)
    results_method3 = method3_test(n_values_method23)
    
    makefile(results_method1, results_method2, results_method3, 
                             n_values_method1, n_values_method23)

def makefile(results_method1, results_method2, results_method3, 
                             n_values_method1, n_values_method23, filename="teorver_stat.txt"):
    with open(filename, "w") as f:
        f.write("{:<10} {:<15} {:<12}\n".format(
            "Method", "Sample Size", "Time (s)"))
        f.write("-" * 40 + "\n")
        
        for n in n_values_method1:
            f.write("{:<10} {:<15d} {:<12.6f}\n".format(
                "OOP", n, results_method1[n]['time']
            ))
        f.write("-" * 40 + "\n")
        
        for n in n_values_method23:
            for method_name, results in [("Inverse CDF", results_method2), 
                                         ("Rejection", results_method3)]:
                f.write("{:<10} {:<15d} {:<12.6f}\n".format(
                    method_name, n, results[n]['time']
                ))
            f.write("-" * 40 + "\n")
            
if __name__ == "__main__":
    eherements()