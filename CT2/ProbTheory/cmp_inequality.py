import numpy as np
from scipy.stats import norm

def compute_n_chebyshev(eps, delta, var=0.5):
    """Вычисляем n из неравенства Чебышёва"""
    return int(np.ceil(var**2 / (delta * eps**2)))

def compute_n_chernoff(eps, delta):
    """Вычисляем n из неравенства типа Чернова"""
    return int(np.ceil((1 / (2 * eps**2)) * np.log(2 / delta)))

def compute_n_cpt(eps, delta, var=0.5):
    """Вычисляем n по ЦПТ"""
    Q = norm.ppf(1 - delta / 2)
    return int(np.ceil((Q * var / eps)**2))

def generate_sample(n, p):
    """Генерируе выборку из Bern(p)."""
    return np.random.binomial(1, p, size=n)

def is_successful(sample_mean, true_mean, eps):
    """Проверяем удовлевотрены ли условия"""
    return abs(sample_mean - true_mean) <= eps

def run_experiment(n, p, epsilon, num_samples=100):
    successful_count = 0
    for _ in range(num_samples):
        sample = generate_sample(n, p)
        sample_mean = np.mean(sample)
        if is_successful(sample_mean, p, epsilon):
            successful_count += 1
    success_rate = successful_count / num_samples
    return successful_count, success_rate

if __name__ == "__main__":
    eps = 0.01
    delta = 0.05
    p = 0.5 # Точка с наибольшей дисперсией для Bern(p)
    N = 100  

    n_cheb = compute_n_chebyshev(eps, delta)
    succ_cheb, rate_cheb = run_experiment(n_cheb, p, eps, N)

    n_cher = compute_n_chernoff(eps, delta)
    succ_cher, rate_cher = run_experiment(n_cher, p, eps, N)

    n_cpt = compute_n_cpt(eps, delta)
    succ_cpt, rate_cpt = run_experiment(n_cpt, p, eps, N)

    with open("sammmples.txt", "w", encoding="utf-8") as f:
        f.write("Итог:\n")
        f.write(f"Чебышёв:  n = {n_cheb}, Успешные выборки: {succ_cheb}/{N}, Доля: {rate_cheb:.2%}\n")
        f.write(f"Чернов:   n = {n_cher}, Успешные выборки: {succ_cher}/{N}, Доля: {rate_cher:.2%}\n")
        f.write(f"ЦПТ:      n = {n_cpt}, Успешные выборки: {succ_cpt}/{N}, Доля: {rate_cpt:.2%}\n")
