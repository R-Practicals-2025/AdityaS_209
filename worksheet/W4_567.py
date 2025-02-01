##Create mode func

from data_W4 import *
from w4.func_stats import *

ex231  = load_data_Q4a()
ex232  = load_data_Q4b()
ex233  = load_data_Q4c()
ex234  = load_data_Q4d()
ex2311  = load_data_Q4e()

def print_stats(data):
    print("Arithmetic Mean:", mean_df(data))
    print("Geometric Mean:", geom_mean(data))
    print("Harmonic Mean:", harmonic_mean(data))
    print("Median:", median(data))
    print("Mode:", mode(data))
    print("Mean Deviation about Mean:", mean_deviation(data))
    print("Mean Deviation about Median:", median_deviation(data))
    print("Variance:", variance_df(data))
    print("Standard Deviation:", std_deviation(data))
    print("Skewness:", skewness_df(data))
    print("Kurtosis:", kurtosis_df(data))
    # print("Harmonic Mean:", harmonic_mean(data))

print('-'*25,'ex231', '-'*25)
print_stats(ex231)

print('-'*25,'ex231', '-'*25)
print_stats(ex232)

print('-'*25,'ex233', '-'*25)
print_stats(ex233)

print('-'*25,'ex234', '-'*25)
print_stats(ex234)

print('-'*25,'ex2311', '-'*25)
print_stats(ex2311)

