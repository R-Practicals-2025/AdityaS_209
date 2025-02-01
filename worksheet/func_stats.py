import math
from math import floor,ceil
from operator import indexOf, invert

# central tendency formula

#Mean
df = [44,46,49,52,55,62,67,72,77,80,83,86,88,90,92,94,99,100,101,106]
def mean_df(df):
    res = math.fsum(df)/len(df)
    return res

def median(df):
    length = len(df)
    df.sort()
    median_index=int(length/2)
    if length%2==0:
        #get middle two terms
        mid_1 = floor(median_index) -1
        mid_2 = ceil(median_index)
        median_val  = (df[mid_1] + df[mid_2])/2
        return median_val
    else:
        median_val = ceil(df[median_index])
        return median_val


def mode(data):
    dict_keys = list(set(data))
    num_array = [0 for x in range(0, len(dict_keys))]
    freq_table = dict(zip(dict_keys, num_array))

    for key, value in freq_table.items():
        count = 0
        for num in data:
            if num == key:
                count +=1
        freq_table[key] = count
    max_val = [x for x,y in freq_table.items() if y == max(freq_table.values())]

    return max_val

#     count_array = []
#     return df[max_count]


#count the occurence of element
#select the element with max count



def geom_mean(df):
    #root(x1,x2,x3)
    length = len(df)

    if 0 not in df:
        df_prod = math.prod(df)
        geom_mean_val = (df_prod)**(1/length)
    else:
        geom_mean_val = 0
    return f'Geometric mean: {geom_mean_val}'


def harmonic_mean(df):
    df_inverse = []
    for i in df:
        if i != 0:
            df_inverse.append(i**(-1))
    harm_res = len(df)/math.fsum(df_inverse)
    return f'Harmonic mean: {harm_res}'


#Measure of dispersion
import matplotlib.pyplot as plt
plt_hist = plt.hist(df,bins=10, edgecolor = "black")
# plt.show()

#mean dispersion
def mean_deviation(df):
    mean_val = mean_df(df)
    total_error = [abs(x - mean_val) for x in df ]
    mean_dev = mean_df(total_error)
    return mean_dev

#Median deviation
def median_deviation(df):
    median_val = median(df)
    total_diff = [abs(i - median_val) for i in df]
    median_dev = mean_df(total_diff)
    return  median_dev


#standard deviation

def std_deviation(df):
    mean_val= mean_df(df)
    sq_sum_mean= math.fsum([abs(x - mean_val)**2 for x in df ])
    std_dev_val = sq_sum_mean / (len(df) - 1)
    return (std_dev_val)**(1/2)



def variance_df(df):
    var_value = std_deviation(df)**2
    return var_value


#Skewness
def skewness_df(df):
    mean_val = mean_df(df)
    n = len(df)
    tri_sum = math.fsum([(x - mean_val)**3 for x in df])
    sq_sum = math.fsum([(x - mean_val)**2 for x in df])
    st_dev_val = math.sqrt(sq_sum/n)
    skew_val = (n * tri_sum) / ((st_dev_val**3) * (n-1) * (n-2))
    return skew_val

#Kurtosis
def kurtosis_df(df):
    mean_val = mean_df(df)
    quad_sum = math.fsum([abs(x - mean_val)**4 for x in df])
    sq_sum = math.fsum([abs(x - mean_val) ** 2 for x in df])
    kurtosis_val = (len(df)*(quad_sum) / ((sq_sum)**2)) - 3
    return kurtosis_val
