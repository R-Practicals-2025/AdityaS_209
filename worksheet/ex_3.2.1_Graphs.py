import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np


#1
# y = np.random.rand(1,200, 50)
x = np.random.randint(-50, 50,20)

def q_1(x):
    y = x ** 2 + 1
    sns.set(style="whitegrid")
    sns.scatterplot(x=x,y=y)
    plt.title(f"y = x ** 2 + 1")
    plt.show()

def q_2(x):
    y = (4 - (x-1)**2)**1/2
    sns.set(style="whitegrid")
    sns.scatterplot(x=x,y=y)
    plt.title(f"y = (4 - (x-1)**2)**1/2")
    plt.show()


def main():
    q_1(x)
    q_2(x)

if __name__=="__main__":
    main()
