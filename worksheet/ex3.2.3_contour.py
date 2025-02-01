import matplotlib.pyplot as plt
import numpy as np

num = np.linspace(-50,50,100)

y = x = num
X,Y = np.meshgrid(x,y)

def plt_contour(X,Y):
    fig = plt.figure(figsize=(20,10))
    z = X**2 + Y**2
    ax1 = fig.add_subplot(121)
    plt.xlabel('x-axis')
    plt.ylabel('y-axis')
    plt.title('z = X^2 + Y^2')
    ax1.contour(z)

    z  = 2*X**2 + y
    ax2 = fig.add_subplot(122)
    ax2.contour(z)
    plt.xlabel('x-axis')
    plt.ylabel('y-axis')
    plt.title('z = 2X^2 + Y^2')
    plt.show()

plt_contour(X,Y)