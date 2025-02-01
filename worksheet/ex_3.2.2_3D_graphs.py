import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(-4,4, 50)
y = np.linspace(-4,4, 50)

def plots_3d(x,y):
    fig = plt.figure(figsize=(30, 10))
    X, Y = np.meshgrid(x, y)

    z = X**2 + Y**2
    ax = fig.add_subplot(131,projection='3d')
    fig1 = ax.plot_surface(X,Y,z, cmap='viridis')
    plt.title("Z = X**2 + Y**2")
    fig.colorbar(fig1, aspect = 3 )
        # plt.show()

    z = 1/(X**2 +  Y**2)
    ax = fig.add_subplot(132, projection='3d')
    fig2 = ax.plot_surface(X,Y,z, cmap='viridis')
    plt.title("z = 1/(x**2 + y**2)")
    fig.colorbar(fig2, aspect = 3 )

    # plt.show()

 # ##add trigno/ arthematic functions in meshgrid
    Z = np.cos(X) + np.cos(Y)
    ax = fig.add_subplot(133, projection='3d')
    fig3 = ax.plot_surface(X,Y,Z, cmap='viridis')
    plt.title("z = cos(x) + cos(y)")
    fig.colorbar(fig3, aspect = 3 )

    plt.tight_layout()
    plt.show()

def main():
    plots_3d(x,y)


if __name__=="__main__":
    main()
