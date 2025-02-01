import matplotlib.pyplot as plt
import numpy as np

# ax**2 + b*x + c

a = -6
b=-17
c=-102

# y = a*x**2 + b*x + c

x = np.random.randint(-100,200, 100)

print(x)

y = a*x**2 + b*x + c
print(y)

plt.scatter(x,y)
# plt.lineplot(x,y)
plt.show()