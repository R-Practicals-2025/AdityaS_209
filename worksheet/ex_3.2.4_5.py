import matplotlib.pyplot as plt
import pandas as pd

data = {'Expenditure': ['Grocery', 'Rent', 'EduChild', 'Medicine', 'Fuel', 'Entertainment', 'Misc'], 'bill': [8000,5000,5000,2000,2000,1000,1000]}
df = pd.DataFrame.from_dict(data)
print(df.info())
plt.bar(df['Expenditure'], df['bill'], color='orange')
plt.title('Bar Graph')
plt.show()

#5
data = {'Item':['bread','fruit bread', 'cakes and pastries', 'biscuits', 'other'] , 'sales':[320,80,160, 120,40]}
df = pd.DataFrame.from_dict(data)
plt.pie(df['sales'], labels=df['Item'])
plt.title('Pie Chart')
plt.show()