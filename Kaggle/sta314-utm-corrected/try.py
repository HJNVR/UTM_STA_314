import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split

df = pd.read_csv('/Users/Starkjing/STA314/Kaggle/sta314-utm-corrected/trainingdata2.csv')
df.head()
df.describe()
df.dtypes
