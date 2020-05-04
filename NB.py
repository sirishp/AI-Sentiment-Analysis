import pandas as pd
import matplotlib.pyplot as plt
%matplotlib inline
from sklearn.preprocessing import StandardScaler
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import confusion_matrix

stock_data = pd.read_csv(r'/Users/sirishpulijala/Desktop/Data_Science_Python/AMZN.csv')

df2018 = stock_data[stock_data.Year == 2018]
df2019 = stock_data[stock_data.Year == 2019]
#
df1 = df2018.groupby(
               ['Week_Number', 'label']
            ).agg(
                {
                     'Return':['mean','std']   
                }
            ).reset_index()
df1.columns = ['Week_Number', 'label','mu','sd']
X_2018 = df1[['mu', 'sd']].values
Y_2018 = df1['label'].values
scaler = StandardScaler()
scaler.fit(X_2018)
X_2018 = scaler.transform(X_2018)

df2 = df2019.groupby(
               ['Week_Number', 'label']
            ).agg(
                {
                     'Return':['mean','std']   
                }
            ).reset_index()
df2.columns = ['Week_Number', 'label','mu','sd']
df22 = df2.iloc[0:52, ] # Here I ignore the last week of year 2019.
X_2019 = df22[['mu', 'sd']].values
Y_2019 = df22['label'].values
scaler.fit(X_2019)
X_2019 = scaler.transform(X_2019)

#Implement a Gaussian naive bayesian classiﬁer and compute its accuracy for year 2019

NB_classifier = GaussianNB().fit(X_2018, Y_2018)
prediction = NB_classifier.predict(X_2019)
accuracy = NB_classifier.score(X_2019, Y_2019)
print("The accuracy of the Gaussian naive bayesian for Year 2019 is", round(accuracy,2))
print('\n')

# the confusion matrix for year 2

cf = confusion_matrix(Y_2019, prediction)
print("The confusion matrix for Year 2019 is", cf)
print('\n')

# true positive rate (sensitivity or recall) for 2019

tpr = cf[1][1] / (cf[1][1] + cf[1][0])
print("The true positive rate (sensitivity or recall) for Year 2019 is", tpr)
print('\n')

# true negative rate (speciﬁcity) for 2019

tnr = cf[0][0] / (cf[0][0] + cf[0][1])
print("The true negative rate (speciﬁcity) for Year 2019 is", tnr)


train_start_date = '2014-01-01'
train_end_date = '2015-12-31'
test_start_date = '2016-01-01'
test_end_date = '2019-12-31'
train = df.loc[train_start_date : train_end_date]
test = df.loc[test_start_date:test_end_date]

y_train = pd.DataFrame(train['prices'])
y_test = pd.DataFrame(test['prices'])

gnb = GaussianNB()
gnb.fit(numpy_df_train,y_train)
target_pred = gnb.predict(numpy_df_test)
accuracy = gnb.score(numpy_df_train,y_train)*100

idx = pd.date_range(test_start_date, test_end_date)
predictions_df = pd.DataFrame(data=target_pred[0:]+5500, index = idx, columns=['prices'])
predictions_df['Week_Number'] = predictions_df.index.strftime('%U')
predictions_df['actual'] = y_test

ax=predictions_df.plot()

df_0 = predictions_df[['Week_Number', 'prices', 'actual']]
df_0.index = range(len(predictions_df))
df_grouped0 = df_0.groupby(['Week_Number'])['prices','actual'].agg([np.mean])
df_grouped0.reset_index(['Week_Number'], inplace=True)

ax = df_grouped0.rename(columns={"prices": "predicted_price"}).plot(title='Naive bayes predicted prices 8-2 years after aligning')
ax.set_xlabel("Dates")
ax.set_ylabel("Stock Prices")
#fig = y_test.rename(columns={"prices": "actual_price"}).plot(ax = ax).get_figure()
#fig.savefig("graphs/naive bayes with aligning.png")