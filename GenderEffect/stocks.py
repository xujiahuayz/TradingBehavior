import pandas as pd
import numpy as np

aa = pd.DataFrame(columns=['gender', 'ISIN', 'future_return', 'past_return', 'month'])

how_many_women_bought = aa.groupby(['ISIN', 'month']).mean() # groupy does summing in each column
# end up with a table with row names = (ISIN, month) combinations and columns being mean gender,
# and the realized (future and past) returns in that month
# mean is important because 'future_return', 'past_return' will just
how_many_women_bought_month = aa.groupby('month').mean()

fraction_of_women_who_bought = how_many_women_bought['gender']
for ind in how_many_women_bought.index:
    month = ind[1]  # index[0] = ISIN, index[1] = month
    fraction_of_women_who_bought.iloc[ind] = fraction_of_women_who_bought.iloc[ind] / how_many_women_bought_month.loc[month, 'gender']


unique_isins = np.unique(aa.ISIN)
unique_months = np.unique(aa.month)

weights = pd.DataFrame(index=unique_months, columns=unique_isins)
future_returns = pd.DataFrame(index=unique_months, columns=unique_isins)
past_returns = pd.DataFrame(index=unique_months, columns=unique_isins)

for month in unique_months:
    for isin in unique_isins:
        weights.loc[month, isin] = fraction_of_women_who_bought.loc[(isin, month), 'gender'] #only this fow weights
        future_returns.loc[month, isin] = how_many_women_bought.loc[(isin, month), 'future_return']
        past_returns.loc[month, isin] = how_many_women_bought.loc[(isin, month), 'past_return']

# now you have the tables so you can build your portfolios
# simple weighting:
demeaned_fraction = weights - weights.mean(1) # demean the cross-section (so you take the mean along the horizontal axis) (substar t from each column of the data table)
simple_portfolio_returns = (future_returns * demeaned_fraction).mean(1)

quantile20 = weights.quantile(0.2, axis=1) # has only one column
quantile80 = weights.quantile(0.8, axis=1) # has only one column

# now we need to create a matrix with identical columns, all equal to quantile20. I use numpy broadcasting.
# In R, there are similar broadcasting tricks.
quantile20 = weights * 0 + quantile20.values.reshape(-1, 1)
quantile80 = weights * 0 + quantile80.values.reshape(-1, 1)

weights_1 = (weights > quantile80) - (weights < quantile20) # given 1 for > 80% and -1 for < 20%
good_portfolio_returns = (future_returns * weights_1).mean(1)

# now we need to do a double sort

weights_2 = (weights > quantile80) * (past_returns > 0) - (weights < quantile20) * (past_returns < 0) # given 1 for > 80% and -1 for < 20%
super_good_portfolio_returns = (future_returns * weights_2).mean(1)



