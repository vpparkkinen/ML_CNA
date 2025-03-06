import pandas as pd
import numpy as np
import os
import re
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
import dt_aux_funcs

nfil = len([1 for x in list(os.scandir("data")) if x.is_file()])

# load data sets
dats = []
for f in range(1, nfil+1):
  dats.append(pd.read_csv("data/dat"+str(f)+".csv", sep = ";"))

# increase N
# dats = [pd.concat([i] * 100, ignore_index=True) for i in dats]

feature_names = list(dats[0])[1:]
outcome = list(dats[0])[0]

# store outcomes ('y') and exo vars/predictors ('X') to separate lists, .value for getting numpy arrays to be used in training
Xs = [x.iloc[:, 1:].values for x in dats]
ys = [x.iloc[:, 0].values for x in dats]

# 80%/20% train/test splits
# -> list of lists where element 0 is X train,
# 1 is X test, 2 is y train, 3 y test
tt_splits = [train_test_split(x, y, test_size=0.2, random_state=1) for x, y in zip(Xs, ys)]

# dict to store the t/t splits in a less awful way
tts = dict(zip(["X_train", "X_test", "y_train", "y_test"],  map(list, zip(*tt_splits))))

# Note: We want to think of the trees as putative
# explanations of the
# outcome and check their correctness w.r.t the data-generating
# causal structure. So
# we don't care about the model predictions
# -> an incorrect explanation might
# predict false outcomes, but also an incomplete
# _correct_ explanation would, and a causally incorrect
# model may predict better than an incomplete correct model.
# So the test sets are not needed, but there
# they are if we want to do something with them later.


fitted = []
for i in range(len(dats)):
    print(i)
    dt_cl = DecisionTreeClassifier(criterion='gini', max_depth=4, max_features=29, random_state=1,
                                   min_samples_leaf=20,
                                   min_weight_fraction_leaf=0.1)
    fitted.append(dt_cl.fit(
        tts["X_train"][i], tts["y_train"][i]))


mods = [dt_to_cna(x, feature_names, outcome) for x in fitted]

# write mods to file
np.savetxt("dt_results.txt", mods, delimiter="\n", fmt="%s")
