import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from pyTsetlinMachine.tm import MultiClassTsetlinMachine

nfil = len([1 for x in list(os.scandir("data")) if x.is_file()])

# load data sets
dats = []
for f in range(1, nfil+1):
  dats.append(pd.read_csv("data/dat"+str(f)+".csv", sep = ";"))

# increase N
dats = [pd.concat([i] * 100, ignore_index=True) for i in dats]

feature_names = list(dats[0])[1:]
outcome = list(dats[0])[0]

# store outcomes ('y') and exo vars/predictors ('X') to separate lists, .value for getting numpy arrays to be used in training
Xs = [x.iloc[:, 1:].values for x in dats]
ys = [x.iloc[:, 0].values for x in dats]

# 80%/20% train/test splits
# -> list of lists where element 0 is X train,
# 1 is X test, 2 is y train, 3 y test
tt_splits = [train_test_split(x, y, test_size=0.2, random_state=1) for x, y in zip(Xs, ys)]

# dict to organize the t/t splits in a less awful way
tts = dict(zip(["X_train", "X_test", "y_train", "y_test"],  map(list, zip(*tt_splits))))

n_clauses = 10

tm = MultiClassTsetlinMachine(n_clauses, 15, 3.0, boost_true_positive_feedback=1)

tm.fit(tts["X_train"][0], tts["y_train"][0])

def clauses_from_TM(tm, nr_of_clauses,
										number_of_features):
	out=[]
	for j in range(0, nr_of_clauses, 2):
		l = []
		for k in range(number_of_features*2):
			if tm.ta_action(1, j, k) == 1:
				if k < number_of_features:
					l.append("x%d" % (k))
				else:
					l.append("-x%d" % (k-number_of_features))
			out.append("*".join(l))
		return out
