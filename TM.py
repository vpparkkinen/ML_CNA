import numpy as np
import pandas as pd
import re
from sklearn.model_selection import train_test_split
from pyTsetlinMachine.tm import MultiClassTsetlinMachine
from tm_aux_funcs import *

nfil = len([1 for x in list(os.scandir("data")) if x.is_file()])

# load data sets
dats = []
for f in range(1, nfil+1):
  dats.append(pd.read_csv("data/dat"+str(f)+".csv", sep = ";"))

# increase N
# dats = [pd.concat([i] * 100, ignore_index=True) for i in dats]

feature_names = list(dats[0])[1:]
outcome = list(dats[0])[0]
n_features = len(feature_names)

# cna literals for translating between TM and CNA model syntax
cna_lits = list(dats[0])[1:] + [x.lower() for x in list(dats[0])[1:]]

# tm literals
# NOTE: this is an arbitrary choice, based on the
# clauses_from_TM() -function's output syntax for the clauses,
# i.e. a (positive) literal starts with (capital ) "X" and its
# negation with (lower case) "x", followed by numbers.
tm_pos_lits = ["X"+i for i in map(str, list(range(len(feature_names))))]
tm_neg_lits = [i.lower() for i in tm_pos_lits]
tm_lits = tm_pos_lits + tm_neg_lits

# translation dict between tm clauses and cna asfs
f_translate_dict = dict(zip(tm_lits, cna_lits))

# store outcomes ('y') and exo vars/predictors ('X') to separate lists, .values for getting numpy arrays to be used in training
Xs = [x.iloc[:, 1:].values for x in dats]
ys = [x.iloc[:, 0].values for x in dats]

# 80%/20% train/test splits
# -> list of lists where element 0 is X train,
# 1 is X test, 2 is y train, 3 y test
tt_splits = [train_test_split(x, y, test_size=0.2, random_state=1) for x, y in zip(Xs, ys)]

# dict to organize the t/t splits in a less awful way
tts = dict(zip(["X_train", "X_test", "y_train", "y_test"],  map(list, zip(*tt_splits))))

n_clauses = 20 # how many clauses to use in TM

models = []
for i in range(len(dats)):
	tm = MultiClassTsetlinMachine(n_clauses, 29, 3.0, boost_true_positive_feedback=1)
	tm.fit(tts["X_train"][i], tts["y_train"][i])
	models.append(tm_to_asf(tm,
												 n_clauses,
												 n_features,
												 f_translate_dict))

models

# write mods to file
np.savetxt("tm_results.txt", models, delimiter="\n", fmt="%s")
