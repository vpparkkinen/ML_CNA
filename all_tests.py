import pandas as pd
import numpy as np
import os
import re
import time
from sklearn.model_selection import train_test_split
import itertools
from sklearn.tree import DecisionTreeClassifier
#from pyTsetlinMachine.tm import MultiClassTsetlinMachine
from tmu.models.classification.vanilla_classifier import TMClassifier
from aux_funcs import *
#os.getcwd()
nfil = len([1 for x in list(os.scandir("data")) if x.is_file()])

# load data sets
dats = []
for f in range(1, nfil+1):
  dats.append(pd.read_csv("data/dat"+str(f)+".csv", sep = ",", dtype=np.uint32))

dats = dats[0:9]
# increase N
# dats = [pd.concat([i] * 100, ignore_index=True) for i in dats]

outcome = "A"

# ##>>>>>>>>> Decision tree

# dt_params = {"max_depth": [5,14], "max_features": [14], "max_leaf_nodes": [15, 30], "criterion": ["gini", "entropy"]}

# dt_params = expand_grid(dt_params)

# DT = Paramtest(dats, outcome=outcome, param_dict=dt_params,
#                test_size=0.2)

# dt_results = DT.DT()

# dtall = pd.concat(dt_results)
# filename = "dtres"+time.strftime("%d%m%Y-%Hh%Mm")+".csv"
# dtall.to_csv(filename)

# ##<<<<<<<<< Decision tree

##>>>>>>>>> Tsetlin Machine

tm_params = {"number_of_clauses": [10,50,100], "T": [5,10], "s": [5], "platform": ["CPU"]}
tm_params = expand_grid(tm_params)
#dats = dats[2:5]
TM = Paramtest(dats, outcome=outcome, param_dict=tm_params,
               test_size=0.1)



tm_results = TM.TM()
# tm2 = TMClassifier(100, 10, 5)
# TM.ttsplits()
# tm2.fit(TM.ttsplits()["X_train"][2], TM.ttsplits()["y_train"][2])

# tm2c=Paramtest.poscfTMnot(tm2, 100, 14)
# consistency(TM.ys[0], ['X1', 'X5', 'X11'], TM.Xs[0], 'X1 and X5 and not X11')
tmall = pd.concat(tm_results)
filename = "tmres"+time.strftime("%d%m%Y-%Hh%Mm")+".csv"
tmall.to_csv(filename)

##<<<<<<<<< Tsetlin Machine
