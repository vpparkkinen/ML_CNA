import pandas as pd
import numpy as np
import os
import re
import time
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from pyTsetlinMachine.tm import MultiClassTsetlinMachine
from aux_funcs import *

nfil = len([1 for x in list(os.scandir("data")) if x.is_file()])

# load data sets
dats = []
for f in range(1, nfil+1):
  dats.append(pd.read_csv("data/dat"+str(f)+".csv", sep = ";"))

# increase N
# dats = [pd.concat([i] * 100, ignore_index=True) for i in dats]

outcome = "A"

##>>>>>>>>> Decision tree

dt_params = {"max_depth": [3,5], "max_features": [28,28], "max_leaf_nodes": [8, 15], "criterion": ["gini", "entropy"]}

DT = Paramtest(dats, outcome=outcome, param_dict=dt_params,
               test_size=0.2)

dt_results = DT.DT()

dtall = pd.concat(dt_results)
filename = "dtres"+time.strftime("%d%m%Y-%Hh%Mm")+".csv"
dtall.to_csv(filename)

##<<<<<<<<< Decision tree

##>>>>>>>>> Tsetlin Machine

tm_params = {"number_of_clauses": [15,20], "T": [15,5], "s": [1,10]}

TM = Paramtest(dats, outcome=outcome, param_dict=tm_params,
               test_size=0.2)

tm_results = TM.TM()

tmall = pd.concat(tm_results)
filename = "tmres"+time.strftime("%d%m%Y-%Hh%Mm")+".csv"
tmall.to_csv(filename)

##<<<<<<<<< Tsetlin Machine
