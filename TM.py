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

outcome = "A"

# dictionary of parameter names and values to test
tm_params = {"number_of_clauses": [10, 15],  "T": [10, 15], "s": [4,8]}

# Initiate a test with the data sets, outcome, dictionary
# of parameters/values,
# and train/test split size (in test_size)
tmtest = Paramtest(dats, outcome=outcome, param_dict=tm_params, test_size=0.2)

# Run a test with Tsetlin machine.
# Every data set is analysed with the settings fetched from
# the parameter dictionary, and the models are collected.
# This returns a list of
# pd data frames, one per each parameter setting.
# 'Models' column contains a model (which may be empty)
# for each data set. Rest of the columns contain
# the parameter values set by the user. Any params that
# were not set by the user (i.e. defaults were used)
# are not included.
res = tmtest.TM()

# Save the results in whichever way is convenient.
