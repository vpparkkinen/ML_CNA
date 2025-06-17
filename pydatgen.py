# temporary hack for generating data w/ outcome
# column as boolean function of predictors/exo vars.
# to do: wrap into function(s) and add to
# aux_funcs.py

import string
import numpy as np
import pandas as pd

outcome_name = "A"
N = 10 # sample size
obs_exo_varnum = 4 # n. of observed exogenous vars

# exo variable names from LETTERS
potential_varnames = [x for x in string.ascii_uppercase]
potential_exo_varnames = [x for x in potential_varnames if x != outcome_name]
obs_exo_varnames = potential_exo_varnames[:obs_exo_varnum]
omitted_varname = "U" # name of the "omitted" variable

# simulate values of the omitted var
omitted = np.random.binomial(1, 0.5, N)

# biases for simulating distributions
# of the observed vars
bias_one = 0.2
bias_zero = 0.8

# create a distribution over exogenous vars with
# clusters based on the value of the omitted var
exos = np.zeros((N, obs_exo_varnum)).astype(int)
for idx in range(obs_exo_varnum):
  exos[:, idx] = np.where(omitted == 1,
                          np.random.binomial(1, bias_one, N),
                          np.random.binomial(0, bias_zero, N)
                          )

# all exo vars, including the "ommitted" one, in one array
all_exos = np.column_stack((omitted, exos))

# keep track of variable names
exo_names = [omitted_varname] + obs_exo_varnames


def assign_outcome(varnames, data, expr):
  """based on `expr`, assigns an outcome
  value for each row of `data`. For the time being,
  `expr` needs to be given as pythonesque boolean expression.
  TO DO: translate from cna syntax automatically."""
  N = data.shape[0]
  outvals = np.zeros((N,1)).astype(int)
  for i in range(N):
    config = dict(zip(varnames, data[i,:]))
    outvals[i] = eval(expr, {}, config)
  return outvals

## now create a full data set
bool_expression = "B or (not C and D)" # condition used to
                                       # generate the outcome # # values
outcome_col = assign_outcome(exo_names, all_exos, bool_expression)
alldat = np.column_stack((all_exos, outcome_col))

## optional, make into pd data frame so that when you forget,
## you can check
## which column was the outcome, and which is to be dropped
## as the omitted variable

allcolnames = exo_names + [outcome_name]
alldat_pd = pd.DataFrame(alldat, columns=allcolnames)

## now look for clusters in columns other than
## the omitted var and outcome
