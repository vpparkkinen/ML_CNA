import pyTsetlinMachine
import re
import numpy as np
import pandas as pd
import itertools
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
#from pyTsetlinMachine.tm import MultiClassTsetlinMachine
from tmu.models.classification.vanilla_classifier import TMClassifier

def expand_grid(data_dict):
    rows = itertools.product(*data_dict.values())
    return pd.DataFrame.from_records(rows, columns=data_dict.keys())

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

def check_consistency(out, varnames, data, expr):
	predicted = assign_outcome(varnames=varnames, data=data, expr=expr)
	out = sum(out.flatten() == predicted.flatten()) / data.shape[0]
	return out

def consistency(out, varnames, data, expr):
	predicted = assign_outcome(varnames=varnames, data=data, expr=expr).flatten()
	idx = predicted == 1
	ones = out[idx]
	consi = (sum(out[idx] == predicted[idx])) / len(predicted[idx])
	return consi

def selectCases(condition: str,
								condtype: str,
								feat_names=None,
								X=None,
								y=None):
	condT = None
	if X is None or y is None:
		varnames = condition.split()
		boolops = ["and", "or", "not"]
		feat_names = [x for x in varnames if x not in boolops]
		vl = len(feat_names)
		X = np.array(list(itertools.product([0,1], repeat=vl))).astype(int)
		y = assign_outcome(feat_names, X, condition).flatten()
		condT = y.astype(bool)
		#return("not implemented yet")
	if condT is None:
		condT = assign_outcome(feat_names, X, condition).flatten().astype(bool)

	c_is_y = condT == y
	if condtype == "suff":
		suffmask = c_is_y + condT
		Xout = X[suffmask]
		yout = y[suffmask]
	elif condtype == "suffnec":
		Xout = X[c_is_y]
		yout = y[c_is_y]
	else:
		print('no valid condtype (`suff` or `suffnec`) given')
		return(None)
	outdict = {"X": Xout, "y": yout}
	return outdict


class Paramtest:
	def __init__(self, datasets, outcome, param_dict, test_size):
		#self.model = model
		self.datasets = datasets
		self.outcome = outcome
		self.param_dict = param_dict
		self.feat_names = list(datasets[0].drop(outcome, axis=1))
		self.n_feature = len(self.feat_names)
		self.test_size = test_size
		# self.Xs = [x.drop(outcome, axis=1).to_numpy() for x in datasets]
		self.Xs = [x.drop(outcome, axis=1).to_numpy(dtype=np.uint32) for x in datasets]
		#self.ys = [x[outcome].to_numpy() for x in datasets]
		self.ys = [x[outcome].to_numpy(dtype=np.uint32) for x in datasets]

	def ttsplits(self):
		tt_splits = [train_test_split(x, y, test_size=self.test_size, random_state=1) for x, y in zip(self.Xs, self.ys)]
		tts = dict(zip(["X_train", "X_test", "y_train", "y_test"],  map(list, zip(*tt_splits))))
		return tts

	def TM(self, epochs = 200, incremental = False):
		tts = self.ttsplits()
		#p_combs = len(next(iter(self.param_dict.values())))
		p_combs = self.param_dict.shape[0]
		cna_lits = self.feat_names + [x.lower() for x in self.feat_names]
		tm_pos_lits = ["X"+i for i in map(str, list(range(len(self.feat_names))))]
		#tm_neg_lits = [i.lower() for i in tm_pos_lits]
		tm_neg_lits = ["not " + i for i in tm_pos_lits]

		tm_lits = tm_pos_lits + tm_neg_lits
		f_translate_dict = dict(zip(tm_lits, cna_lits))
		allres = []
		for comb in range(p_combs):
			models = []
			for dat in range(len(self.datasets)):
				# tmargs = {x: y[comb] for x, y in self.param_dict.items()}
				tmargs = self.param_dict.iloc[comb,].to_dict()
				print(dat)
				print(tmargs)
				#tm = MultiClassTsetlinMachine(**tmargs)
				tm = TMClassifier(**tmargs)
				#tm.fit(tts["X_train"][dat], tts["y_train"][dat], epochs=epochs, incremental=incremental)
				tm.fit(tts["X_train"][dat], tts["y_train"][dat], epochs=epochs)
				models.append(self.tm_to_asf(tm, tm.number_of_clauses, self.n_feature, f_translate_dict))
			res = {"models": models}
			res.update(tmargs)
			respd = pd.DataFrame(res)
			allres.append(respd)
		return allres

	# @staticmethod
	# def clauses_from_TM(tm, nr_of_clauses,
	# 									number_of_features):
	# 	out=[]
	# 	for j in range(0, nr_of_clauses, 2):
	# 		l = []
	# 		for k in range(number_of_features*2):
	# 				if tm.ta_action(1, j, k) == 1:
	# 					if k < number_of_features:
	# 						l.append("X%d" % (k))
	# 					else:
	# 						l.append("x%d" % (k-number_of_features))
	# 						out.append("*".join(l))
	# 	return out

	@staticmethod
	def clauses_from_TM(tm, nr_of_clauses, number_of_features):
		clauses = []
		for j in range(nr_of_clauses // 2):
			l = []
			for k in range(number_of_features * 2):
				if tm.get_ta_action(j, k, the_class=1, polarity=0):
					if k < number_of_features:
						l.append(f"X{k}")
					else:
						l.append(f"x{k - number_of_features}")
			clauses.append("*".join(l))
		return clauses

	# @staticmethod
	# def clauses_from_TM(tm, nr_of_clauses, number_of_features):
	# 	clauses = []
	# 	for j in range(nr_of_clauses // 2):
	# 		l = []
	# 		for k in range(number_of_features * 2):
	# 			if tm.get_ta_action(j, k, the_class=1, polarity=0):
	# 				if k < number_of_features:
	# 					l.append(f"X{k}")
	# 				else:
	# 					l.append(f"x{k - number_of_features}")
	# 		clauses.append("*".join(l))
	# 	return clauses

	# @staticmethod
	# def poscfTM_as_not(tm, nr_of_clauses, number_of_features):
	# 	clauses = []
	# 	for j in range(nr_of_clauses // 2):
	# 		l = []
	# 		for k in range(number_of_features * 2):
	# 			if tm.get_ta_action(j, k, the_class=1, polarity=0):
	# 				if k < number_of_features:
	# 					l.append(f"X{k}")
	# 				else:
	# 					l.append(f"not X{k - number_of_features}")
	# 		clauses.append(" and ".join(l))
	# 	return clauses

	# @staticmethod
	# def negcfTM_as_not(tm, nr_of_clauses, number_of_features):
	# 	clauses = []
	# 	for j in range(nr_of_clauses // 2):
	# 		l = []
	# 		for k in range(number_of_features * 2):
	# 			if tm.get_ta_action(j, k, the_class=0, polarity=0):
	# 				if k < number_of_features:
	# 					l.append(f"X{k}")
	# 				else:
	# 					l.append(f"not X{k - number_of_features}")
	# 		clauses.append(" and ".join(l))
	# 	return clauses

	@staticmethod
	def poscfTMnot(tm, nr_of_clauses, number_of_features):
		clauses = []
		for j in range(nr_of_clauses // 2):
			l = []
			for k in range(number_of_features * 2):
				if tm.get_ta_action(j, k, the_class=1, polarity=0):
					if k < number_of_features:
						l.append(f"X{k}")
					else:
						l.append(f"not X{k - number_of_features}")
			clauses.append(" and ".join(l))
		return clauses

		# clauses = []
    # for j in range(args.number_of_clauses // 2):
    #     l = []
    #     for k in range(args.number_of_features * 2):
    #         if tm.get_ta_action(j, k, the_class=1, polarity=0):
    #             if k < args.number_of_features:
    #                 l.append(f"X{k}")
    #             else:
    #                 l.append(f"x{k - args.number_of_features}")
    #     print(" ∧ ".join(l))
    #     clauses.append(" ∧ ".join(l))
    # d_clauses[f"Positive clauses for outcome={1}"] = clauses

	def tm_to_asf(self, tm,
							nr_of_clauses,
							nr_of_features,
							translate_dict):
		# fitted_clauses = Paramtest.clauses_from_TM(tm, nr_of_clauses, nr_of_features)
		print(translate_dict)
		fitted_clauses = Paramtest.poscfTMnot(tm, nr_of_clauses, nr_of_features)
		print(fitted_clauses)
		suffs = [Paramtest.tm_clause_to_cna(x, translate_dict) for x in fitted_clauses]
		suffs = list(filter(lambda x: len(x) != 0, suffs))
		suffs = [re.sub(r'not\s+([A-Z]+)', lambda match: match.group(1).lower(), x) for x in suffs]
		suffs = [re.sub(" and ", "*", x) for x in suffs]
		return "+".join(set(suffs))

	@staticmethod
	def tm_clause_to_cna(txt, translate_dict):
		for key, value in translate_dict.items():
			txt = re.sub(r"\b"+key+r"\b", value, txt)
		return txt



	def DT(self, rf_select = True):
		#p_combs = len(next(iter(self.param_dict.values())))
		p_combs = self.param_dict.shape[0]
		tts = self.ttsplits()
		allres = []
		for comb in range(p_combs):
			models = []
			for dat in range(len(self.datasets)):
				dtargs = {x: y[comb] for x, y in self.param_dict.items()}
				print(dat)
				print(dtargs)
				dt = DecisionTreeClassifier(**dtargs)
				mod = dt.fit(tts["X_train"][dat], tts["y_train"][dat])
				models.append(Paramtest.dt_to_cna(mod, self.feat_names, self.outcome))
			res = {"models": models}
			res.update(dtargs)
			respd = pd.DataFrame(res)
			allres.append(respd)
		return allres

	@staticmethod
	def get_decision_paths(tree, feature_names):
		left = tree.tree_.children_left
		right = tree.tree_.children_right
		threshold = tree.tree_.threshold
		features = [feature_names[i] if i != -2 else "Leaf" for i in tree.tree_.feature]
		value = tree.tree_.value
		paths_by_class = {}

		def recurse(node, path):
			if left[node] == -1 and right[node] == -1:  # Leaf node
				predicted_class = np.argmax(value[node])
				if predicted_class not in paths_by_class:
					paths_by_class[predicted_class] = []
				paths_by_class[predicted_class].append("*".join(path))
			else:
				if left[node] != -1:
					recurse(left[node], path + [f"{features[node]} <= {threshold[node]:.2f}"])
				if right[node] != -1:
					recurse(right[node], path + [f"{features[node]} > {threshold[node]:.2f}"])
		recurse(0, [])
		if 1 not in paths_by_class:
			return [""]
		else:
			return paths_by_class[1]


	@staticmethod
	def eq_to_lits(input_string):
		pattern_leq = r"([A-Za-z0-9]*)\s*<=\s*0\.50"
		pattern_gt = r"([A-Za-z0-9]*)\s*>\s*0\.50"
		output_string = re.sub(pattern_leq, lambda m: m.group(1).lower(), input_string)
		output_string = re.sub(pattern_gt, lambda m: m.group(1).upper(), output_string)
		return output_string

	@staticmethod
	def dt_to_cna(dt, feature_names, outcome, incl_out = False):
		paths = Paramtest.get_decision_paths(dt, feature_names=feature_names)
		suffs = [Paramtest.eq_to_lits(x) for x in paths]
		if incl_out:
			return "+".join(suffs)+"<->"+outcome
		else:
			return "+".join(suffs)
