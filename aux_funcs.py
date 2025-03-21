import pyTsetlinMachine
import re
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from pyTsetlinMachine.tm import MultiClassTsetlinMachine

class Paramtest:
	def __init__(self, datasets, outcome, param_dict, test_size):
		#self.model = model
		self.datasets = datasets
		self.outcome = outcome
		self.param_dict = param_dict
		self.feat_names = list(datasets[0].drop(outcome, axis=1))
		self.n_feature = len(self.feat_names)
		self.test_size = test_size
		self.Xs = [x.drop(outcome, axis=1).to_numpy() for x in datasets]
		self.ys = [x[outcome].to_numpy() for x in datasets]

	def ttsplits(self):
		tt_splits = [train_test_split(x, y, test_size=self.test_size, random_state=1) for x, y in zip(self.Xs, self.ys)]
		tts = dict(zip(["X_train", "X_test", "y_train", "y_test"],  map(list, zip(*tt_splits))))
		return tts

	def TM(self, epochs = 200, Incremental = False):
		tts = self.ttsplits()
		p_combs = len(next(iter(self.param_dict.values())))
		cna_lits = self.feat_names + [x.lower() for x in self.feat_names]
		tm_pos_lits = ["X"+i for i in map(str, list(range(len(self.feat_names))))]
		tm_neg_lits = [i.lower() for i in tm_pos_lits]
		tm_lits = tm_pos_lits + tm_neg_lits
		f_translate_dict = dict(zip(tm_lits, cna_lits))
		allres = []
		for comb in range(p_combs):
			models = []
			for dat in range(len(self.datasets)):
				tmargs = {x: y[comb] for x, y in self.param_dict.items()}
				tm = MultiClassTsetlinMachine(**tmargs)
				tm.fit(tts["X_train"][dat], tts["y_train"][dat], epochs=epochs, Incremental=Incremental)
				models.append(self.tm_to_asf(tm, tm.number_of_clauses, self.n_feature, f_translate_dict))
			res = {"models": models}
			res.update(tmargs)
			respd = pd.DataFrame(res)
			allres.append(respd)
		return allres

	@staticmethod
	def clauses_from_TM(tm, nr_of_clauses,
										number_of_features):
		out=[]
		for j in range(0, nr_of_clauses, 2):
			l = []
			for k in range(number_of_features*2):
					if tm.ta_action(1, j, k) == 1:
						if k < number_of_features:
							l.append("X%d" % (k))
						else:
							l.append("x%d" % (k-number_of_features))
							out.append("*".join(l))
		return out

	def tm_to_asf(self, tm,
							nr_of_clauses,
							nr_of_features,
							translate_dict):
		fitted_clauses = Paramtest.clauses_from_TM(tm, nr_of_clauses, nr_of_features)
		suffs = [Paramtest.tm_clause_to_cna(x, translate_dict) for x in fitted_clauses]
		suffs = list(filter(lambda x: len(x) != 0, suffs))
		return "+".join(set(suffs))

	@staticmethod
	def tm_clause_to_cna(txt, translate_dict):
		for key, value in translate_dict.items():
			txt = re.sub(r"\b"+key+r"\b", value, txt)
		return txt

	def DT(self):
		p_combs = len(next(iter(self.param_dict.values())))
		tts = self.ttsplits()
		allres = []
		for comb in range(p_combs):
			models = []
			for dat in range(len(self.datasets)):
				dtargs = {x: y[comb] for x, y in self.param_dict.items()}
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
