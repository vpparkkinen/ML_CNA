import pyTsetlinMachine
import re

class Paramtest:
	def __init__(self, datasets, outcome, param_dict, test_size):
		#self.model = model
		self.datasets = datasets
		self.outcome = outcome
		self.param_dict = param_dict
		self.feat_names = list(datasets[0].drop(outcome, axis=1))
		self.n_feature = len(self.feat_names)
		self.test_size = test_size
		self.Xs = [x.drop(outcome, axis=1) for x in datasets]
		self.ys = [x.drop(self.feat_names, axis=1) for x in datasets]

	def ttsplits(self):
		tt_splits = [train_test_split(x, y, test_size=0.2, random_state=1) for x, y in zip(self.Xs, self.ys)]
		tts = dict(zip(["X_train", "X_test", "y_train", "y_test"],  map(list, zip(*tt_splits))))
		return tts

	def TM(self):
		tts = self.ttsplits()
		cna_lits = list(self.feat_names + [x.lower() for x in self.feat_names])
		tm_pos_lits = ["X"+i for i in map(str, list(range(len(feature_names))))]
		tm_neg_lits = [i.lower() for i in tm_pos_lits]
		tm_lits = tm_pos_lits + tm_neg_lits
		f_translate_dict = dict(zip(tm_lits, cna_lits))
		models = []
		for i in range(len(self.datasets)):
			tmargs = {x: y[i], for x, y in self.param_dict.items()}
			tm = MultiClassTsetlinMachine(**tmargs)
			tm.fit(tts["X_train"][i], tts["y_train"][i])
			models.append(self.tm_to_asf(tm, tm.number_of_clauses, self.n_feature, f_translate_dict))
		return models

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
		fitted_clauses = clauses_from_TM(tm, nr_of_clauses, nr_of_features)
		suffs = [tm_clause_to_cna(x, translate_dict) for x in fitted_clauses]
		suffs = list(filter(lambda x: len(x) != 0, suffs))
		return "+".join(set(suffs))

	@staticmethod
	def tm_clause_to_cna(txt, translate_dict):
		for key, value in translate_dict.items():
			txt = re.sub(r"\b"+key+r"\b", value, txt)
			return txt
