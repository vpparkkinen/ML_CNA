import pyTsetlinMachine
import re

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


def tm_clause_to_cna(txt, translate_dict):
	for key, value in translate_dict.items():
		txt = re.sub(r"\b"+key+r"\b", value, txt)
	return txt

def tm_to_asf(tm,
							nr_of_clauses,
							nr_of_features,
							translate_dict):
	fitted_clauses = clauses_from_TM(tm, nr_of_clauses, nr_of_features)
	suffs = [tm_clause_to_cna(x, translate_dict) for x in fitted_clauses]
	suffs = list(filter(lambda x: len(x) != 0, suffs))
	return "+".join(set(suffs))
