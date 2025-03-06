def get_decision_paths(tree, feature_names):
    left = tree.tree_.children_left
    right = tree.tree_.children_right
    threshold = tree.tree_.threshold
    features = [feature_names[i] if i != -2 else "Leaf" for i in tree.tree_.feature]
    value = tree.tree_.value
    paths_by_class = {}

    def recurse(node, path):
       # print(node)
       # print(path)
        if left[node] == -1 and right[node] == -1:  # Leaf node
            predicted_class = np.argmax(value[node])  # Get class with highest probability
            #path.append(f"Class: {predicted_class}")
            # Store paths by class
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



# replace 'X <= 0.50' w/ 'x' and 'X > 0.50' w/ X.
def eq_to_lits(input_string):
      pattern_leq = r"([A-Za-z0-9]*)\s*<=\s*0\.50"
      pattern_gt = r"([A-Za-z0-9]*)\s*>\s*0\.50"
      output_string = re.sub(pattern_leq, lambda m: m.group(1).lower(), input_string)
      output_string = re.sub(pattern_gt, lambda m: m.group(1).upper(), output_string)
      return output_string

# translate fitted DTs paths to cna asfs
def dt_to_cna(dt, feature_names, outcome, incl_out = False):
    paths = get_decision_paths(dt, feature_names=feature_names)
    suffs = [eq_to_lits(x) for x in paths]
    if incl_out:
        return "+".join(suffs)+"<->"+outcome
    else:
        return "+".join(suffs)
