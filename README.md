# Causally interpreted machine learning with CNA

This repo contains code for simulations where various "explainable" ML techniques such as tree-based methods are used on binary data to learn putative sufficient conditions for an outcome, and [`cna`](https://cran.r-project.org/web/packages/cna/index.html) is used to minimize and combine those sufficient conditions to models that are causally interpretable in light of an INUS-theory of causation.

For `sklearn`-ish models with python, a generic test setup is defined in `Paramtest` class defined in `aux_funcs.py`, specific tests for different ML models (currently decision trees and Tsetlin Machine) defined as methods of that class. `all_tests.py` for running whatever tests were last tried, expects data sets (generated in `datgen.R`) in `data/`.
Tests for random forests and rule ensembles in other directories, all in `R`.
For tests with python -implemented models, minimization, and correctness checks in `cor_check.R`.

TO DO: expand, document, automate.
