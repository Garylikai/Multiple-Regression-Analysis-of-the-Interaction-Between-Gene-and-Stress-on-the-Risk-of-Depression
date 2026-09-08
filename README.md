# Regression Analysis of Gene and Stress Variables in Synthetic Depression Data

An AMS 578 project from Spring 2021 applying multiple regression diagnostics, multiple imputation, transformation, model selection, and interaction analysis to a **synthetic** dataset motivated by research on depression.

## Project overview

The workflow:

- summarizes and cleans three provided data files;
- uses classification and regression tree (CART) imputation through `mice` to construct two completed datasets;
- examines regression assumptions and applies a response transformation;
- uses BIC-based stepwise selection and model comparison criteria; and
- evaluates main effects, gene–gene interactions, and gene–environment interactions before pooling estimates.

In the selected pooled model, several genetic and stress-related variables and pairwise gene interactions were retained. No gene–environment interaction was retained. Because the data are synthetic and the analysis is a course exercise, the results have no clinical, diagnostic, or causal interpretation.

## Repository contents

- `analysis.R` — complete R analysis
- `IDEgroup.csv`, `IDGgroup.csv`, and `IDYgroup.csv` — analysis inputs
- `final.pdf` — final report

## Author

Kai Li.
