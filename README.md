# Specification-Sensitivity Audit Protocol

Code accompanying *"Where the Break Comes From: Specification Sensitivity in
Automated Financial-Analytics Classification."*

## What this is

A small set of general-purpose R functions for testing whether a
classification produced by an attribution-based analysis is robust to three
upstream choices that are usually treated as settled rather than as choices:

1. **Where a continuous timeline is split into two groups** (`boundary_sensitivity()`)
2. **Where a continuous score is thresholded into a category** (`threshold_sensitivity()`)
3. **How finely an outcome is decomposed into named factors before one factor is singled out** (`granularity_sensitivity()`)

None of these functions is specific to BYD, to DuPont decomposition, or to
finance. Each takes a generic panel (a data frame of factor values indexed
by a period identifier) and a list of candidate specifications, and reports
whether the downstream classification holds up across them. The protocol is
designed to be pointed at the output of any attribution-based classification
system — including the automated financial-analytics tools discussed in the
paper — given access to the underlying panel of inputs.

By design, none of these functions runs a significance test. The paper
argues that this is the right choice, not an oversight: the question this
protocol answers is whether a classification is invariant to defensible
alternative specifications, which is a different question from whether an
observed contrast could plausibly arise by chance in a six-observation
panel. See the paper's Method section for the full argument.

## Repository structure

```
R/
  specification_audit.R   # the five core functions, fully documented
examples/
  run_byd_example.R       # applies the protocol to the paper's BYD panel,
                           # reproducing Tables 2-4 exactly
outputs/                  # (empty; populated by running the example script)
```

## Requirements

Base R only (no packages required) for `specification_audit.R` and
`run_byd_example.R`. The optional brute-force verification function,
`shapley_brute_force()`, uses `gtools::permutations()` and is not called by
the main example; it exists so the closed-form shortcut used elsewhere in
the file can be independently checked on a small number of factors.

## Running the BYD example

```
Rscript examples/run_byd_example.R
```

This prints the decomposition-integrity check, then Tables 2, 3, and 4 from
the paper, computed directly from the Table 1 panel hardcoded in the script.

## Applying the protocol to a different dataset

1. Build a `data.frame` with one row per period and one column per named
   factor, plus a `period_id` column.
2. Decide which candidate period boundaries, thresholds, and/or factor
   groupings you want to test, and write down *why* each one is defensible
   independently of the result it produces, before running anything. The
   paper's central methodological point is that this justification has to
   come from outside the computation — a boundary chosen because it is
   documented in an external policy record, say, not because it was found
   by search to flip or preserve a classification.
3. Call `boundary_sensitivity()`, `threshold_sensitivity()`, and/or
   `granularity_sensitivity()` as shown in `examples/run_byd_example.R`,
   substituting your panel and candidate specifications for BYD's.

## Verification note

This code was developed and its output checked against an independent
Python reimplementation of the same arithmetic; the R scripts themselves
have not been executed in an R interpreter as part of preparing this
repository, since none was available in the preparation environment. Anyone
running `run_byd_example.R` should get output identical to Tables 2-4 in
the paper; please open an issue if it does not.

## Citation

If you use this protocol, please cite the paper it accompanies.
