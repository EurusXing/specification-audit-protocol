# =============================================================================
# run_byd_example.R
#
# A worked application of the portable specification-sensitivity audit
# protocol (R/specification_audit.R) to the BYD Co. Ltd. five-factor DuPont
# panel discussed in "Same Data, Opposite Verdicts: Measuring Subsidy
# Dependence in the EU-China Electric Vehicle Dispute."
#
# Running this script end-to-end reproduces Tables 3, 4, and 5 of the paper
# from the published panel in Table 2. Nothing in this file is required to
# use the protocol on a different dataset: it exists to demonstrate the
# calling convention and to let a reader independently verify the paper's
# reported figures against the underlying computation.
#
# Run from the repository root:
#   Rscript examples/run_byd_example.R
# =============================================================================

source("R/specification_audit.R")

# -----------------------------------------------------------------------------
# Table 2: BYD Co. Ltd., Five-Factor DuPont Panel, 2018-2023
# Source: BYD Co. Ltd. audited annual reports (HKEx); see paper's Data
# Availability statement for filing references.
# -----------------------------------------------------------------------------
byd_panel <- data.frame(
  period_id = 2018:2023,
  F1_tax_burden       = c(0.634, 0.664, 0.615, 0.674, 0.789, 0.806),
  F2_interest_burden  = c(0.584, 0.411, 0.688, 0.703, 0.941, 0.953),
  F3_ebit_margin_pct  = c(6.16,  4.86,  6.52,  3.04,  5.28,  6.49),
  F4_asset_turnover   = c(0.626, 0.624, 0.774, 0.851, 1.074, 1.027),
  F5_equity_multiplier = c(3.52, 3.45, 3.53, 3.11, 4.45, 4.90),
  ROE_pct             = c(5.04, 2.85, 7.55, 3.81, 18.73, 25.07)
)

factor_cols <- c("F1_tax_burden", "F2_interest_burden", "F3_ebit_margin_pct",
                  "F4_asset_turnover", "F5_equity_multiplier")

cat("=== Decomposition integrity check ===\n")
integrity <- check_decomposition_integrity(byd_panel, factor_cols, "ROE_pct")
print(data.frame(period_id = byd_panel$period_id, integrity_ok = integrity))
cat("\n")

# -----------------------------------------------------------------------------
# Reproduces Table 3: F2 and F4 under the published and alternative period
# partitions.
#
# Published partition:   2018-2021 "subsidy" (n=4) / 2022-2023 "post-subsidy" (n=2)
# Alternative partition: 2018-2020 "subsidy" (n=3) / 2021-2023 "post-subsidy" (n=3)
#
# The alternative partition is included here because it is independently
# motivated by China's documented, gradual NEV subsidy phase-out schedule
# (10% reduction in 2020, 20% in 2021, 30% in 2022, full withdrawal 1 Jan
# 2023), not because it was searched over to find a boundary that flips the
# published classification. See the paper's Method section for the full
# argument.
# -----------------------------------------------------------------------------
candidate_period_splits <- list(
  list(name = "published",   group1 = 2018:2021, group2 = 2022:2023),
  list(name = "alternative", group1 = 2018:2020, group2 = 2021:2023)
)

cat("=== Table 3: F2 (interest burden) boundary sensitivity ===\n")
f2_boundary <- boundary_sensitivity(byd_panel, "F2_interest_burden",
                                     candidate_period_splits)
print(f2_boundary, row.names = FALSE)
cat("\n")

cat("=== Table 3: F4 (asset turnover) boundary sensitivity ===\n")
f4_boundary <- boundary_sensitivity(byd_panel, "F4_asset_turnover",
                                     candidate_period_splits)
print(f4_boundary, row.names = FALSE)
cat("\n")

# -----------------------------------------------------------------------------
# Reproduces Table 4: sensitivity of the policy-resilience classification to
# the F2 >= threshold rule, bracketing the published 0.90 threshold with
# 0.85 and 0.95.
# -----------------------------------------------------------------------------
post_period_f2_means <- setNames(
  f2_boundary$mean[f2_boundary$group == "group2"],
  f2_boundary$split_name
)

cat("=== Table 4: threshold sensitivity (F2 classification) ===\n")
thresh_result <- threshold_sensitivity(
  post_period_f2_means,
  thresholds = c(0.85, 0.90, 0.95),
  labels = c("dependent", "resilient")
)
print(thresh_result, row.names = FALSE)
cat("\n")

# -----------------------------------------------------------------------------
# Reproduces Table 5: granularity sensitivity. Collapses F1, F2, F3 into a
# single three-factor "net margin" term (net profit margin x asset turnover
# x equity multiplier is the conventional three-factor DuPont model; F1 x F2
# x F3 here is the net-margin component of that model) and re-tests the
# same published boundary.
# -----------------------------------------------------------------------------
cat("=== Table 5: granularity sensitivity (3-factor net margin) ===\n")
net_margin_result <- granularity_sensitivity(
  byd_panel,
  collapse_cols = c("F1_tax_burden", "F2_interest_burden", "F3_ebit_margin_pct"),
  collapsed_name = "net_margin_3factor",
  candidate_splits = list(candidate_period_splits[[1]])  # published boundary only
)
print(net_margin_result, row.names = FALSE)
cat("\n")

cat("Done. Compare the printed tables above against Tables 3-5 in the paper.\n")
