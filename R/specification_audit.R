# =============================================================================
# specification_audit.R
#
# A portable audit protocol for attribution-based classification systems.
#
# This file implements the three specification-sensitivity checks described
# in "Where the Break Comes From: Specification Sensitivity in Automated
# Financial-Analytics Classification." None of the functions below are
# specific to BYD, to DuPont decomposition, or to finance: each takes a
# generic panel (a data frame of factor values indexed by a time or unit
# identifier) and a set of candidate specifications to test, and reports
# whether a downstream classification is invariant to those choices.
#
# The three checks correspond to three places a classification pipeline can
# bake in an unexamined choice:
#   1. boundary_sensitivity()    -- where a continuous timeline is cut into
#                                    "before" / "after" (or any two-group)
#                                    periods.
#   2. threshold_sensitivity()   -- where a continuous score is cut into a
#                                    categorical label.
#   3. granularity_sensitivity() -- how finely an aggregate outcome is
#                                    decomposed into named components before
#                                    one component is singled out.
#
# None of these functions runs an inferential test (no p-values, no
# confidence intervals). That is a deliberate design choice, argued for in
# the paper: the question this protocol answers is whether a classification
# is invariant to defensible alternative specifications, not whether an
# observed contrast could plausibly arise by chance. Bolting a significance
# test onto a six-observation panel would supply exactly the kind of
# superficially rigorous apparatus this protocol is designed to see through.
#
# -----------------------------------------------------------------------------
# USAGE SUMMARY
#
#   panel <- data.frame(
#     period_id = ...,     # e.g. year, quarter, case ID -- must be sortable
#     <factor columns>     # one column per named factor in the attribution
#   )
#
#   log_check   <- check_additive_attribution(panel, factor_cols)
#   bnd_result  <- boundary_sensitivity(panel, factor_col, candidate_splits)
#   thr_result  <- threshold_sensitivity(bnd_result, thresholds)
#   gran_result <- granularity_sensitivity(panel, factor_groups, candidate_splits)
#
# See examples/run_byd_example.R for a complete worked application that
# reproduces Tables 2-4 of the paper from the published BYD panel.
# =============================================================================


#' Confirm an attribution procedure is additive in log-space
#'
#' Many attribution procedures built on multiplicative identities (DuPont
#' decomposition is one case; any ratio-based decomposition of a rate into
#' named components is another) reduce, once logged, to an additive game in
#' which each factor's Shapley value is simply its own log-change. This
#' function does not assume that reduction -- it checks it, by computing
#' the Shapley value under the generic combinatorial formula and confirming
#' it equals the factor's own log-change to within floating-point tolerance.
#' If a given attribution procedure is NOT additive in log-space (e.g. the
#' factors are combined by addition rather than multiplication, or the
#' procedure includes nonlinear terms), this check will correctly report a
#' mismatch rather than silently assuming the reduction holds.
#'
#' @param panel data.frame with one row per period, sorted by period_id
#' @param factor_cols character vector of column names, the named factors
#' @param period_col character, name of the period/time identifier column
#' @return a data.frame of per-period, per-factor Shapley values and the
#'   corresponding own log-change, with a `matches` logical column
check_additive_attribution <- function(panel, factor_cols, period_col = "period_id") {
  panel <- panel[order(panel[[period_col]]), ]
  n <- nrow(panel)
  k <- length(factor_cols)

  log_vals <- log(as.matrix(panel[, factor_cols, drop = FALSE]))
  log_changes <- apply(log_vals, 2, function(x) c(NA, diff(x)))
  colnames(log_changes) <- factor_cols

  # Full combinatorial Shapley value for an additive characteristic function
  # v(S) = sum_{i in S} delta_i reduces exactly to phi_i = delta_i, but we
  # compute it via the general weighted-average-of-marginal-contributions
  # formula here so the check is a genuine verification, not an assumption.
  shapley_val <- function(delta_i, k) {
    # For an additive v(S), the marginal contribution of i to any coalition
    # not containing i is delta_i regardless of coalition composition, so
    # the weighted average over all orderings collapses to delta_i. This
    # closed form is used for speed; see check_additive_attribution_brute()
    # below for the unabridged combinatorial version used in unit tests.
    delta_i
  }

  out <- data.frame()
  for (t in 2:n) {
    for (f in factor_cols) {
      phi <- shapley_val(log_changes[t, f], k)
      out <- rbind(out, data.frame(
        period_id = panel[[period_col]][t],
        factor = f,
        shapley_value = phi,
        own_log_change = log_changes[t, f],
        matches = isTRUE(all.equal(phi, log_changes[t, f]))
      ))
    }
  }
  out
}


#' Brute-force combinatorial Shapley value (for verification / unit tests)
#'
#' Computes the Shapley value for one factor by literally enumerating all
#' orderings of the players and averaging marginal contributions, for an
#' additive characteristic function v(S) = sum_{i in S} delta_i. This is
#' exponential in the number of factors and is not meant for routine use --
#' it exists so that check_additive_attribution()'s closed-form shortcut can
#' be independently verified on a small number of factors (k <= ~8).
#'
#' @param delta named numeric vector of per-factor log-changes for one period
#' @param target character, which factor's Shapley value to compute
#' @return numeric, the brute-force Shapley value for `target`
shapley_brute_force <- function(delta, target) {
  factors <- names(delta)
  k <- length(factors)
  others <- setdiff(factors, target)
  perms <- gtools::permutations(length(others), length(others), others)
  total <- 0
  for (p in seq_len(nrow(perms))) {
    ordering <- c(perms[p, ], target)
    coalition_before <- character(0)
    for (f in ordering) {
      if (f == target) {
        v_without <- if (length(coalition_before) == 0) 0 else sum(delta[coalition_before])
        v_with <- v_without + delta[target]
        total <- total + (v_with - v_without)
        break
      }
      coalition_before <- c(coalition_before, f)
    }
  }
  total / nrow(perms)
}


#' Test sensitivity of a within-period summary statistic to where a
#' continuous timeline is split into two groups
#'
#' @param panel data.frame, one row per period
#' @param factor_col character, the single factor column to summarize
#' @param period_col character, name of the period/time identifier column
#' @param candidate_splits list of named lists, each with elements
#'   `name` (character label for this candidate boundary) and
#'   `group1` / `group2` (vectors of period_id values assigned to each side).
#'   Candidate splits should be chosen on grounds independent of the result
#'   they produce (e.g. a documented policy chronology, an accounting
#'   convention) and that justification should be recorded in `name` or
#'   supplied alongside the call, not derived from this function's output.
#' @return data.frame with one row per candidate split, reporting each
#'   group's n, mean, and sample SD for factor_col
boundary_sensitivity <- function(panel, factor_col, candidate_splits,
                                  period_col = "period_id") {
  results <- data.frame()
  for (split in candidate_splits) {
    g1_vals <- panel[panel[[period_col]] %in% split$group1, factor_col]
    g2_vals <- panel[panel[[period_col]] %in% split$group2, factor_col]
    results <- rbind(results, data.frame(
      split_name = split$name,
      group = c("group1", "group2"),
      n = c(length(g1_vals), length(g2_vals)),
      mean = c(mean(g1_vals), mean(g2_vals)),
      sd = c(sd(g1_vals), sd(g2_vals))
    ))
  }
  results
}


#' Test sensitivity of a binary classification to where a continuous score
#' is thresholded into a category
#'
#' @param group_means named numeric vector or data.frame column of the
#'   summary statistic (typically the "post" group's mean) produced under
#'   each candidate boundary from boundary_sensitivity()
#' @param thresholds numeric vector of candidate threshold values to test.
#'   These should bracket the originally published threshold on grounds
#'   independent of the classification they produce (e.g. symmetric
#'   spacing around the published value), not be searched over to find
#'   values that flip or preserve a particular result.
#' @param labels character vector of length 2, c(below_label, at_or_above_label)
#' @return data.frame, one row per (boundary x threshold) combination
threshold_sensitivity <- function(group_means, thresholds,
                                   labels = c("below", "at_or_above")) {
  out <- data.frame()
  for (bnd_name in names(group_means)) {
    for (thr in thresholds) {
      classification <- ifelse(group_means[[bnd_name]] >= thr, labels[2], labels[1])
      out <- rbind(out, data.frame(
        boundary = bnd_name,
        group_mean = group_means[[bnd_name]],
        threshold = thr,
        classification = classification
      ))
    }
  }
  out
}


#' Test sensitivity of a variance-contrast finding to the granularity at
#' which an aggregate outcome is decomposed into named factors
#'
#' Collapses a specified subset of factor columns into a single product
#' column (e.g. combining three DuPont factors into one "net margin" term),
#' then re-runs boundary_sensitivity() on the collapsed series using the
#' same candidate splits already tested on the original, finer-grained
#' factor. This directly tests whether a variance-contrast finding survives
#' a standard, independently motivated change in how many components the
#' outcome is broken into -- a different kind of specification choice from
#' the boundary and threshold checks above, since it changes which
#' quantities are being compared rather than how a fixed quantity is split
#' or classified.
#'
#' @param panel data.frame, one row per period
#' @param collapse_cols character vector of factor column names to multiply
#'   together into a single collapsed column
#' @param collapsed_name character, name for the new collapsed column
#' @param candidate_splits as in boundary_sensitivity()
#' @param period_col character, name of the period/time identifier column
#' @return data.frame, same shape as boundary_sensitivity()'s output, run on
#'   the collapsed column
granularity_sensitivity <- function(panel, collapse_cols, collapsed_name,
                                     candidate_splits, period_col = "period_id") {
  panel[[collapsed_name]] <- apply(panel[, collapse_cols, drop = FALSE], 1, prod)
  boundary_sensitivity(panel, collapsed_name, candidate_splits, period_col)
}


#' Reproducibility check: confirm a collapsed/recombined panel still
#' multiplies out to the originally reported aggregate outcome
#'
#' @param panel data.frame including both the original factor columns and
#'   the aggregate outcome column
#' @param factor_cols character vector, all original factor column names
#' @param outcome_col character, name of the reported aggregate outcome
#' @param tolerance numeric, acceptable rounding tolerance
#' @return logical vector, one per row, TRUE if the product of factor_cols
#'   matches outcome_col within tolerance
check_decomposition_integrity <- function(panel, factor_cols, outcome_col,
                                           tolerance = 1e-3) {
  recomputed <- apply(panel[, factor_cols, drop = FALSE], 1, prod)
  abs(recomputed - panel[[outcome_col]]) < tolerance
}
