###############################################################################
# validate-AB-replication.R
#
# Validate that approach A (model10, analytic endogenous wait) and approach B
# (model11, exact FCFS companion) agree on mean CEA endpoints.
#
# WHY REPLICATION, NOT SHARED-SEED CRN: per-patient CRN banks align mol-vs-field
# WITHIN a model perfectly, but cannot align A against B — the two models inject
# different confirm-handling events into the hand-rolled loop, so the reactive
# resampling consumes the disease streams at different cadences and the same
# patient/seed lands on different draws. (A shared-seed null-treatment run shows
# A != B even when treatment is inert — a pure CRN-cadence artifact.) So we
# DISABLE CRN (CRN$force_disabled <- TRUE) and compare the two models over many
# INDEPENDENT replications. Paired seeds reduce the variance of the difference.
#
# Reads: A=B holds if the paired B-A 95% CI for each endpoint brackets 0.
###############################################################################

# Run with working directory = the repo's R/ directory, e.g.
#   cd <repo>/R && Rscript ../validation/validate-AB-replication.R
stopifnot(file.exists("model10.R"), file.exists("model11.R"))

K     <- 30L                  # replications
N     <- 1000L                # cohort size per replication
seeds <- seq_len(K)

run_reps <- function(modelfile) {
  invisible(capture.output(suppressMessages(source(modelfile))))
  CRN$force_disabled <- TRUE   # sticky: independent (non-CRN) replications
  do.call(rbind, lapply(seeds, function(s) {
    m <- summarise_run(des_run(modifyList(inputs, list(N = N, strategy = 'mol')),   seed = s))
    f <- summarise_run(des_run(modifyList(inputs, list(N = N, strategy = 'field')), seed = s))
    data.frame(seed = s,
               mol_dcost = m$dcost, mol_dqaly = m$dqaly,
               field_dcost = f$dcost, field_dqaly = f$dqaly,
               d_dcost = f$dcost - m$dcost, d_dqaly = f$dqaly - m$dqaly)
  }))
}

cat(sprintf("Running %d replications x {mol,field} x {A,B} at N=%d (CRN off)...\n\n", K, N))
A <- run_reps('model10.R')   # approach A
B <- run_reps('model11.R')   # approach B

se <- function(x) sd(x) / sqrt(length(x))

report <- function(metric, scale = 1, unit = "") {
  a <- A[[metric]] / scale; b <- B[[metric]] / scale
  tt <- t.test(b, a, paired = TRUE)
  flag <- if (tt$conf.int[1] <= 0 && tt$conf.int[2] >= 0) "  agree" else "  DIFFER"
  cat(sprintf("%-12s A=%9.3f (se %.3f)  B=%9.3f (se %.3f)  B-A=%+8.3f  95%%CI[%+.3f,%+.3f] p=%.3f%s%s\n",
              metric, mean(a), se(a), mean(b), se(b),
              mean(b) - mean(a), tt$conf.int[1], tt$conf.int[2], tt$p.value, flag, unit))
}

cat("=== Approach A (model10) vs B (model11): paired over", K, "independent seeds ===\n\n")
cat("-- QALYs --\n")
report('mol_dqaly');   report('field_dqaly');  report('d_dqaly')
cat("\n-- Costs ($1000s) --\n")
report('mol_dcost',   1000); report('field_dcost', 1000); report('d_dcost', 1000)

cat("\nKey endpoint is d_dqaly / d_dcost (the field-minus-molecular CEA result).\n")
cat("'agree' = paired 95% CI brackets 0.\n")
