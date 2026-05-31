#############################################################################
# CEA / ICER HARNESS
# Cost-effectiveness of treatment under different capacity assumptions:
#   reference : No treatment (SoC)            -- treatA = FALSE
#   strategy  : Infinite capacity             -- treatA = TRUE, huge cap (Approach A engine)
#   strategy  : Constrained capacity, A       -- model-A.R, treatA = TRUE, n.capacity = c
#   strategy  : Constrained capacity, B       -- model-B.R, treatA = TRUE, n.capacity = c
#
# Discounted cost / QALY are read exactly as the validation harnesses do
#   (run$outcomes |> summarise(mean(dcost), mean(dqaly))), discounted at 3%.
# Reports mean +/- SE over SEEDS, incrementals vs SoC, ICERs, and the dampack frontier.
#
# Run:  Rscript drafts/cea-icer-harness.R
#   env knobs: CEA_SEEDS (default 10), CEA_N (default 1000), CEA_CAPS (default "2,5,25")
#   The split_arrivals() costing is the bottleneck; a full N=1000/10-seed run is minutes.
#############################################################################
suppressMessages({ library(dplyr); library(here) })

SEEDS <- seq_len(as.integer(Sys.getenv("CEA_SEEDS", "10")))
N     <- as.integer(Sys.getenv("CEA_N", "1000"))
CAPS  <- as.numeric(strsplit(Sys.getenv("CEA_CAPS", "2,5,25"), ",")[[1]])
INF_CAP <- 1e9   # both engines treat "infinite" via a capacity that never binds

# mean per-patient discounted cost & QALY for one run (same extraction as val-harness-*.R)
mean_outcome <- function(run) {
  o <- run$outcomes %>% dplyr::summarise(dcost = mean(dcost), dqaly = mean(dqaly))
  c(dcost = o$dcost, dqaly = o$dqaly)
}
# mean +/- SE across SEEDS for a scenario (des_run + inputs are whichever model is currently sourced)
# Also times each des_run (simulation + split_arrivals costing) -> mean wall-clock seconds / run.
run_strategy <- function(...) {
  ov <- modifyList(inputs, list(N = N, ...))
  rows <- lapply(SEEDS, function(s) {
    set.seed(s)
    el  <- system.time(run <- des_run(ov))[["elapsed"]]
    c(mean_outcome(run), sec = el)
  })
  m <- do.call(rbind, rows)
  c(dcost = mean(m[, "dcost"]), dcost_se = sd(m[, "dcost"]) / sqrt(nrow(m)),
    dqaly = mean(m[, "dqaly"]), dqaly_se = sd(m[, "dqaly"]) / sqrt(nrow(m)),
    sec = mean(m[, "sec"]), sec_se = sd(m[, "sec"]) / sqrt(nrow(m)))
}

res <- list()

## ---- Approach A engine: SoC (reference), Infinite, Constrained-A at each c ----
Sys.setenv(MODEL_A_NORUN = "1")
suppressMessages(source(here::here("model-A.R")))
res[["SoC"]]      <- run_strategy(treatA = FALSE)                       # identical across engines
res[["Infinite"]] <- run_strategy(treatA = TRUE, n.capacity = INF_CAP)  # unattainable benchmark (A's dose)
for (cc in CAPS)
  res[[paste0("Constrained-A (c=", cc, ")")]] <- run_strategy(treatA = TRUE, n.capacity = cc)

## ---- Approach B engine: Constrained-B at each c ----
suppressMessages(source(here::here("model-B.R")))
for (cc in CAPS)
  res[[paste0("Constrained-B (c=", cc, ")")]] <- run_strategy(treatA = TRUE, n.capacity = cc)

## ---- assemble, incrementals vs SoC, ICERs ----
tab <- data.frame(
  strategy = names(res),
  dcost    = sapply(res, `[`, "dcost"),
  dcost_se = sapply(res, `[`, "dcost_se"),
  dqaly    = sapply(res, `[`, "dqaly"),
  dqaly_se = sapply(res, `[`, "dqaly_se"),
  sec      = sapply(res, `[`, "sec"),
  sec_se   = sapply(res, `[`, "sec_se"),
  row.names = NULL
)
soc <- tab[tab$strategy == "SoC", ]
tab$inc_cost <- tab$dcost - soc$dcost
tab$inc_qaly <- tab$dqaly - soc$dqaly
tab$icer_vs_soc <- ifelse(tab$strategy == "SoC", NA_real_, tab$inc_cost / tab$inc_qaly)

cat(sprintf("\n### CEA/ICER  (N=%d, %d seeds, discounted 3%%) ###\n", N, length(SEEDS)))
print(transform(tab,
                dcost = round(dcost), dcost_se = round(dcost_se),
                dqaly = round(dqaly, 3), dqaly_se = round(dqaly_se, 3),
                sec = round(sec, 2), sec_se = round(sec_se, 2),
                inc_cost = round(inc_cost), inc_qaly = round(inc_qaly, 3),
                icer_vs_soc = round(icer_vs_soc)), row.names = FALSE)

## ---- dampack frontier (descriptive: includes the unattainable Infinite benchmark) ----
if (requireNamespace("dampack", quietly = TRUE)) {
  ic <- dampack::calculate_icers(cost = tab$dcost, effect = tab$dqaly, strategies = tab$strategy)
  cat("\n--- dampack frontier (all strategies) ---\n"); print(ic)
}

## machine-readable for downstream table-building
for (i in seq_len(nrow(tab)))
  cat(sprintf("CEARESULT\t%s\t%.1f\t%.1f\t%.4f\t%.4f\t%.3f\t%.3f\n",
              tab$strategy[i], tab$dcost[i], tab$dcost_se[i], tab$dqaly[i], tab$dqaly_se[i],
              tab$sec[i], tab$sec_se[i]))
