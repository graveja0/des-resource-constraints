#############################################################################
# VALIDATION HARNESS — MODEL B  (matched-seed, 10-seed means)
# Sources model-B.R for its function defs (sys.nframe()>0 suppresses run block).
# Emits machine lines: RESULT<TAB>B<TAB><tag><TAB><name><TAB><value>
#############################################################################
suppressMessages(source(here::here("model-B.R")))
options(width = 200)

SEEDS <- 1:10
N_DEF <- 1000

# realized companion wait, instrumented per-run via measure_waits()
wait_for <- function(ov, seed) {
  w <- tryCatch(measure_waits(ov, seed = seed), error = function(e) c(n = 0, mean_wait = NA, max_wait = NA))
  unname(w["mean_wait"])
}

extract <- function(ov, seed, do_outcomes = TRUE, do_wait = TRUE) {
  set.seed(seed)
  run <- des_run(ov)
  arr <- run$arrivals; res <- run$resources
  trt <- res[res$resource == "treatment", ]
  max_srv  <- if (nrow(trt)) max(trt$server) else 0
  min_srv  <- if (nrow(trt)) min(trt$server) else 0
  cap      <- ov$n.capacity
  over_cap <- if (nrow(trt)) any(trt$server > cap) else FALSE
  neg      <- if (nrow(trt)) any(trt$server < 0) else FALSE
  final_srv <- if (nrow(trt)) trt$server[nrow(trt)] else 0

  n_deaths  <- sum(arr$resource == "death")
  n_treated <- sum(arr$resource == "treatment_held")
  s1_names  <- unique(arr$name[arr$resource == "sick1"])
  n_s1 <- length(s1_names)
  s2 <- arr[arr$resource == "sick2", ]
  s2_py <- if (nrow(s2)) sum(pmin(s2$end_time, ov$horizon) - s2$start_time) else 0

  oc <- run$outcomes %>%
    summarise(cost = mean(cost), dcost = mean(dcost), qaly = mean(qaly), dqaly = mean(dqaly))

  mw <- if (do_wait) wait_for(ov, seed) else NA_real_

  list(dcost = oc$dcost, dqaly = oc$dqaly, cost = oc$cost, qaly = oc$qaly,
       n_deaths = n_deaths, n_s1 = n_s1, n_treated = n_treated,
       s2_py = s2_py, mean_wait = mw,
       max_srv = max_srv, min_srv = min_srv, over_cap = over_cap, neg = neg,
       final_srv = final_srv)
}

ms <- function(ov, seeds = SEEDS, do_wait = TRUE) {
  rows <- lapply(seeds, function(s) extract(ov, s, do_wait = do_wait))
  num <- function(f) { v <- sapply(rows, function(r) r[[f]]); v <- v[is.finite(v)]; if (length(v)) mean(v) else NA_real_ }
  se  <- function(f) { v <- sapply(rows, function(r) r[[f]]); v <- v[is.finite(v)]; if (length(v) > 1) sd(v)/sqrt(length(v)) else NA_real_ }
  list(dcost = num("dcost"), dqaly = num("dqaly"), dqaly_se = se("dqaly"),
       cost = num("cost"), qaly = num("qaly"),
       n_deaths = num("n_deaths"), n_s1 = num("n_s1"), n_treated = num("n_treated"),
       s2_py = num("s2_py"), s2_py_se = se("s2_py"), mean_wait = num("mean_wait"),
       max_srv = max(sapply(rows, function(r) r$max_srv)),
       min_srv = min(sapply(rows, function(r) r$min_srv)),
       over_cap = any(sapply(rows, function(r) isTRUE(r$over_cap))),
       neg = any(sapply(rows, function(r) isTRUE(r$neg))),
       final_srv = max(sapply(rows, function(r) r$final_srv)))
}

emit <- function(tag, lst) for (nm in names(lst)) {
  v <- lst[[nm]]; if (is.logical(v)) v <- as.integer(v)
  cat(sprintf("RESULT\tB\t%s\t%s\t%s\n", tag, nm, format(v, digits = 8, scientific = FALSE)))
}
base <- function(...) modifyList(inputs, list(...))
INF_CAP <- 1e9   # B treats Inf via a huge finite capacity

cat("### MODEL B VALIDATION START ###\n")

# (1) unconstrained limit
emit("soc",           ms(base(N = N_DEF, treatA = FALSE), do_wait = FALSE))
emit("unconstr_huge", ms(base(N = N_DEF, treatA = TRUE, n.capacity = INF_CAP)))

# (2) null-effect regression
nullify <- function(ov) modifyList(ov, list(tx_prog_factor = 1, tx_cure_factor = 1, u.TrtA = inputs$u.S1))
emit("null_soc", ms(nullify(base(N = N_DEF, treatA = FALSE)), do_wait = FALSE))
for (cc in list(c(tag="2", v=2), c(tag="5", v=5), c(tag="25", v=25), c(tag="Inf", v=INF_CAP)))
  emit(paste0("null_c", cc[["tag"]]),
       ms(nullify(base(N = N_DEF, treatA = TRUE, n.capacity = as.numeric(cc[["v"]]))), do_wait = FALSE))

# (3) face validity + (5) endogeneity in c
for (cc in list(c(tag="2", v=2), c(tag="5", v=5), c(tag="10", v=10), c(tag="25", v=25), c(tag="Inf", v=INF_CAP)))
  emit(paste0("c", cc[["tag"]]),
       ms(base(N = N_DEF, treatA = TRUE, n.capacity = as.numeric(cc[["v"]]))))

# (5b) endogeneity in N (fixed c=3)
for (nn in c(250, 500, 1000, 2000))
  emit(paste0("N", nn),
       ms(base(N = nn, treatA = TRUE, n.capacity = 3)))

cat("### MODEL B VALIDATION END ###\n")
