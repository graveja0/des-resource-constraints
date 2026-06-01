#############################################################################
# VALIDATION HARNESS — MODEL A  (matched-seed, 10-seed means)
# Sources model-A.R with run block suppressed; exercises the 6 tests.
# Emits machine lines: RESULT<TAB>A<TAB><tag><TAB><name><TAB><value>
#############################################################################
Sys.setenv(MODEL_A_NORUN = "1")
suppressMessages(source(here::here("model-A.R")))
options(width = 200)

SEEDS <- 1:10
N_DEF <- 1000

extract <- function(ov, seed, compute_outcomes = TRUE) {
  set.seed(seed)
  run <- des_run(ov, compute_outcomes = compute_outcomes)
  arr <- run$arrivals; res <- run$resources
  ra <- res[res$resource == "A", ]
  max_srv  <- if (nrow(ra)) max(ra$server) else 0
  min_srv  <- if (nrow(ra)) min(ra$server) else 0
  over_cap <- if (nrow(ra)) any(ra$server > ra$capacity) else FALSE
  neg      <- if (nrow(ra)) any(ra$server < 0) else FALSE
  final_srv <- if (nrow(ra)) ra$server[nrow(ra)] else 0
  wl_len   <- length(WL$join_order)

  n_deaths <- sum(arr$resource == "death")
  s1_names <- unique(arr$name[arr$resource == "sick1"])
  a_names  <- unique(arr$name[arr$resource == "A"])
  n_s1 <- length(s1_names); n_treated <- length(a_names)
  s2 <- arr[arr$resource == "sick2", ]
  s2_py <- if (nrow(s2)) sum(pmin(s2$end_time, ov$horizon) - s2$start_time) else 0

  waits <- numeric(0)
  a  <- arr[arr$resource == "A", ]; s1 <- arr[arr$resource == "sick1", ]
  if (nrow(a)) for (nm in a_names) {
    a_starts  <- sort(a$start_time[a$name == nm])
    s1_starts <- sort(s1$start_time[s1$name == nm])
    for (ast in a_starts) {
      prior <- s1_starts[s1_starts <= ast + 1e-9]
      if (length(prior)) waits <- c(waits, ast - max(prior))
    }
  }
  mean_wait <- if (length(waits)) mean(waits) else NA_real_

  oc <- NULL
  if (compute_outcomes) oc <- run$outcomes %>%
    summarise(cost = mean(cost), dcost = mean(dcost), qaly = mean(qaly), dqaly = mean(dqaly))

  list(dcost = if (!is.null(oc)) oc$dcost else NA_real_,
       dqaly = if (!is.null(oc)) oc$dqaly else NA_real_,
       cost = if (!is.null(oc)) oc$cost else NA_real_,
       qaly = if (!is.null(oc)) oc$qaly else NA_real_,
       n_deaths = n_deaths, n_s1 = n_s1, n_treated = n_treated,
       s2_py = s2_py, mean_wait = mean_wait,
       max_srv = max_srv, min_srv = min_srv, over_cap = over_cap, neg = neg,
       final_srv = final_srv, wl_len = wl_len)
}

ms <- function(ov, seeds = SEEDS, compute_outcomes = TRUE) {
  rows <- lapply(seeds, function(s) extract(ov, s, compute_outcomes))
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
       final_srv = max(sapply(rows, function(r) r$final_srv)),
       wl_len = max(sapply(rows, function(r) r$wl_len)))
}

emit <- function(tag, lst) for (nm in names(lst)) {
  v <- lst[[nm]]; if (is.logical(v)) v <- as.integer(v)
  cat(sprintf("RESULT\tA\t%s\t%s\t%s\n", tag, nm, format(v, digits = 8, scientific = FALSE)))
}
base <- function(...) modifyList(inputs, list(...))

cat("### MODEL A VALIDATION START ###\n")

# (1) unconstrained limit
emit("soc",           ms(base(N = N_DEF, treatA = FALSE)))
emit("unconstr_huge", ms(base(N = N_DEF, treatA = TRUE, n.capacity = 1e6)))
emit("unconstr_inf",  ms(base(N = N_DEF, treatA = TRUE, n.capacity = Inf)))

# (2) null-effect regression
nullify <- function(ov) modifyList(ov, list(tx_prog_factor = 1, tx_recov_factor = 1, u.TrtA = inputs$u.S1))
emit("null_soc", ms(nullify(base(N = N_DEF, treatA = FALSE))))
for (cc in c(2, 5, 25, Inf))
  emit(paste0("null_c", ifelse(is.infinite(cc), "Inf", cc)),
       ms(nullify(base(N = N_DEF, treatA = TRUE, n.capacity = cc))))

# (3) face validity + (5) endogeneity in c
for (cc in c(2, 5, 10, 25, Inf))
  emit(paste0("c", ifelse(is.infinite(cc), "Inf", cc)),
       ms(base(N = N_DEF, treatA = TRUE, n.capacity = cc)))

# (5b) endogeneity in N (fixed c=3, queue/leak audit only)
for (nn in c(250, 500, 1000, 2000))
  emit(paste0("N", nn),
       ms(base(N = nn, treatA = TRUE, n.capacity = 3), compute_outcomes = FALSE))

cat("### MODEL A VALIDATION END ###\n")
