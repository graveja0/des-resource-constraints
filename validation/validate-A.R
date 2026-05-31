## ===========================================================================
## VALIDATION HARNESS — MODEL A
## Sources model-A.R function defs (MODEL_A_NORUN) and runs the 6 required tests.
## Emits machine-readable lines prefixed "RESULT|" for downstream parsing.
## ===========================================================================
Sys.setenv(MODEL_A_NORUN = "1")
suppressMessages(source("model-A.R"))

emit <- function(...) cat(sprintf("RESULT|%s\n", paste(..., sep = "|")))

## ---- audit one run: capacity, leak, deaths, S2 person-years, mean wait -----
audit_full <- function(run, ins) {
  arr <- run$arrivals
  res <- run$resources
  ra  <- res[res$resource == "A", ]
  over_cap <- if (nrow(ra)) any(ra$server > ra$capacity) else FALSE
  neg      <- if (nrow(ra)) any(ra$server < 0) else FALSE
  max_srv  <- if (nrow(ra)) max(ra$server) else 0
  last_srv <- if (nrow(ra)) ra$server[which.max(ra$time)] else 0   # final server count

  a   <- arr[arr$resource == "A", ]
  s1  <- arr[arr$resource == "sick1", ]
  s2  <- arr[arr$resource == "sick2", ]
  treated_names <- unique(a$name)
  n_s1      <- length(unique(s1$name))
  n_treated <- length(treated_names)
  n_deaths  <- sum(arr$resource == "death")
  n_s2      <- length(unique(s2$name))

  ## S2 person-years (UNDISCOUNTED time-in-S2 summed across patients), capped at horizon
  s2py <- 0
  if (nrow(s2)) {
    e <- pmin(s2$end_time, ins$horizon)
    st <- pmin(s2$start_time, ins$horizon)
    s2py <- sum(pmax(0, e - st))
  }

  ## realized wait S1-entry -> A-seize
  waits <- numeric(0)
  if (nrow(a)) for (nm in treated_names) {
    a_starts  <- sort(a$start_time[a$name == nm])
    s1_starts <- sort(s1$start_time[s1$name == nm])
    for (ast in a_starts) {
      prior <- s1_starts[s1_starts <= ast + 1e-9]
      if (length(prior)) waits <- c(waits, ast - max(prior))
    }
  }
  list(over_cap = over_cap, neg = neg, max_srv = max_srv, last_srv = last_srv,
       n_s1 = n_s1, n_treated = n_treated, n_deaths = n_deaths, n_s2 = n_s2,
       s2py = s2py,
       mean_wait = if (length(waits)) mean(waits) else NA_real_)
}

## multi-seed mean of an audit metric (no costing -> fast)
maudit <- function(ov, seeds) {
  rows <- lapply(seeds, function(s) { set.seed(s); audit_full(des_run(ov, compute_outcomes = FALSE), ov) })
  g <- function(f) mean(sapply(rows, f), na.rm = TRUE)
  list(mean_wait = g(function(x) x$mean_wait),
       n_treated = g(function(x) x$n_treated),
       n_s1      = g(function(x) x$n_s1),
       n_s2      = g(function(x) x$n_s2),
       s2py      = g(function(x) x$s2py),
       n_deaths  = g(function(x) x$n_deaths),
       over_cap  = any(sapply(rows, function(x) x$over_cap)),
       neg       = any(sapply(rows, function(x) isTRUE(x$neg))),
       max_srv   = max(sapply(rows, function(x) x$max_srv)),
       last_srv  = max(sapply(rows, function(x) x$last_srv)))
}

## multi-seed mean CEA outcome (full costing)
moutcome <- function(ov, seeds) {
  vv <- lapply(seeds, function(s) {
    set.seed(s)
    r <- des_run(ov)
    oc <- summarise_outcomes(r)
    a  <- audit_full(r, ov)
    list(dcost = oc$dcost, dqaly = oc$dqaly, cost = oc$cost, qaly = oc$qaly,
         deaths = a$n_deaths, s2py = a$s2py, mean_wait = a$mean_wait, n_treated = a$n_treated)
  })
  g <- function(f) mean(sapply(vv, f), na.rm = TRUE)
  s <- function(f) sd(sapply(vv, f))
  list(dcost = g(function(x) x$dcost), dqaly = g(function(x) x$dqaly),
       dqaly_sd = s(function(x) x$dqaly),
       cost = g(function(x) x$cost), qaly = g(function(x) x$qaly),
       deaths = g(function(x) x$deaths), s2py = g(function(x) x$s2py),
       mean_wait = g(function(x) x$mean_wait), n_treated = g(function(x) x$n_treated))
}

N   <- 1000
S5  <- 1:5
S10 <- 1:10

cat("\n################ MODEL A VALIDATION ################\n\n")

## (1) UNCONSTRAINED LIMIT: c=Inf treatA vs SoC ; also a HUGE finite c -------
cat("--- (1) unconstrained limit ---\n")
soc <- moutcome(modifyList(inputs, list(N = N, treatA = FALSE, treatB = FALSE)), S5)
unc <- moutcome(modifyList(inputs, list(N = N, treatA = TRUE,  treatB = FALSE, n.capacity = Inf)), S5)
big <- moutcome(modifyList(inputs, list(N = N, treatA = TRUE,  treatB = FALSE, n.capacity = 100000)), S5)
emit("A","soc",   N, "NA", sprintf("%.4f",soc$dqaly), sprintf("%.0f",soc$dcost), sprintf("%.1f",soc$deaths), sprintf("%.2f",soc$s2py), "NA")
emit("A","cInf",  N, "Inf", sprintf("%.4f",unc$dqaly), sprintf("%.0f",unc$dcost), sprintf("%.1f",unc$deaths), sprintf("%.2f",unc$s2py), sprintf("%.4f",unc$mean_wait))
emit("A","cBig",  N, "1e5", sprintf("%.4f",big$dqaly), sprintf("%.0f",big$dcost), sprintf("%.1f",big$deaths), sprintf("%.2f",big$s2py), sprintf("%.4f",big$mean_wait))
cat(sprintf("  SoC      dqaly=%.4f dcost=%.0f deaths=%.1f\n", soc$dqaly, soc$dcost, soc$deaths))
cat(sprintf("  A c=Inf  dqaly=%.4f dcost=%.0f deaths=%.1f wait=%.4f\n", unc$dqaly, unc$dcost, unc$deaths, unc$mean_wait))
cat(sprintf("  A c=1e5  dqaly=%.4f dcost=%.0f deaths=%.1f wait=%.4f\n", big$dqaly, big$dcost, big$deaths, big$mean_wait))

## (2) NULL-EFFECT: prog=1,recov=1,u.TrtA=u.S1 -> identical to SoC across c ---
cat("\n--- (2) null-effect regression ---\n")
null_base <- modifyList(inputs, list(N = N, tx_prog_factor = 1, tx_recov_factor = 1, u.TrtA = inputs$u.S1))
nsoc <- moutcome(modifyList(null_base, list(treatA = FALSE, treatB = FALSE)), S10)
se_n <- nsoc$dqaly_sd / sqrt(length(S10))
emit("A","null_soc", N, "NA", sprintf("%.4f",nsoc$dqaly), sprintf("%.4f",se_n), "NA","NA","NA")
cat(sprintf("  null SoC     dqaly=%.4f (SE=%.4f)\n", nsoc$dqaly, se_n))
for (cc in c(2,5,25,Inf)) {
  nn <- moutcome(modifyList(null_base, list(treatA = TRUE, treatB = FALSE, n.capacity = cc)), S10)
  d  <- nn$dqaly - nsoc$dqaly
  emit("A","null", N, ifelse(is.infinite(cc),"Inf",cc), sprintf("%.4f",nn$dqaly), sprintf("%.4f",d), sprintf("%.2f",d/se_n), sprintf("%.4f",nn$dqaly_sd/sqrt(length(S10))),"NA")
  cat(sprintf("  null A c=%-4s dqaly=%.4f  d=%+.4f (%+.1f SE)\n", ifelse(is.infinite(cc),"Inf",cc), nn$dqaly, d, d/se_n))
}

## (3) FACE VALIDITY: deaths & S2 person-years monotone as c shrinks ---------
cat("\n--- (3) face validity: deaths & S2 person-years vs c (treatA) ---\n")
for (cc in c(1,2,5,25,Inf)) {
  a <- maudit(modifyList(inputs, list(N = N, treatA = TRUE, treatB = FALSE, n.capacity = cc)), S5)
  emit("A","face", N, ifelse(is.infinite(cc),"Inf",cc), sprintf("%.1f",a$n_deaths), sprintf("%.2f",a$s2py), sprintf("%.1f",a$n_treated), sprintf("%.4f",a$mean_wait), sprintf("%.1f",a$n_s2))
  cat(sprintf("  c=%-4s deaths=%.1f  S2py=%.1f  treated=%.1f/%d  wait=%.4f\n",
              ifelse(is.infinite(cc),"Inf",cc), a$n_deaths, a$s2py, a$n_treated, a$n_s1, a$mean_wait))
}

## (4) LEAK AUDIT: server -> 0 past horizon, no neg, no overcap (across c) ----
cat("\n--- (4) leak audit ---\n")
for (cc in c(1,2,5,25)) {
  a <- maudit(modifyList(inputs, list(N = N, treatA = TRUE, treatB = FALSE, n.capacity = cc)), S5)
  emit("A","leak", N, cc, as.character(a$over_cap), as.character(a$neg), sprintf("%.0f",a$max_srv), sprintf("%.0f",a$last_srv), "NA")
  cat(sprintf("  c=%-3d overCap=%s neg=%s maxSrv=%.0f finalSrv=%.0f (WL len=%d)\n",
              cc, a$over_cap, a$neg, a$max_srv, a$last_srv, length(WL$join_order)))
}

## (5) ENDOGENEITY: wait up as c down (fixed N) and as N up (fixed c) ---------
cat("\n--- (5) endogeneity: wait vs c (N=1000) ---\n")
for (cc in c(1,2,5,25)) {
  a <- maudit(modifyList(inputs, list(N = N, treatA = TRUE, treatB = FALSE, n.capacity = cc)), S5)
  emit("A","endo_c", N, cc, sprintf("%.4f",a$mean_wait), sprintf("%.1f",a$n_treated), "NA","NA","NA")
  cat(sprintf("  c=%-3d wait=%.4f treated=%.1f\n", cc, a$mean_wait, a$n_treated))
}
cat("--- (5) endogeneity: wait vs N (c=3) ---\n")
for (nn in c(250,500,1000,2000)) {
  a <- maudit(modifyList(inputs, list(N = nn, treatA = TRUE, treatB = FALSE, n.capacity = 3)), S5)
  emit("A","endo_N", nn, 3, sprintf("%.4f",a$mean_wait), sprintf("%.1f",a$n_treated), "NA","NA","NA")
  cat(sprintf("  N=%-4d wait=%.4f treated=%.1f\n", nn, a$mean_wait, a$n_treated))
}

## (6) A-vs-B matched CEA cell: c in {2,5}, N=1000 -> deaths/S2py/wait/dQALY/dcost
cat("\n--- (6) matched CEA cells for A-vs-B (N=1000, 5-seed) ---\n")
for (cc in c(2,5,Inf)) {
  o <- moutcome(modifyList(inputs, list(N = N, treatA = TRUE, treatB = FALSE, n.capacity = cc)), S5)
  emit("A","match", N, ifelse(is.infinite(cc),"Inf",cc),
       sprintf("%.1f",o$deaths), sprintf("%.2f",o$s2py), sprintf("%.4f",o$mean_wait),
       sprintf("%.4f",o$dqaly), sprintf("%.0f",o$dcost), sprintf("%.4f",o$dqaly-soc$dqaly), sprintf("%.0f",o$dcost-soc$dcost))
  cat(sprintf("  c=%-4s deaths=%.1f S2py=%.1f wait=%.4f dqaly=%.4f dcost=%.0f  vsSoC dqaly=%+.4f dcost=%+.0f\n",
              ifelse(is.infinite(cc),"Inf",cc), o$deaths, o$s2py, o$mean_wait, o$dqaly, o$dcost, o$dqaly-soc$dqaly, o$dcost-soc$dcost))
}
emit("A","soc_ref", N, "NA", sprintf("%.1f",soc$deaths), sprintf("%.2f",soc$s2py), "NA", sprintf("%.4f",soc$dqaly), sprintf("%.0f",soc$dcost),"NA","NA")

cat("\n################ MODEL A DONE ################\n")
