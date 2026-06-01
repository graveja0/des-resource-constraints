###############################################################################
# crn.R — per-patient Common Random Number (CRN) banks
#
# Adapts the RIPS_CVD_microsim CRN design to the simmer next-event engine.
#
# THE PROBLEM (why naive same-seed CRN fails here)
# ------------------------------------------------
# Every event file calls rexp()/runif() inline, drawing from R's single GLOBAL
# RNG stream. simmer interleaves all arrivals on that one shared stream. The
# moment one patient takes a different branch under a different strategy (an
# extra false-positive attribute, a treatment that changes event timing), the
# count of draws consumed diverges — and from that point EVERY later patient
# reads a shifted stream. Streams desync on the first divergent patient.
#
# THE FIX (per-patient private streams, consumed by inverse-CDF)
# --------------------------------------------------------------
#   - Each patient gets a private "bank" of pre-drawn uniforms, one vector per
#     event type ("death", "sick1", ...), keyed by the patient's simmer name.
#   - The bank is drawn ONCE at patient entry from a deterministic per-patient
#     seed (CRN$seed + patient index). crn_init() SAVES and RESTORES the global
#     .Random.seed around its draws, so building a bank never advances the
#     global stream — patient i's bank cannot shift patient j's.
#   - Event functions call next_u("death") etc. to pull the next uniform from
#     the current patient's bank, then invert it: qexp(u, rate) instead of
#     rexp(1, rate); u < p instead of runif(1) < p. The uniform a patient sees
#     for a given (event, draw index) is IDENTICAL across strategies; only the
#     rate/threshold changes. So untreated patients match exactly across arms
#     and cancel in the paired difference, collapsing Monte-Carlo variance.
#
# Lesson from RIPS test.Rmd: their sliding-window index (pos = indiv + cycle)
# trades higher variance for lower memory because a 40-cycle × huge-N microsim
# is memory-bound. We are NOT — N is in the thousands — so we take the cleaner
# full-per-patient bank (the lower-variance "matrix" version they had to give
# up).
###############################################################################

CRN <- new.env(parent = emptyenv())
CRN$bank <- list()        # name -> list(ctr = <env>, u = list(stream -> numeric))
CRN$seed <- 12345L
CRN$size <- 500L          # uniforms per stream per patient (rollback caps ~100 events)

# Sticky kill-switch (NOT cleared by crn_reset). Set TRUE to force every draw
# onto the plain global-RNG fallback even when banks exist — used by the A-vs-B
# replication harness, which needs independent (non-CRN) runs because shared
# banks cannot align two models with different event-loop cadences.
CRN$force_disabled <- FALSE

# Streams: one per stochastic decision in the screening models.
CRN$streams <- c("death", "sick1", "healthy", "sick2",
                 "screen", "screen_time", "confirm", "trtdur", "age")

# Reset before every run; pass the run's master seed here.
crn_reset <- function(seed = 12345L) {
  CRN$bank <- list()
  CRN$seed <- as.integer(seed)
  invisible(NULL)
}

# Patient index from simmer name ("patient0" -> 0, "patient41" -> 41).
crn_index <- function(name) {
  as.integer(gsub("[^0-9]", "", name))
}

# Build the current patient's bank. Call ONCE as the first step of
# initialize_patient (so the bank exists before assign_events samples times).
# Saves/restores the global RNG so this never advances the shared stream.
crn_init <- function(name) {
  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
           get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(CRN$seed + crn_index(name) + 1L)

  b <- list(ctr = new.env(parent = emptyenv()), u = vector("list", length(CRN$streams)))
  names(b$u) <- CRN$streams
  for (s in CRN$streams) {
    b$u[[s]] <- runif(CRN$size)
    assign(s, 0L, envir = b$ctr)
  }
  CRN$bank[[name]] <- b

  if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
  invisible(0)
}

# Pull the next uniform from the current patient's stream. `env` is the simmer
# environment in scope inside event functions (get_name(env) is the patient).
next_u <- function(stream) {
  b <- CRN$bank[[get_name(env)]]
  k <- get(stream, envir = b$ctr) + 1L
  if (k > length(b$u[[stream]]))
    stop(sprintf("CRN stream '%s' exhausted (>%d draws) for %s; raise CRN$size",
                 stream, CRN$size, get_name(env)))
  assign(stream, k, envir = b$ctr)
  b$u[[stream]][k]
}

# ---------------------------------------------------------------------------
# Fallback-aware draw helpers (Option A)
# ---------------------------------------------------------------------------
# The shared event files call these instead of rexp()/runif() directly. CRN is
# "armed" automatically for a patient iff crn_init() created a bank for them.
#   - Armed (models 7-11): inverse-CDF on the patient's private banked stream.
#   - Unarmed (models 1-6, which never call crn_init): fall back to the exact
#     same rexp(1, rate) / runif(1) calls as before -> bit-identical results,
#     identical global-RNG consumption. So models 1-6 are unaffected.
crn_active <- function() !isTRUE(CRN$force_disabled) && !is.null(CRN$bank[[get_name(env)]])

# Inverse-CDF exponential draw. qexp(u, rate) = -log(1 - u)/rate ~ Exp(rate).
draw_exp <- function(stream, rate) {
  if (crn_active()) qexp(next_u(stream), rate) else rexp(1, rate)
}

# Uniform draw on the named stream (for screen sens/spec thresholds, etc.).
draw_unif <- function(stream) {
  if (crn_active()) next_u(stream) else runif(1)
}
