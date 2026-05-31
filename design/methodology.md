# Modeling resource contention in the simmer sick-sicker DES — methodological agenda

**Status:** plan / research synthesis. Author: drafted for John Graves & Shawn Garbett, May 2026.
**Scope:** how to faithfully model a *finite number of treatment spots* (capacity `c` ≪ demand)
in the LMIC sick-sicker cost-effectiveness model, so that **disease keeps progressing while a
patient waits** for a contended resource and the patient continues normally afterward.

All claims below were verified empirically against the actual code in
`spgarbet/sick_sicker_des` on **R 4.5.2 / simmer 4.4.7** (the version installed locally). Where a
number appears, it came from a probe that was run, then independently re-run by an adversarial
verifier.

---

## 1. The problem, precisely

The model (`main_loop.R`) is a **hand-rolled next-event competing-risks engine**: each patient is
one simmer arrival; an `event_registry` holds, per event, a `time_to_event()` (samples a time, e.g.
`rexp`), a `func()` (trajectory modifier), and a `reactive` flag; `process_events()` does
`timeout()` to the soonest sampled event, `branch()`es on which fired, runs its `func`, reschedules;
`des()` wraps the whole loop in `branch(function() 1, continue=TRUE, …) |> rollback(1, 100)`.

In this design **simmer is only a timeout/attribute/tally engine.** The "resources"
(`healthy`, `sick1`, `sick2`, `time_in_model`, `death`) are `capacity = Inf` accounting tallies used
to integrate time-in-state for costing. **Competing risks are not simmer resource races** — they are
the `min()` of sampled event times.

A *genuinely finite* treatment resource (`capacity = c < N`) forces simmer's **native blocking
`seize()`**. When the servers are full, `seize()` blocks the whole arrival in the resource's queue —
which **freezes that patient's hand-rolled event clock.** While queued the patient cannot die,
progress, or recover. That is the `model-8` failure documented in `Sick_Sicker_Simmer_Constrained.qmd`:
restricting treatment *prevented* simulated deaths instead of causing them. The two paradigms —
hand-rolled next-event vs. simmer-native blocking — **do not compose.**

"Faithful" therefore requires: (i) the wait is **endogenous** (an individual waits longer when more
people compete for the `c` slots), (ii) **competing risks keep racing during the wait**, (iii) the
capacity-`c` cap is **actually enforced**, (iv) the patient **re-enters the event loop cleanly**, and
(v) **no resource leaks / double-release.**

---

## 2. Why the existing attempts fail (verified, not theorized)

We reproduced both reported failures verbatim and isolated their causes on simmer 4.4.7.

### `clone()` + `synchronize()` (`model12.R` / `become-sick-cloned.R`)

- **`synchronize(wait = FALSE)` → "not previously seized" crash.** Each clone is a *distinct arrival
  with its own seize ledger.* A resource seized **before** `clone()` can be released by **only one**
  clone; the second clone's release underflows the server count and simmer aborts
  (`'time_in_model' not previously seized`, plus `'patientX': leaving without releasing 'healthy'`).
  Because both clones in `become_sick` end up touching the shared state tallies
  (`healthy`/`sick1`/…), this fires essentially every run, at an RNG-dependent time — hence "intermittent."
- **`synchronize(wait = TRUE)` → "winner jettisoned."** This is **not** an intrinsic property of
  `synchronize` (we proved the survivor *does* re-enter an enclosing `rollback` loop). It is a
  **deadlock**: `wait = TRUE` blocks the survivor until *every* sibling reaches the sync point, and a
  sibling parked in `wait()` on a per-patient signal that never reliably fires **never arrives** — so
  the survivor hangs forever and experiences no further events. That is Shawn's "thin line": `wait=FALSE`
  double-releases shared tallies; `wait=TRUE` deadlocks. Neither is debuggable away — they are
  consequences of how clones, ledgers, and synchronize actually work.
- A discarded clone's still-held resources are **not** auto-released or transferred → **leaked** (the
  server count stays elevated and the survivor cannot release them).

### `renege_if` on a global signal (`model11.R`, constrained `.qmd`)

- `send()` is a **global broadcast**, not addressable to one patient; per-patient targeting is faked by
  baking the id into the signal string — fragile, and a mismatch silently drops the signal.
- An arrival **blocked in `seize()` ignores `trap()`/`wait()`** entirely (verified). Only
  `renege_if`/`renege_in` can pull an arrival out of a `seize` queue. So the patient stuck in the queue
  cannot fire its own competing events, and the global signal can't reliably reach just that patient.

### `rollback` × clone interaction

`rollback(target, times)` counts **trajectory activity nodes**, not runtime events. `clone` and
`synchronize` are two extra nodes and they re-inject the survivor — which silently shifts integer
targets and breaks the `times` termination counter (a nested clone+synchronize test **ignored
`times=3` and looped ~5.77M times to the horizon**). **Mitigation if clones are ever used:** use
*tag-named* rollback targets, keep `clone/synchronize` strictly inside the `branch` wrapper, prefer a
`check=` predicate over `times=`.

**Conclusion:** a faithful fix should **not** use `clone()` on the state tallies. The architectural
incompatibility (Inf-capacity per-patient tallies + a single-arrival `rollback(1,100)` loop vs. clones
that multiply arrivals sharing attributes but with independent single-redemption ledgers) is
fundamental, not incidental.

---

## 3. The load-bearing simmer facts (4.4.7)

| Primitive | What it actually does | Consequence for us |
|---|---|---|
| `seize()` on full resource | blocks the arrival; its hand-rolled clock freezes | the core incompatibility; never block the *patient* arrival |
| `trap()`/`wait()` | ignored while blocked in `seize()`; `send()` is **global** | can't rescue a queued patient; per-patient signals are string-encoded |
| `renege_if`/`renege_in` | **fire even while blocked in `seize()`**; `keep_seized` defaults **FALSE** (auto-release) | the *only* lever to pull an arrival out of a queue; set `keep_seized` deliberately |
| `clone()` | distinct arrivals, **independent seize ledgers** | pre-clone seizes can be released once → double-release crash |
| `synchronize(wait=F/T)` | F: first survives; T: waits for all, last survives; discarded clones' resources **leak** | survivor re-enters loops fine; `wait=TRUE` deadlocks on a parked sibling |
| `get_server_count`/`get_queue_count`/`get_capacity`/`get_seized` | **callable mid-trajectory and inside `time_to_event`**, read live occupancy | enables an **endogenous wait with no blocking** |
| `rollback(int, times)` | counts activity nodes; clones shift the count | use tag targets if clones are present |

---

## 4. The design space — five approaches, tested

| # | Approach | Contention | Competing risks during wait | Leaks | Verdict |
|---|---|---|---|---|---|
| naive | blocking `seize()` in the loop (`model-8`/`model11`) | exact | **frozen** (patient blocked) | — | **broken** — destroys face validity |
| **A** | **Endogenous-wait registry event** (live `get_server_count` → wait) | capacity **exact**; wait-time **approx** | **yes** | none | **VIABLE — most robust** |
| **B** | **Claim-ticket companion** (patient never blocks; a separate arrival seizes the real queue + signals back) | **exact FIFO queue** | **yes** | none | **VIABLE — exact, more fragile** |
| C | renege competing-risks (fold risks into one renege timer, race vs. seize) | degenerate | yes | **slots never released** | **broken as shipped** |
| D | manual global slot calendar | endogenous direction only | yes | **cap not enforced** | **broken as shipped** |

### A — Endogenous-wait-as-event (recommended production model)

Keep everything in the hand-rolled loop. The patient **never** does a blocking `seize` of the
contended resource. Add a `"Get Treatment A"` registry event whose `time_to_event()` reads **live
occupancy** of a real capacity-`c` resource and the patient's FIFO position, and converts congestion
into a wait (≈instant if a slot is free; otherwise an M/M/c-flavoured `rexp(rate = c·μ / n_ahead)`).
When the event fires, a **fire-time guard** does a real `seize` *only if a slot is genuinely free*
(else re-defer) — so the cap is enforced exactly with no blocking. A timed `EndA` event releases the slot.

- **Tested:** mean wait falls monotonically as `c` grows (1.03→0.81→0.46→0.21→0.007 for c=1,2,4,8,16,
  10-seed mean) and rises with `N` (0.21→1.33 for N=25→400) — genuine endogeneity, confirmed by an
  ablation that strips the occupancy term and flattens the gradient. Capacity **exact**: 0 over-capacity
  and 0 negative-count instants across 6,350 monitored rows. Competing risks race: 56/91 S1-entrants
  never got a slot and all died; deaths/S2-time rise under scarcity (the right direction).
- **Honest caveat:** the **wait *distribution*** is an approximation — correct *first moment under
  saturation* but a single Exp (over-dispersed, CV=1 vs. Erlang's 1/√n), assumes all servers stay busy
  (under-estimates light-traffic wait), and ignores that waiters ahead may abandon (over-estimates
  `n_ahead`). For **mean-based CEA endpoints (QALYs, costs) this is largely benign**; for
  tail/variance-sensitive endpoints, less so.
- **Fragile points to preserve on retrofit:** the global FIFO waitlist lives *outside* simmer — every
  exit path (recover/progress/die/horizon-cleanup) must remove the patient from it; the fire-time guard
  is **not optional**; the contended resource must **not** be in the Inf-capacity `counters` list.
- **Why it wins:** no `clone`/`synchronize`/`send`/`trap` anywhere → none of the fragility in §2. It is a
  small, teachable extension of `model10` (replace the exogenous `waitA_days` with an occupancy-derived
  wait + a live-occupancy guard).

### B — Claim-ticket companion (recommended *exact* reference / showcase)

"`model12` done right." The patient lives **only** in the loop and touches **only** tallies. On
becoming sick it `clone(n=2)`: clone-1 (`self`, empty) survives via `synchronize(wait=FALSE)` and
re-enters the loop; clone-2 (`companion`) is a lightweight ticket whose **only** job is to `seize` the
**real** finite resource (blocking in *that* queue is fine — it carries no tallies). On acquisition the
companion `send`s a **per-patient** signal; the patient `trap`s it and fires its reactive
"treatment-acquired" registry event. Teardown: leaving S1 `send`s an abandon signal → the companion
`renege_if`s out of the queue. **Patient and companion never share a resource → no double-release.**

- **Tested:** the queue is **exact simmer FIFO** — server never exceeds `c`, mean wait monotone in `1/c`
  *and* `N` (0.65y@N=20 → 5.52y@N=400 at cap=3). Face validity smoothly interpolates from full-treatment
  to no-treatment as `c` shrinks. **Null-effect regression** (set progression/cure factors to 1) gives
  statistically identical outcomes across all `c` — the decisive proof nothing is silently frozen. At
  c=1, 83/90 companions are abandoned mid-queue because the patient died/progressed/recovered while
  waiting. No leaks (server returns to 0 past the horizon).
- **Honest caveats:** (1) the **treatment *effect* window is decoupled from the slot-hold duration** —
  `onTrt` clears on leaving S1, but the companion frees the slot at `treatment_duration`; ~4/35 treated
  patients kept the treated rate >3y after their slot freed. This is a defensible modeling choice but
  **must be documented in the CEA.** (2) Do **not** count `treatment` per-resource arrivals as
  `n_treated` (67/106 are reneged companions, `activity_time = 0`) — use the held tally.
- **Load-bearing invariants (violate any → silent breakage, *no error*):** contended resource not in
  `counters`; the state change is routed through the *registry event*, not the `trap` handler; `aTrt`
  forced to `now(env)`; `synchronize(wait=FALSE)`; per-patient signal names. **Ship with the null-effect
  regression test as a permanent guard.**

### C and D — instructive but broken as shipped (good cautionary tales)

- **C (renege competing-risks):** simmer allows only **one** active renege timer (a second
  *overwrites*), so all risks must be hand-folded into one `min()` timer. Clever, but adversarial testing
  found the **treatment slot is acquired and never released** (45 seize / 0 release at c=5) → "fill once,
  then starve," not a turnover queue; the "no leak" claim was hollow (frozen gauge); accounting off 4.3×.
- **D (manual slot calendar):** promising and free of clone pathology, but the per-slot scalars hold
  one booking while `slot_free` chains many → **capacity is not enforced** (a "cap=5" run carried up to
  **11** concurrent treatments, >5 for 54% of the timeline). This *under*-states waits / *over*-states
  throughput — the **dangerous** direction for a scarce-resource LMIC conclusion. Its 4/4 "exactness"
  unit tests passed only because none exercised the failing case. Salvageable with a per-slot booking
  *ledger* (list of `[start,end]`), but not as written.

---

## 5. Recommended way forward

1. **Production model → A (endogenous-wait event).** Robust, reuses `model10`'s entire flag/registry/CEA
   machinery, zero clone/signal fragility. **Frame it honestly:** *capacity enforced exactly, wait-time an
   analytic approximation with the correct mean under saturation.*
2. **Exact reference + teaching showcase → B (claim-ticket companion).** This is the model that actually
   demonstrates simmer's native resource contention (real FIFO `seize` queue + `renege` abandonment), and
   it is the **ground truth** for validating A.
3. **Cross-validate A against B** at matched `c, N, μ`. B's queue is exact; A's is approximate. If they
   agree on CEA endpoints (deaths, S2-time, dQALY, mean wait) across the regimes of interest — which they
   should, since both have the correct mean — you have both (a) license to ship the cheap, robust A, and
   (b) a genuine **methods contribution**: *"an analytic endogenous-wait approximation reproduces an exact
   DES queue for mean-based cost-effectiveness endpoints."*
4. **Retire** `model11`/`model12` from the teaching path except as documented cautionary examples (§2).
   **Do not** ship C or D without the fixes in §4.

---

## 6. Validation ladder (apply to A and B)

1. **Analytic M/M/c special case** — disable disease dynamics; compare mean wait to Erlang-C. (Tests A's
   approximation directly; B should match closely.)
2. **Unconstrained limit** — `c = Inf` must reproduce `model10`'s unconstrained-treatment results.
3. **Null treatment effect** — progression/cure factors = 1 must reproduce no-treatment outcomes across
   *all* `c` (catches B's two silent-failure modes; keep as a regression test).
4. **Face validity** — deaths and person-years in S2 monotone in `1/c`.
5. **Leak / accounting audit** — server returns to 0 past the horizon; tallies net to 0; for B,
   `n_treated` from the held tally (not per-resource arrival rows); resource-time integral == arrivals sum.
6. **Endogeneity** — mean wait rises as `c` shrinks and as `N` grows (10-seed means to kill MC noise).
7. **A-vs-B agreement** — the cross-validation in §5.3 on the full CEA table.

---

## 7. Implementation roadmap (retrofit onto `model10.R` / `main_loop.R`)

**Model A**
- Add a real `add_resource("A", capacity = c)` — **not** `Inf`, and **not** in the `counters` list.
- Add a global FIFO waitlist (an R environment keyed by `get_name(env)`); reset it in `des_run`.
- Add `"Get Treatment A"` (reactive) with the endogenous `years_till_treatmentA()` (live
  `get_server_count`/`get_capacity` + FIFO position) and a `get_treatmentA()` `func` with the fire-time
  **free-slot guard**; add a timed `"EndA"` event that releases `A`.
- Gate `years_till_sick2()` on the on-treatment flag (progression halts/slows while treated).
- Ensure `healthy`/`sick2`/`death`/`cleanup_on_termination` all `release("A")` **and** `wl_leave()`.
- Expose `c`, `μ_A`, the rate form, and `rate_admit_when_free` as **named, documented assumptions.**

**Model B**
- Add the contended `treatment` resource (capacity `c`, **not** in `counters`) and a `treatment_held`
  Inf tally for costing.
- In `sick1()`: `trap(acq_<pid>)` → set `trtReady`, force `aTrt = now()`; then `clone(n=2, self,
  companion)` `|> synchronize(wait=FALSE)`. The companion: `renege_if(abandon_<pid>) |> seize |>
  renege_abort |> send(acq_<pid>) |> timeout(dur) |> release`.
- Add the `"Treatment acquired"` reactive event (`years_till_trt`/`treatment_acquired`) that sets
  `onTrt` and seizes `treatment_held`; add `abandon_treatment()` (send abandon, clear flags, release held).
- Route cost/QALY through `treatment_held` exactly as `model10` does for resource A.
- **Document** the effect-duration-vs-slot-occupancy decoupling; add the null-effect regression test.

---

## 8. Teaching narrative for the Oxford workshop

A clean arc that matches the curriculum's "contention is what DES adds over Markov/microsim":

`model-8` (naive blocking `seize` → patients frozen, deaths spuriously drop) → `model10` (exogenous
wait — looks like contention but each patient's wait is independent) → **the two clone traps**
(`model11`/`model12`: global signals, `trap` ignored while queued, clone ledger double-release,
`synchronize(wait=TRUE)` deadlock — *why* it's a thin line) → **the fix:** either keep it in the event
loop with an endogenous-occupancy wait (**A**), or decouple the queue into a claim-ticket companion
(**B**). Use **C** and **D** as "tempting traps" with the specific bugs adversarial testing surfaced —
they make excellent "spot the bug" exercises.

---

## 9. Open decisions for John & Shawn

1. **Which is the production model — A or B?** Recommendation: **A** as production (robustness), **B** as
   the exact reference that validates it. (Could also ship B as production if exact per-person waits are a
   headline output; it costs the fragility in §4.)
2. **Is the wait *distribution* (not just its mean) an output you report?** If yes, A's single-Exp
   approximation needs upgrading (Erlang draw, or an event-driven "slot-freed" recompute) — or use B.
3. **How is treatment *effect duration* defined** relative to slot occupancy (matters for B and for the
   costing in both)?
4. **Scale:** the constrained `.qmd` notes simmer DES is ~O(n log n) and impractical beyond N≈100k.
   For the contended case, the relevant N is "the number competing for the finite resource" — confirm the
   LMIC case sits within budget, or plan the ACCRE batching already referenced.
