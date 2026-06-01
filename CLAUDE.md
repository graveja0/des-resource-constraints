# CLAUDE.md

Context for working on this project. Read it first.

> **Note on the Dropbox-level CLAUDE.md.** A `~/Dropbox/CLAUDE.md` exists that
> documents an unrelated soccer-tactics project. It does **not** apply here.
> This file is the source of truth for `Projects/des-resource-constraints/`.

---

## What this project is

A home for **discrete-event simulation (DES) teaching material on resource
contention / capacity constraints in health-economic modeling**, built around
the Sick-Sicker model. There are **two deliverables plus an archived
precursor**, all drawing on a **single shared simulation engine in `R/`**:

1. **`manuscript/`** — the tutorial (the project's main, active work): a
   fully-executable **Quarto explainer**, *Discrete-Event Simulation for
   Cost-Effectiveness Analysis with Resource Contention.* It walks the
   Sick-Sicker model from `model-1` up to two correct contention designs
   (**Approach A** and **Approach B**) and compares their tradeoffs.
   Renders to HTML + PDF.
2. **`slides/`** — a **revealjs** talk built *from* the manuscript, reusing the
   same `R/` engine and `figures/`. (Currently a scaffold mirroring the
   manuscript's arc — extend it as the talk develops.)
3. **`workshop-2025/`** — archived materials from the **July 2025 University of
   Oxford HERC workshop**, *"Discrete Event Simulation Modeling with Queues and
   Resource Constraints: An Introduction."* A self-contained Quarto **website**
   (`type: website`) with a 4-part lecture progression — the precursor the
   manuscript grew out of. Kept as a **known-good snapshot**; it keeps its own
   engine copy under `workshop-2025/R/` (intentionally *not* unified with the
   shared `R/`).

---

## The shared modeling substrate

Both sub-projects teach the **Sick-Sicker** progressive-disease CEA model
implemented in `simmer`, but with a deliberate twist that is the whole
intellectual core of this project:

- `main_loop.R` is a **hand-rolled next-event competing-risks engine**. Each
  patient is one `simmer` arrival. An `event_registry` holds, per event, a
  `time_to_event()` (samples a time, e.g. `rexp`), a `func()` (trajectory
  modifier), and a `reactive` flag. Competing risks are resolved as the
  `min()` of sampled event times — **not** as `simmer` resource races.
- In this design **`simmer` is only a timeout / attribute / tally engine.**
  The "resources" (`healthy`, `sick1`, `sick2`, `time_in_model`, `death`) are
  `capacity = Inf` accounting tallies used to integrate time-in-state for
  costing.
- **The central problem.** A *genuinely finite* treatment resource
  (`capacity = c < N`) forces `simmer`'s native blocking `seize()`. When
  servers are full, `seize()` blocks the whole arrival in the queue — which
  **freezes that patient's hand-rolled event clock** (they can't die,
  progress, or recover while queued). The two paradigms — hand-rolled
  next-event vs. `simmer`-native blocking — **do not compose**. This is the
  `model-8` "freeze trap."
- **The shared invariant any correct fix must honor:** *the patient must never
  block; disease must keep progressing during the wait; the capacity-`c` cap
  must actually be enforced; no resource leaks / double-release.*
- **Approach A** (`model-A.R`): endogenous-wait registry event — a real
  capacity-`c` resource + a fire-time free-slot guard; the wait is computed
  analytically from live occupancy. Selling point: adds contention **without
  touching the decade-stable `main_loop.R` engine**.
- **Approach B** (`model-B.R`): claim-ticket companion — a companion
  trajectory holds a real FIFO `seize()` queue with per-patient `send`/`trap`
  + `renege`. Selling point: the **exact** emergent queue, at the cost of
  fragility.

If you change anything about contention mechanics, read **`design/methodology.md`**
first — it is the verified design doc (all claims checked against
`spgarbet/sick_sicker_des` on R 4.5.2 / simmer 4.4.7) and **`design/PLAN.md`** —
the section-by-section build spec for the manuscript `.qmd`.

---

## Model ladder

Each model file introduces exactly one new idea. The progression has two phases:
**build the CEA spine** (1–6), **introduce screening and resource contention** (7–11).

The screening arc models a cancer with a long pre-clinical phase (S1 = screen-detectable
early disease; S2 = symptomatic/advanced). The intervention is a screening program with
two competing tests: a high-sensitivity molecular test (expensive, low coverage) vs. a
cheaper field test (lower sensitivity, but primary-care deliverable at much higher coverage).
The resource constraint is **confirmatory diagnostic capacity** — the bottleneck that the
field test's high referral volume (driven by both true and **false** positives) overwhelms.

Each patient is screened **once**, at a CRN-drawn time `~Uniform(t.screen.start, t.screen.end)`
over a rollout window (staggered, not a synchronized burst — realistic and keeps the
confirm queue near steady state). Detection uses `cov`/`sens`/`spec` per test: a true
positive needs `u_tp < cov·sens` (in S1), a false positive `u_fp < cov·(1−spec)` (in H).
All parameters live in `inputs2.R`; `r.S1H = 0.05` (cancer S1 rarely resolves) and
`c.TrtA = 500` (cheap generic treatment) are the calibration levers that put the
field-vs-molecular comparison in the SE quadrant.

**Common Random Numbers (`R/crn.R`).** Every patient draws a private bank of uniforms from
a deterministic per-patient seed; the shared event files consume it by inverse-CDF via
`draw_exp()`/`draw_unif()` (fallback to plain `rexp`/`runif` when unarmed, so models 1–6 and
`main_loop.R` are untouched and bit-identical). This crushes the mol-vs-field variance ~80×.
CRN aligns mol-vs-field **within** a model; it cannot align A vs B across models (their
confirm-event cadences differ), so A=B is validated by **replication** with CRN disabled
(`validation/validate-AB-replication.R`), not shared seeds.

| Model | What it introduces |
|-------|--------------------|
| **model-1** | Bare DES skeleton. One counter (`time_in_model`). One registry event: `Terminate at time horizon`. No disease, no costing. |
| **model-2** | Sources `discount.R`. Cost side is a stub (returns zero). **QALY side is live**: `qaly_arrivals()` already computes discounted time-in-model QALYs. |
| **model-3** | Adds `event_death.R` + `death` counter. Constant-rate exponential mortality; no state-dependent hazard multipliers yet. |
| **model-4** | Switches to `event_death2.R` (applies `hr.S1D` in S1; no S2 multiplier yet). Adds `event_sick1.R` + `event_healthy.R`; counters `healthy`, `sick1`. **Full H/S1 costs and utilities with discounting are already present here** (`c.H`, `c.S1`, `u.H`, `u.S1`). |
| **model-5** | Same sources and counters as model-4. **Adds the `Healthy` return event to the registry** — the H↔S1 round-trip now works. Costing was already in model-4; nothing new there. |
| **model-6** | Switches to `event_death3.R` (adds `hr.S2D`). Adds `event_sick2.R` + `sick2` counter. S2 costs and utilities. Full three-state Sick-Sicker natural history. |
| **model-7** | **Molecular screening, capacity = ∞.** New `event_screen.R`: one-time staggered screen; true positives are confirmed and treated immediately (`TreatA=1` → `hr.TrtS1S2` cuts S1→S2; `treated_s1` counter replaces `sick1`, utility `u.TrtA`). One-time screen + confirm costs stored as attributes, folded in by `add_attr_costs()`. Everyone positive is treated at once — no queue. |
| **model-8** | **Field test comparison, capacity = ∞.** Two strategies side-by-side (`mol` vs `field`). Field's coverage expansion dominates its sensitivity loss → **SE quadrant** (lower total cost, more total QALYs). False positives appear here (they consume confirm costs). |
| **model-9** | **Exogenous queue.** A confirmation wait drawn from a lognormal (`confirm_wait_logmean/logsd`, mean ~18 wks) is inserted between positive screen and treatment; disease progresses during the wait (S1 can advance to S2, losing the benefit). A "Confirm" registry event fires at the stored `tConfirm`. |
| **model10** | **Endogenous queue — approach A.** Real finite-capacity `confirm` resource + global FIFO waitlist. Wait computed analytically from live `get_server_count`/`get_capacity` (M/M/c-flavored); a fire-time guard (`get_confirm`) seizes only when a slot is genuinely free, so the patient never blocks. `main_loop.R` untouched. Honest caveat: the wait *distribution* is a single-Exp approximation. |
| **model11** | **Claim-ticket companion — approach B. Most defensible.** On a positive screen the patient `clone(n=2, self, companion)`s; the companion holds the real blocking `seize("confirm")` FIFO queue and `send()`s an `acq_<pid>` signal (keyed on an inherited `pid`) that fires a "Confirm acquired" registry event so reactive resampling re-draws at treated rates. Exact emergent FIFO queue. A=B on mean endpoints is checked by replication (see CRN note above). |

**Files deleted in the model-7+ rewrites** (kept for reference only):
- Old treatment-arc `model-A.R`, `model-B.R`, `model12.R`
- `event_sick1-v2.R`, `event_healthy-v2.R`, `become-sick-cloned.R`, old `inputs2.R`

---

## Layout

```
des-resource-constraints/
├── CLAUDE.md                    this file
├── README.md                    orientation + render instructions
├── _quarto.yml                  PROJECT config: execute-dir: project; renders manuscript/ + slides/
├── des-resource-constraints.Rproj   anchors here() at the repo root
├── references.bib               single shared bibliography
│
├── R/                           ── single source of truth for all code ──
│   ├── main_loop.R              the hand-rolled next-event engine (black box; unchanged)
│   ├── inputs.R  inputs2.R      parameter sheets
│   ├── discount.R               continuous-time discounting helper
│   ├── cea-table-functions.R    ICER / CEA-table helpers
│   ├── event_*.R                one transition per file (death, sick1, healthy, sick2, …)
│   ├── model-1.R … model-9.R    natural-history + naive-contention progression (one idea each)
│   ├── model10.R                endogenous-wait registry event (seed of Approach A)
│   ├── model11.R  model12.R     cautionary traps (renege; clone/sync) — shown eval:false
│   ├── model-A.R  model-B.R     the two correct contention designs (~750 LOC each, validated)
│   └── become-sick-cloned.R     model12 helper
│
├── figures/                     shared static figure assets (Petri-net, car-race, diagrams)
├── manuscript/                  DELIVERABLE 1 — the tutorial
│   ├── resource-contention.qmd  the explainer (renders to HTML + PDF)
│   └── _metadata.yml            format overrides (html+pdf); NOT a here() root marker
├── slides/                      DELIVERABLE 2 — the revealjs talk
│   ├── resource-contention-slides.qmd
│   └── _metadata.yml            revealjs format override
├── workshop-2025/               ARCHIVED July-2025 Oxford HERC workshop (own Quarto WEBSITE)
│   ├── _quarto.yml              type: website, html only, freeze: true
│   ├── content/                 01–04 lecture parts + images
│   └── R/                       its OWN frozen engine copy (intentionally not unified)
├── design/                      working/design docs — NOT deliverables
│   ├── methodology.md           verified A/B methods + simmer-semantics design doc
│   └── PLAN.md                  section-by-section build plan for the manuscript
├── validation/                  harnesses that reproduce the manuscript's numbers
│   └── val-harness-{A,B}.R  cea-icer-harness.R  probe-{A,B}-*.R  validate-A.R
└── _freeze/                     Quarto freeze cache (project-level) — COMMIT THIS
    └── manuscript/resource-contention/   html.json + tex.json (per-format)
```

### How paths resolve (the load-bearing design)

The **repo root is a single Quarto project**. `_quarto.yml` sets
`execute-dir: project`, so every chunk runs with **CWD = repo root**.
The deliverables use a **`_metadata.yml`** (not a `_quarto.yml`) for their
format overrides — a `_metadata.yml` is *not* a `here()`/project root marker,
so `here::here()` resolves to the repo root (which has `_quarto.yml` + `.Rproj`
+ `.git`). This is deliberate: a `_quarto.yml` inside `manuscript/` would make
`here()` anchor *there* and break `here("R/…")`.

- Models are sourced as **`source(here::here("R/model-N.R"), chdir = TRUE)`**.
  The `chdir = TRUE` is essential: each `model-N.R` does its own bare
  `source('inputs.R')`, which only resolves while CWD is temporarily `R/`.
- Models 1–9 keep their **bare** internal `source()` paths (close to Shawn's
  upstream, for easy syncing); `model10`/`model12`/`model-A`/`model-B` use
  `here('R/…')`. Both resolve to `R/`.
- Markdown image paths in `manuscript/` use **`../figures/…`** (relative to the
  document, not affected by `execute-dir`).

### Intentional duplication (do NOT unify without asking)

- `workshop-2025/R/` keeps its **own engine copy**, deliberately frozen. The
  shared engine in `R/` is byte-identical to it *except* `cea-table-functions`
  (the workshop's is the older, larger July-2025 version). The workshop is a
  delivered, archived artifact — leave it self-contained.
- Some figures appear in both `figures/` and `workshop-2025/content/images/` —
  also intentional; the workshop is self-contained.

---

## How to render

```bash
quarto render                                           # whole project: manuscript + slides
quarto render manuscript/resource-contention.qmd --to html   # fast iteration
quarto render manuscript/resource-contention.qmd --to pdf    # system LaTeX (TeX Live at /Library/TeX/texbin)
quarto render slides/resource-contention-slides.qmd
cd workshop-2025 && quarto render                       # the archived website (its own project)
```

PDF works with system LaTeX — no `tinytex` needed (on a LaTeX-less machine:
`quarto install tinytex` once). Full manuscript render is ~5 min; freeze caches
it. Rendered `.html`/`.pdf`/`_files/` are **gitignored**; `_freeze/` is the
committed reproducibility artifact.

---

## ⚠️ The freeze gotcha (read before editing any `R/*.R` file)

`execute.freeze` re-runs a chunk only when the **`.qmd` text** changes — it
does **not** notice edits to the `source()`d files in `R/`. There is **no
`--no-freeze` flag**. After editing any `R/model-*.R`, `R/inputs.R`,
`R/main_loop.R`, etc., clear the project-level cache so chunks re-execute:

```bash
rm -rf _freeze && quarto render manuscript/resource-contention.qmd
```

(Because this is a Quarto *project*, the freeze cache is at the **repo-root
`_freeze/`**, not inside `manuscript/`.) Keep `set.seed()` in every stochastic
chunk so frozen output is reproducible and reviewer diffs aren't noisy.
**Commit `_freeze/`.**

---

## Working principles

1. **`design/methodology.md` and `design/PLAN.md` are the contract.** The
   contention mechanics, the A/B labeling, the "gallery of traps," and the
   section ordering are all specified there. Read them before changing model
   code or restructuring the doc.
2. **One new idea per model step.** The `model-1 … model-9 → A/B` progression
   deliberately introduces exactly one concept per file. Preserve that rhythm.
3. **model10 and model11 are the two correct contention designs.** Neither is "broken."
   Both should execute. The headline result: they agree on mean CEA endpoints within
   Monte-Carlo noise, differing only on the wait distribution.
4. **Don't touch `main_loop.R` lightly.** It's the decade-stable engine
   (treated as a black box). Approach A's whole value proposition is adding
   contention *without* modifying it.
5. **The headline result must hold:** A and B agree on mean CEA endpoints
   (deaths, S2-time, dQALY, dcost) within Monte-Carlo noise, differing only by
   construction on the wait distribution. If a change breaks that agreement,
   it's a bug — the harnesses in `validation/` (`val-harness-{A,B}.R`,
   `cea-icer-harness.R`) are how you check.
6. **Keep `N` modest in the doc** (200–1000, `replicate(20–50)`); quote
   production-`N` (1e4) numbers as prose. `model10`'s per-patient costing is
   the perf bottleneck.

## Environment

- R 4.5.2 / `simmer` 4.4.7 (the verified versions). `model-A.R`/`model-B.R`
  load `simmer` **last** and force it above `lubridate` on the search path so
  `simmer::now()` is never masked.
- Both `model-A.R` and `model-B.R` skip their experiment blocks when
  `source()`d (A via `MODEL_A_NORUN=1`, B via a `sys.nframe()` guard), so the
  `.qmd` can source them cheaply.

## Open / likely future work

- **Flesh out `slides/`** — currently a scaffold mirroring the manuscript's arc.
- Migrate `R/cea-table-functions.R` off `flextable` to `kableExtra`/`gt` for
  faithful PDF tables.
- Pre-render the bespoke A/B mechanism diagrams to committed `.svg` + `.pdf`.
- Raise demo `N` once the render budget allows.
- Possible DALY extension (Leech/Graves MDM 2025) — currently out of scope.
