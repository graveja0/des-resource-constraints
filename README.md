# resource-contention

A **self-contained** Quarto project that builds an incremental, fully-executable explainer:
*Discrete-Event Simulation for Cost-Effectiveness Analysis with Resource Contention.* It walks
the Sick-Sicker model from `model-1` up to two correct contention designs (**A** and **B**) and
compares their tradeoffs.

Everything needed to render lives in this directory — it can be lifted out into its own project
without changes (the `.Rproj` anchors `here()` here; all `source()` paths are relative).

## Layout

```
resource-contention/
├── resource-contention.qmd     the explainer (renders to HTML + PDF)
├── _quarto.yml                 dual-format config; execute.freeze: auto
├── resource-contention.Rproj   project anchor (makes here() root here; move-ready)
├── references.bib
├── methodology.md              the A/B methods + verified simmer semantics (design doc)
├── PLAN.md                     section-by-section build plan for the .qmd
│
├── main_loop.R                 the hand-rolled next-event engine (black box; unchanged)
├── inputs.R  inputs2.R         parameter sheets
├── discount.R                  continuous-time discounting helper
├── cea-table-functions.R       ICER / CEA-table helpers
├── event_*.R                   one transition per file (death, sick1, healthy, sick2, …)
├── model-1.R … model-9.R       the natural-history + naive-contention progression (one idea each)
├── model10.R                   endogenous-wait registry event (seed of Approach A)
├── model11.R  model12.R        cautionary traps (global-signal renege; clone/synchronize) — shown eval:false
├── become-sick-cloned.R        model12 helper
│
├── drafts/                     validated A/B probe code (NOT yet on the CEA spine — Phase 0 seed)
│   ├── probe-A-endogenous-wait.R
│   └── probe-B-claim-ticket.R
└── figures/                    Petri-net, car-race, model-diagram, etc.
```

## Render

```bash
cd resource-contention
quarto render resource-contention.qmd            # both formats
quarto render resource-contention.qmd --to html  # fast iteration
quarto render resource-contention.qmd --to pdf    # uses system LaTeX (TeX Live found at /Library/TeX/texbin)
```

PDF works with the existing system LaTeX — no `tinytex` install needed. On a LaTeX-less machine:
`quarto install tinytex` once.

## ⚠️ Freeze gotcha (read this)

`execute.freeze: auto` re-runs a chunk only when the **`.qmd` text** changes — it does **not** notice
edits to the `source()`d `.R` files. There is **no `--no-freeze` flag**; after editing any `model-*.R`,
`inputs.R`, etc., clear the cache first so the chunks re-execute:

```bash
rm -rf _freeze && quarto render resource-contention.qmd
```

Keep `set.seed()` in every stochastic chunk so frozen output is reproducible. Commit `_freeze/`.

## Status

- ✅ Self-contained: all engine/event/costing/model code copied here and verified to run from this dir.
- ✅ **Parts I–II backfilled:** Orientation + Models 1–8 (each = one idea, source-and-run + commented
  `eval:false` deltas + the random-audit idiom) + Model 8 "freeze trap" fulcrum (death rate falls 6.3 → 3.2
  under constraint, p < 0.001) + the "gallery of traps" (model11/12, C, D — `eval:false`, each naming the
  invariant it violates). 30 executable chunks; renders clean to **HTML (463 KB) + PDF (911 KB)**.
- ✅ **Phase 0 done:** `model-A.R` (endogenous-wait, ~750 LOC) and `model-B.R` (claim-ticket companion,
  ~740 LOC) built on the `model10` CEA spine and **independently verified** — capacity enforced exactly
  (max concurrent == c), zero leaks (seize == release, server → 0), contention genuinely endogenous (wait
  rises as c↓ / N↑; ablation confirms), competing risks race during the wait, CEA accounting conserved
  (no sick1+treatment double-count), null-effect regression flat across c. **Headline result holds: A and B
  agree on mean CEA endpoints (deaths, S2-time, dQALY, dcost) within Monte-Carlo noise**, differing only by
  construction on the wait distribution (B exact FIFO, A analytic) and treated count.
  - *Costing fix made vs `model10`:* `qaly_arrivals()` now keys utilities on the order-independent
    `*_active` booleans instead of the exact `active_resources` string (`'sick1, A'`), which silently
    dropped to zero utility when real contention reordered the seizes. (Caught by the null-effect test.)
  - *Run guards:* `model-A.R` skips its experiment block when `MODEL_A_NORUN=1`; `model-B.R` skips it under
    `source()` automatically (`sys.nframe()` guard). So both can be `source()`d into the `.qmd` cheaply.
  - *Perf caveat:* `model10`'s per-patient `split_arrivals()` costing is the bottleneck (~minutes at
    N=1000, 10 seeds). Use modest N in the doc + freeze.
- ✅ **Parts III–IV + Appendix done — first full draft complete.** Approach A (endogenous-wait, fire-time
  guard) and Approach B (claim-ticket companion) mechanism walkthroughs with live demos; the A-vs-B
  head-to-head (static verified agreement table + a small live reproduction + the tradeoff table); the
  7-rung validation ladder; the LMIC "choosing an approach" guidance; and the `simmer`-semantics appendix.
  **~48 executable chunks; renders clean to HTML (799 KB) + PDF (1.12 MB)** in ~5 min (freeze caches it).
  `drafts/val-harness-{A,B}.R` reproduce the N=1000/10-seed numbers.
- ✅ **CEA / ICER table added (Part IV, after the head-to-head).** Four strategies — No-treatment ·
  Infinite · Constrained-A · Constrained-B — with discounted cost/QALYs, incrementals (± SE), and ICERs,
  reproduced by `drafts/cea-icer-harness.R`. Infinite capacity is *dominant* (cost-saving + 3 QALYs); the
  constraint forfeits nearly all of that value; A and B agree within Monte-Carlo noise (both constrained
  increments are within ~1 SE of zero, so those ICERs are noise-dominated — the robust signals are the gulf
  vs infinite capacity and the A≈B agreement).
- ✅ **`model-A.R` / `model-B.R` library hygiene fixed.** Both now load `simmer` last and force it above
  `lubridate` on the search path (detach + reattach), so `simmer::now()` is never masked and both are robust
  standalone; the `.qmd`'s re-attach workaround was removed.
- ⏳ **Open polish items (none blocking):** migrate `cea-table-functions.R` to `kableExtra`/`gt`; pre-render
  bespoke A/B mechanism diagrams to `.svg`+`.pdf`; raise demo N once render budget allows.

Tables: migrate `cea-table-functions.R` output to `kableExtra`/`gt` (via `dampack::calculate_icers(...,
return_data = TRUE)`) for faithful PDF — `flextable` styling is only partially faithful in PDF.
