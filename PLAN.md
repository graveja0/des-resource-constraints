# Build plan — "DES for CEA with resource contention": an incremental, executable explainer

**Status:** scope / plan. Companion to `resource-contention-methodology.md` (which establishes the
A/B methods this document teaches). Author: drafted for John Graves & Shawn Garbett, May 2026.

**What this is:** a section-by-section build spec for a *standalone, fully executable* Quarto
document that walks the Sick-Sicker model from `model-1` up to two correct resource-contention
designs (**A** and **B**), with detailed prose, commented code, and a closing tradeoff discussion.

**Decisions locked in (from scoping):**
- **Standalone, full-arc** document (`model-1` → A/B), reusing existing prose/figures where useful.
- **Fully executable** — every result, table, and figure runs at render.
- **Standard Quarto → HTML *and* PDF** (not closeread).

---

## 0. Prerequisite phase (must precede a fully-executable doc)

Because every chunk runs at render, **A and B must exist as working, validated model files first.**
This is the single hard dependency. Per `resource-contention-methodology.md` §7:

- [ ] **`model-A.R`** — extend `model10.R`'s endogenous-wait registry event into true occupancy-coupled
  contention: a real `add_resource("A", capacity = c)` (not in `counters`), a global FIFO waitlist,
  `years_till_treatmentA()` reading live `get_server_count`/`get_capacity` + queue position, the
  fire-time free-slot **guard**, an `EndA` release event, and `years_till_sick2()` gated on treatment.
- [ ] **`model-B.R`** — put the claim-ticket companion on `model10`'s CEA spine: contended `treatment`
  resource (not in `counters`) + `treatment_held` tally; `sick1()` does `trap(acq_<pid>)` →
  `clone(n=2, self, companion) |> synchronize(wait=FALSE)`; companion `renege_if(abandon_<pid>) |>
  seize |> renege_abort |> send(acq_<pid>) |> timeout(dur) |> release`; the reactive
  "Treatment acquired" event; per-patient signal names.
- [ ] **Validation harness** (the doc's §13 results come from this): unconstrained limit reproduces
  `model10`; **null-effect regression** (set progression/cure factors = 1 → identical outcomes across
  all `c`); face validity (deaths/S2-time monotone in `1/c`); leak audits; **A-vs-B agreement** table.
- [ ] Add `set.seed()` to every stochastic demo (the current model files don't) so frozen output is
  reproducible and reviewer diffs aren't noisy.

The validated `/tmp/probe_A-*.R` and `/tmp/probe_B-*.R` from the methodology work are the starting
drafts — they just need retrofitting onto the real `main_loop`/`model10` CEA pipeline and `set.seed`.

---

## 1. Document structure

Four parts + appendices. **Each base model = one section** (the proven "one new idea per step"
rhythm). The contention endpoints are relabeled **Approach A/B** to avoid colliding the `model-N`
numbering with the methods labels; the broken attempts become a "gallery of traps."

| § | Source model | New concept (the teaching step) | Code: runs vs `eval:false` delta | Key reuse |
|---|---|---|---|---|
| **Front** | — | "How to read DES as a CEA modeler" terminology table; car-race + Petri-net intuition; why-DES error taxonomy; the O(n log n) "bouncer" cost | none (prose+figures) | Shawn's DES↔policy map (extended w/ constrained rows); John's car-race + JAMA-color vocab table + `petri-token-queue.png`; 01_oxford error taxonomy |
| **I.1** | `model-1` | The engine + the empty patient: the 6-part contract; `main_loop.R` as a black-box next-event engine; resources-as-tallies; the `function()`-callback gotcha | source+run; delta = the contract skeleton | DES.qmd Model 1 prose; Oxford 03 line-highlight walkthrough |
| **I.2** | `model-2` | Valuation as a separate pass: `cost_arrivals`/`qaly_arrivals`; continuous discounting | source+run; delta = the two costing fns + `discount_value` | discount.R 3 worked examples; Oxford 04 costing walkthrough |
| **I.3** | `model-3` | The first competing risk (Death): event-per-file, `branch(continue=FALSE)`, `next_event` = argmin | source+run | DES.qmd Model 3 `reactive` explanation |
| **I.4** | `model-4` | First disease state + `reactive=TRUE` resampling; state-dependent hazard; per-state cost/QALY; **the random-audit validation idiom** | source+run; delta = `years_till_sick1`/`sick1` + registry | DES.qmd Model 4 audit pattern (lift verbatim) |
| **I.5** | `model-5` | Recurrence (S1→H): bidirectional loop; state-gated `time_to_event` (`horizon+1` to disable) | source+run; delta = 4th registry entry | DES.qmd Model 5 |
| **I.6** | `model-6` | The full ladder (Sicker/S2): the **5-place checklist** for adding a state | source+run; delta = `sick2` + `event_death3` | DES.qmd Model 6 |
| **I.7** | `model-7`(+8-analysis) | A comparator (treatment strategy): strategy-as-attribute, treatment-as-resource, `u.Trt`; **first ICER** (NHB/NMB), `N=1e4`, DTMC validation row | source+run twice (treat/notreat), difference | DES.qmd Model 7–8; `model10`'s `create_cea_table`/DTMC |
| **II.8** | `model-8` | **The freeze trap** (the fulcrum): a finite-capacity `seize()` blocks the arrival → disease clock freezes → death rate *collapses*. State the **shared invariant: the patient must never block.** | run, then show the broken death-rate `t.test` | Constrained.qmd "Naive Solution" prose ("destroys the simulated rate of death") |
| **II.9** | `model-11`,`model-12` + C,D | **Gallery of traps** (never executed): global-signal renege (broadcast hits everyone); clone/synchronize (independent ledgers → double-release; `wait=TRUE` deadlock); C (renege-folded → slots never recycle); D (manual calendar → cap not enforced). Each with the *exact invariant it violates* | **all `eval:false`** in `callout-warning` blocks | methodology doc §2–3 simmer-semantics facts |
| **III.A** | `model-A.R` | **Approach A — endogenous-wait registry event:** real capacity-`c` resource + fire-time guard (cap exact); occupancy-coupled analytic wait + its **free parameters**; patient never blocks | source+run; delta = `years_till_treatmentA` guard/occupancy + `EndA` | methodology doc §4-A + probe numbers |
| **III.B** | `model-B.R` | **Approach B — claim-ticket companion:** companion holds a real FIFO `seize()` queue + per-patient `send`/`trap` + `renege`; **exact emergent** contention; the decoupled effect-vs-occupancy duration | source+run; delta = companion trajectory + signals | methodology doc §4-B + Constrained.qmd send/trap lineage |
| **IV.12** | A & B | **Head-to-head:** run both under one scenario; compare wait distributions, deaths, dQALY; show A tracks B for mean CEA endpoints; audit one patient's wait under each; the **A-vs-B tradeoff table** | source+run both | "Three Approaches" block as the table *template* (content superseded) |
| **IV.13** | harness | **Validation ladder:** M/M/c special case; unconstrained limit = `model10`; null-effect regression; face validity; leak audits; endogeneity; A-vs-B agreement | source+run | methodology doc §6 |
| **IV.14** | — | **Choosing an approach + LMIC framing:** investment-in-reducing-wait decision problem; scale/ACCRE note | prose | Constrained.qmd Part 2 reframing |
| **App.** | — | simmer-semantics reference (clone/synchronize/renege/rollback gotchas); reproducibility notes; file manifest | prose | methodology doc §2-3 |

**Narrative spine to make explicit** (the thread that ties it together): models 1–7 build a faithful
CEA model where simmer is *only a timeout/tally engine*; `model-8` introduces the first finite resource
and shows the naive `seize()` **breaks face validity**; the traps show why the obvious fixes fail; A and
B are the two ways to honor the invariant (*patient never blocks; disease progresses during the wait*).
A's selling point: it adds contention **without touching the decade-stable `main_loop.R` engine**
(a registry event); B's: it gives the **exact** queue at the cost of fragility.

---

## 2. Build & reproducibility spec (all verified locally)

**Format / front-matter** — single self-contained `.qmd`, dual output:
```yaml
format:
  html: { theme: cosmo, toc: true, toc-depth: 3, code-tools: true, fig-format: svg, fig-width: 6, fig-height: 3.5 }
  pdf:  { toc: true, number-sections: true, documentclass: scrartcl, fig-pos: "H", fig-format: pdf, fig-width: 6, fig-height: 3.5 }
knitr: { opts_chunk: { warning: false, message: false, fig.align: center } }
execute: { freeze: auto, echo: true }
```
- **PDF works as-is**: `quarto check` finds system TeX Live 2023 at `/Library/TeX/texbin` (lualatex); a
  two-format probe rendered to both HTML and PDF cleanly. **No tinytex install needed.** (On a LaTeX-less
  machine: `quarto install tinytex` once.)
- **Do NOT reuse the workshop's project `_quarto.yml`** — it's `type: website`, `format: html` only,
  `freeze: true`, and pulls `_extensions/` you don't need. Give this doc its own config.

**Freeze (the reproducibility engine):**
- Use `freeze: auto` (re-execute only when the `.qmd` changes) while authoring; `freeze: true` once final.
- Freeze is **per-format** → first render of *each* format runs the simmer code once (you'll get both
  `execute-results/html.json` and `pdf.json`). Commit `_freeze/` to git.
- ⚠️ **Biggest gotcha:** freeze keys off the `.qmd` text, **not** off the `source()`d `.R` files. After
  editing any `model-N.R`/`inputs.R`, you **must** clear the cache first (there is no `--no-freeze` flag:
  `rm -rf _freeze && quarto render doc.qmd`) or the doc renders stale output. Document this in a README; it
  bites a `source()`-heavy doc hard.

**Tables (must render in both formats):**
- The current `create_cea_table()` uses **flextable** — it *does* emit native LaTeX `longtable` (not an
  image), but its `merge_v`/`fp_border`/`compose` styling is only partially faithful in PDF.
- **Standardize on `kableExtra` or `gt`** (both verified dual-format). Clean seam: keep the ICER math in
  `cea-table-functions.R` via `dampack::calculate_icers(..., return_data = TRUE)`, then render that tidy
  frame with `kbl(..., format = if (knitr::is_latex_output()) "latex" else "html")` / `gt()` in the doc.

**Figures:** per-format `fig-format` (`svg` for HTML, `pdf` for PDF) → vector in both, no Chrome
dependency. For the **load-bearing Petri/state/handshake diagrams** (the A guard, the B claim-ticket
handshake), pre-render each to committed `.svg` + `.pdf` and `include_graphics(if (knitr::is_latex_output())
…pdf else …svg)`. Reserve live ```{mermaid}``` for throwaway HTML-only sketches (Mermaid→PDF routes
through headless Chrome — present, but slow/fragile/CI-hostile).

**Code organization** — "commented, executable, progressively revealed":
1. **Spine = `source('model-N.R')` then `des_run(inputs)`** (echo on, so readers see which file). The
   model files are self-contained and re-`source` `inputs.R`/`main_loop.R`, so sourcing `model-8` after
   `model-7` cleanly *replaces* definitions — no stale state. Keeps the doc DRY against tested files.
2. **Delta = an `eval:false`, heavily-commented snippet** showing only the new/changed function for that
   step (the new registry entry, the new event pair, the A guard, the B companion). This is how you get
   all three of commented + executable + progressive.
3. **`library(simmer)` + a params/`set.seed` chunk once near the top**; scale per-chunk via
   `modifyList(inputs, list(N = …))`.
4. **Text in `::: {.callout-note}`**, the broken C/D/model-11/12 snippets in `::: {.callout-warning}`,
   **`eval:false`** — never execute the broken models (they'd emit plausible-looking garbage).

**Render budget:** keep doc-level `N` modest (200–1000) and `replicate(20–50)`; quote production-`N`
(1e4) numbers as prose. `model10`'s six `N=1e4` scenarios (~45 s) and any `replicate(100)` at `N=1e4`
(~minutes) are the cost cliff — freeze pays it once per format.

---

## 3. File / repo layout

Recommended home: **this workshop repo** (`des-workshop-2025/`), which already has the engine in `R/`
(`main_loop.R`, `inputs.R`, `discount.R`, `event_*.R`, `cea-table-functions.r`). It is missing only the
`model-N.R` *driver* files (those live in Shawn's repo) and the new A/B files.

```
des-workshop-2025/
└── des-cea-contention/              # new mini-project dir (own _quarto.yml, NOT the website one)
    ├── des-cea-contention.qmd       # the explainer
    ├── _quarto.yml                  # dual html+pdf, freeze: auto
    ├── _freeze/                     # committed; html.json + pdf.json per render
    ├── R/                           # copy the canonical engine + event + costing files here
    │   ├── main_loop.R inputs.R discount.R cea-table-functions.R event_*.R
    │   ├── model-1.R … model-10.R   # copied from spgarbet/sick_sicker_des
    │   └── model-A.R  model-B.R     # NEW (Phase 0)
    ├── figures/                     # pre-rendered diagrams: <name>.svg + <name>.pdf
    └── references.bib               # reuse existing
```
Keep the `.R` files beside the `.qmd` (their `source()` calls are relative). **Open decision:** whether
the canonical model files stay sourced-from-copy here, or this doc lives in Shawn's repo next to the
originals. Recommendation: copy into this repo's `R/` so the doc is self-contained and renders in CI;
keep Shawn's repo as upstream-of-record and sync on change. (The doc is then self-contained and renders in CI.)

---

## 4. Sequencing

1. **Phase 0** — build & validate `model-A.R`, `model-B.R` + the validation harness (the gate).
2. **Scaffold** — mini-project dir, `_quarto.yml`, copy `R/` files, stub all sections with headings +
   the source/run + `eval:false`-delta skeleton; confirm it renders to both formats empty.
3. **Part I** (models 1–7) — mostly assembly: lift existing prose, wire source+run+delta per step.
4. **Part II** (model-8 fulcrum + traps gallery) — the pivot; write the freeze-trap demonstration.
5. **Part III** (A, B) — the new methods sections + their mechanism diagrams.
6. **Part IV** (A-vs-B results, validation ladder, guidance) — depends on Phase 0 harness output.
7. **Polish** — pre-render diagrams to svg+pdf, set `freeze: true`, README with the clear-`_freeze/`-after-`.R`-edit rule.

---

## 5. Risks / open decisions

- **Phase 0 is the critical path.** No validated A/B → no executable Part III/IV. Don't start the doc's
  back half until the two model files pass the null-effect + A-vs-B-agreement tests.
- **`source()` + freeze staleness** (see §2) — needs the clear-`_freeze/`-after-`.R`-edit discipline.
- **Table migration** off flextable to kableExtra/gt — small but touches `cea-table-functions.R`.
- **Labeling:** confirm the A/B/C/D ↔ model-10/11/12 mapping presentation (proposed: A/B as named
  approaches; 11/12/C/D as the trap gallery) reads cleanly for workshop attendees who saw the old numbering.
- **Where it lives** (this repo vs Shawn's) — §3.
- **Scope of the cautionary gallery** — full four traps (11, 12, C, D) or just the two historical ones
  (11, 12)? Recommendation: all four, briefly, since they're high-value "spot the bug" material and the
  methodology work already has the verified snippets + invariants.
- **DALY extension** (Leech/Graves MDM 2025) — in scope for this doc or a follow-on? Currently out.
