# DES for CEA with Resource Contention

This repository builds teaching material on **discrete-event simulation (DES) for
cost-effectiveness analysis (CEA) with resource contention / capacity constraints**, around the
Sick-Sicker model. There are two deliverables plus an archived precursor, all drawing on a single
shared simulation engine.

- **`manuscript/`** — the tutorial: *Discrete-Event Simulation for Cost-Effectiveness Analysis with
  Resource Contention.* Walks the Sick-Sicker model from `model-1` up to two correct contention
  designs (**Approach A** and **Approach B**) and compares their tradeoffs. Renders to HTML + PDF.
- **`slides/`** — a revealjs talk built **from** the manuscript, reusing the same engine and figures.
- **`workshop-2025/`** — archived materials from the July 2025 University of Oxford HERC workshop
  (a self-contained Quarto website, the precursor to the manuscript). Kept as a known-good snapshot.

## Layout

```
des-resource-constraints/
├── _quarto.yml                  project config: execute-dir: project; renders manuscript/ + slides/
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
│   ├── model11.R  model12.R     cautionary traps (shown eval:false in the manuscript)
│   ├── model-A.R  model-B.R     the two correct contention designs (validated)
│   └── become-sick-cloned.R     model12 helper
│
├── figures/                     shared static figure assets (Petri nets, car-race, diagrams)
├── manuscript/                  the tutorial: resource-contention.qmd + _metadata.yml + _freeze/
├── slides/                      the revealjs talk: resource-contention-slides.qmd + _metadata.yml
├── workshop-2025/               archived Oxford HERC workshop (its own self-contained Quarto project)
├── design/                      working/design docs (NOT deliverables): methodology.md, PLAN.md
└── validation/                  harnesses that reproduce the manuscript's numbers (val-harness-*, probes)
```

### How paths resolve (important)

The repo root is a single Quarto project. `_quarto.yml` sets `execute-dir: project`, so **every chunk
runs with the working directory at the repo root**. Deliverables in `manuscript/` and `slides/` use a
`_metadata.yml` (not a `_quarto.yml`) for their format overrides — a `_metadata.yml` is *not* a `here()`
project-root marker, so `here::here()` still resolves to the repo root.

- The model files in `R/` are sourced as `source(here::here("R/model-N.R"), chdir = TRUE)`. The
  `chdir = TRUE` matters: each `model-N.R` does its own bare `source('inputs.R')`, which resolves
  against `R/` only while the working directory is temporarily `R/`.
- Markdown image paths in `manuscript/` use `../figures/…` (relative to the document).

## Render

```bash
# the whole project (manuscript + slides):
quarto render

# a single deliverable / fast iteration:
quarto render manuscript/resource-contention.qmd --to html
quarto render manuscript/resource-contention.qmd --to pdf   # system LaTeX (TeX Live at /Library/TeX/texbin)
quarto render slides/resource-contention-slides.qmd

# the archived workshop (its own project):
cd workshop-2025 && quarto render
```

PDF works with system LaTeX — no `tinytex` needed (on a LaTeX-less machine: `quarto install tinytex` once).

## ⚠️ The freeze gotcha (read before editing any `R/*.R` file)

`execute.freeze` re-runs a chunk only when the **`.qmd` text** changes — it does **not** notice edits to
the `source()`d files in `R/`. There is **no `--no-freeze` flag**. After editing any `R/model-*.R`,
`R/inputs.R`, `R/main_loop.R`, etc., clear the cache so chunks re-execute:

```bash
# this is a Quarto project, so the freeze cache lives at the repo-root _freeze/
rm -rf _freeze && quarto render manuscript/resource-contention.qmd
```

Keep `set.seed()` in every stochastic chunk so frozen output is reproducible. **Commit `_freeze/`.**
Rendered `.html`/`.pdf`/`_files/` are gitignored (regenerate from source); `_freeze/` is the committed
reproducibility artifact.

## The modeling substrate (one-paragraph orientation)

`main_loop.R` is a hand-rolled next-event competing-risks engine; in it, `simmer` is only a
timeout/tally engine and the "resources" are `capacity = Inf` accounting tallies. A *genuinely finite*
resource forces `simmer`'s native blocking `seize()`, which freezes a queued patient's event clock —
the **freeze trap** (Model 8). The shared invariant any correct fix must honor: *the patient must never
block; disease must keep progressing during the wait; capacity `c` must be enforced exactly; no leaks.*
**Approach A** honors it with an endogenous-wait registry event (analytic wait, engine untouched);
**Approach B** with a claim-ticket companion (exact FIFO queue). See `design/methodology.md` for the
verified methods and simmer semantics, and `design/PLAN.md` for the section-by-section build plan.
