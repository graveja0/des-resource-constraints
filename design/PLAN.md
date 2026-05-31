# Build plan — "DES for CEA with resource contention": an incremental, executable explainer

**Status:** rewritten for the **LMIC cancer-screening** arc (May 2026). Companion to
`design/methodology.md` (the verified A/B contention methods). Authors: John Graves & Shawn Garbett.

**What this is:** a section-by-section build spec for a *standalone, fully executable* Quarto
document that walks the Sick-Sicker model from `model-1` up to two correct resource-contention
designs (**Approach A** and **Approach B**), framed around a concrete LMIC screening decision.

**Decisions locked in:**
- Standalone, full-arc document (`model-1` → A/B), fully executable, dual HTML + PDF.
- **Clinical case = LMIC cancer screening** (Republic of SMDM; the SMDM-2026 symposium case).
  Molecular test = **standard of care**; a cheaper **field test** is the comparator.
- **No "gallery of traps."** This is a *ladder*, not an analytic odyssey. The freeze trap appears
  only as a short prose motivator for why A and B never block — no broken models are run.
- **Common Random Numbers** (`R/crn.R`) are the variance-reduction substrate throughout.
- **Headline result:** without contention the field test is **SE** (cheaper *and* more effective);
  a saturated confirmatory bottleneck drifts the ICER into **SW** (cheaper but *less* effective).

---

## 0. The clinical case (the spine the whole document hangs on)

The Republic of SMDM screens for a cancer with a long pre-clinical phase. Two tests compete:

| | Molecular (SoC) | Field test |
|---|---|---|
| Coverage | 12 % | 50 % (primary-care deliverable) |
| Sensitivity | 0.95 | 0.70 |
| Specificity | 0.95 | 0.70 (heavier false-positive burden) |
| Cost / test | \$1,200 (cold chain, cartridges, lab) | \$100 |

The downstream **confirmatory diagnostic capacity** is fixed (≈ 4 confirmations / yr per 1,000
eligible). The field test's 3–4× referral volume — true *and* false positives — overwhelms it. The
modelling question: **does accounting for that bottleneck change the recommendation?**

Disease model = Sick-Sicker (H → S1 pre-clinical → S2 advanced → Dead). Treatment of a confirmed S1
case cuts S1→S2 progression 80 %. The cost structure is **program-dominated**: disease-state costs are
modest (advanced disease is cheap, deadly palliation), the screening program is the expensive line —
so the cost axis is set by the test, and the queue moves the *effect* axis.

---

## 1. Document structure

Four parts + appendix. **One new idea per model step** (the proven rhythm). Numbering is the file
numbering (`model-1` … `model-11`); A/B are `model10`/`model11`.

| § | Source | The teaching step | Code |
|---|---|---|---|
| **Front** | — | How to read DES as a CEA modeller; the car-race + Petri-net intuition; why DES (not Markov/microsim); the O(n log n) "bouncer" cost; the JAMA-colour vocabulary | prose + figures |
| **I.1** | `model-1` | The engine + the empty patient: the 6-part contract; `main_loop.R` as a black-box next-event engine; resources-as-tallies; the `function()`-callback gotcha | source+run |
| **I.2** | `model-2` | Valuation as a separate pass: `cost_arrivals`/`qaly_arrivals`; continuous discounting (QALYs live from here) | source+run |
| **I.3** | `model-3` | The first competing risk (Death): event-per-file, `branch(continue=FALSE)`, `next_event` = argmin | source+run |
| **I.4** | `model-4` | First disease state + `reactive=TRUE` resampling; state-dependent hazard; per-state cost/QALY; the random-audit idiom | source+run |
| **I.5** | `model-5` | Recurrence (S1→H): the `Healthy` registry event; state-gated `time_to_event` | source+run |
| **I.6** | `model-6` | The full ladder (S2): the 5-place checklist for adding a state. **End of the natural-history spine.** | source+run |
| **I.7** | `model-7` | **The intervention = screening.** One-time staggered screen (`event_screen.R`); coverage vs result; true/false positives; confirmation + one-time treatment cost; `treated_s1`. Establish **molecular as the standard of care** (vs no-screen: cost-effective). | source+run |
| **II.8** | `model-8` | **The comparator = the field test**, *no contention*. Molecular vs field, capacity = ∞. Field is **SE-dominant** (cheaper at scale, more total true positives). Introduce **CRN** here — the paired-seed variance crush that makes the SE signal clean. The "looks great on paper" result. | source+run |
| **II.9** | `model-9` | **Exogenous queue:** a confirmation wait drawn from a fixed lognormal. Disease progresses during the wait. **It barely moves the ICER** — a fixed wait, identical across arms, cannot see that the high-volume strategy congests *itself*. The cautionary "false reassurance." | source+run |
| **III.10** | `model10` | **Approach A — endogenous-wait registry event.** Real finite `confirm` resource + FIFO waitlist; occupancy-coupled analytic wait; fire-time guard (cap exact, patient never blocks); `main_loop.R` untouched. Field's advantage collapses toward break-even. | source+run |
| **III.11** | `model11` | **Approach B — claim-ticket companion.** `clone(n=2, self, companion)` + `pid`-keyed `send`/`trap`; companion holds the exact FIFO `seize()`; "Confirm acquired" registry event. The exact emergent queue tips the ICER into **SW**. | source+run |
| **IV** | A & B | **The swing, and the choice of model.** The SE→SW drift across §8→11. A vs B: they *agree at mild load* (replication, CRN off) but *straddle the SE/SW boundary under saturation* — the approximate and exact models disagree on the recommendation exactly where it matters. Why B is the defensible choice. The LMIC framing. **Limitations** (FP disutility, overdiagnosis, treatment-capacity bottleneck, LTFU — the honest extensions). | source+run |
| **App.** | — | simmer semantics (clone carries attribute values not the seize ledger; `send` is global; live occupancy readable in a sampler); the CRN design (why per-patient banks; why CRN can't bridge A vs B); the freeze gotcha + reproducibility | prose |

**Narrative spine:** models 1–6 build a faithful natural history where simmer is *only* a timeout/tally
engine; model-7 makes molecular screening the standard of care; model-8 shows the cheaper field test
looks dominant (SE) when capacity is ignored; model-9 shows an exogenous queue gives false reassurance;
A and B model the queue *endogenously* and the recommendation drifts into SW — with the exact model (B)
and the approximate model (A) disagreeing under saturation. The invariant both honour: **the patient
never blocks; disease progresses during the wait.** A's selling point: contention *without touching the
decade-stable `main_loop.R`*. B's: the *exact* queue, at the cost of fragility.

---

## 2. Build & reproducibility spec

**Format / front-matter** — single self-contained `.qmd`, dual output (html cosmo + pdf scrartcl),
`fig-format` svg/pdf per format, `execute: { freeze: auto }`. Lives at `manuscript/resource-contention.qmd`
in this repo; renders via the repo-root `_quarto.yml` (`execute-dir: project`).

**The freeze gotcha (load-bearing):** freeze keys off the `.qmd` text, NOT the `source()`d `R/*.R`.
After editing any model/inputs/engine file, `rm -rf _freeze && quarto render …`. Commit `_freeze/`.

**CRN + seeds:** every `des_run()` takes a `seed` and arms per-patient CRN banks; the two strategy arms
share the seed so the mol-vs-field difference is signal, not noise. Keep doc-level `N` at 1,000 (the SE/SW
result is clean there); quote `N=1e4` numbers as prose (model11 at N=1e4 ≈ 14 s).

**Tables:** migrate `cea-table-functions.R` off flextable to `kableExtra`/`gt` (dual-format).

**Figures:** pre-render the load-bearing mechanism diagrams (the A fire-time guard; the B clone/`pid`
handshake; the SE→SW cost-effectiveness plane) to committed `.svg` + `.pdf`.

---

## 3. Sequencing

1. **Phase 0 (DONE):** models 7–11 built + validated; CRN; staggered screening; per-1000 capacity;
   screening-cost accounting (#1); one-time treatment cost (#3); SE→SW calibration locked;
   A=B-by-replication harness (`validation/validate-AB-replication.R`).
2. **Scaffold** the new Part II–IV headings against the existing Part I/orientation (which survive).
3. **Part I (1–6):** light refresh — fix the model-4/5 attribution (costing is in model-4; model-5 adds
   the Healthy event), the model-2 "QALYs live from here" point.
4. **Part I.7 + II–III:** rewrite for screening / field comparison / queues A,B. **Delete** the freeze-trap
   demonstration and the four-trap gallery; keep the freeze trap as one prose paragraph.
5. **Part IV:** SE→SW drift table; A-vs-B replication result; LMIC framing; limitations.
6. **Polish:** pre-render diagrams; `freeze: true`; update README.

---

## 4. Open decisions / notes

- **A vs B under saturation:** at cap=2 they straddle SE/SW. Frame as the *point* (exact vs approximate
  diverge under load), with B as defensible. Replication numbers from `validation/validate-AB-out.txt`.
- **Limitations to state explicitly** (face-validity items not modelled): false-positive disutility;
  overdiagnosis; loss-to-follow-up as a hazard distinct from progression; oncology-treatment capacity as a
  second bottleneck; constant (age-independent) hazards. The screening-cost (#1) and lumpy-treatment (#3)
  items are now fixed.
- **DALY extension** (Leech/Graves MDM 2025): out of scope for this document.
