# Planned Modification: Multi-threshold support in `Fahrzeit_Zusammenfassung`

## Motivation

`Fahrzeit_Zusammenfassung` currently accepts only a single scalar value for
`Grenzwert_Minuten`. Analyses often need to compare results across multiple
thresholds (e.g. 30 min and 60 min) in a single call. This modification
extends the argument to accept a numeric vector and returns wide-format output.

Additionally, `create_polygon_label` contains a `rowwise()` call that is
flagged as a performance anti-pattern in `todo_list.md` (item 1.2) and the
tidyverse style guide; this is fixed in the same change.

---

## API change: `Grenzwert_Minuten`

| Input | Output columns |
|---|---|
| `NULL` | baseline only (unchanged) |
| scalar, e.g. `30` | `Anzahl_Betroffene`, `Prozent_Betroffene` (no suffix — backward compatible) |
| vector, e.g. `c(30, 60)` | `Anzahl_Betroffene_30min`, `Prozent_Betroffene_30min`, `Anzahl_Betroffene_60min`, `Prozent_Betroffene_60min` |

`NA` values and duplicates in a vector input are silently dropped before
processing (`unique(na.omit(...))`). An empty vector (`numeric(0)`) after
this cleaning is treated the same as `NULL`.

---

## Logic

```
data
  |> mutate(Fahrzeit_Minuten = Fahrzeit_Sekunden / 60)
  |> summarise baseline columns (.by = {{ .by }})
       Einwohner_Gesamt, Mittlere_Gewichtete_Fahrzeit, Anzahl_Gitterzellen

thresholds <- unique(na.omit(Grenzwert_Minuten))

if length(thresholds) == 0: return baseline

if length(thresholds) == 1:
  compute per-threshold cols (no suffix) via summarise_threshold()
  left_join to baseline
  return

if length(thresholds) > 1:
  map(thresholds, \(thresh) summarise_threshold(..., threshold = thresh))
  |> list_rbind()
  |> pivot_wider(
       names_from  = Schwellenwert,
       names_glue  = "{.value}_{Schwellenwert}min",
       values_from = c(Anzahl_Betroffene, Prozent_Betroffene)
     )
  left_join to baseline
  return
```

---

## Internal helper (not exported)

```r
summarise_threshold <- function(data, threshold, .by) {
  data |>
    mutate(Ueber_Grenzwert = Fahrzeit_Minuten > threshold) |>
    summarise(
      Anzahl_Betroffene = sum(Einwohner[Ueber_Grenzwert], na.rm = TRUE),
      Prozent_Betroffene = Anzahl_Betroffene / sum(Einwohner, na.rm = TRUE) * 100,
      .by = {{ .by }}
    ) |>
    mutate(Schwellenwert = threshold)
}
```

The helper is defined in `R/Fahrzeitenkarte.R` but not `@export`ed.

---

## `create_polygon_label` fix

Replace:

```r
result <- result |>
  rowwise() |>
  mutate(label = HTML(label))
```

With:

```r
result <- result |>
  mutate(label = lapply(label, HTML))
```

This avoids `rowwise()` (slow, flagged in todo_list.md item 1.2) while
producing an identical list-column of `html` objects.

---

## Files changed

| File | Change |
|---|---|
| `R/Fahrzeitenkarte.R` | Add `summarise_threshold()` helper; rewrite `Fahrzeit_Zusammenfassung()`; fix `rowwise()` in `create_polygon_label()` |
| `R/zzz.R` | Add `@importFrom tidyr pivot_wider` |
| `DESCRIPTION` | Add `tidyr` to `Imports` |
| `tests/testthat/test-fahrzeit.R` | Add multi-threshold tests |

---

## `globalVariables` additions

The helper introduces the intermediate column `Schwellenwert` which is
referenced bare inside `mutate()`. Add `"Schwellenwert"` to the
`globalVariables()` call at the top of `R/Fahrzeitenkarte.R`.

---

## Tests to add

- `NULL` threshold → no threshold columns (baseline unchanged)
- scalar `30` → `Anzahl_Betroffene`, `Prozent_Betroffene` (no suffix, backward compat)
- vector `c(30, 60)` → wide columns `Anzahl_Betroffene_30min`, `Prozent_Betroffene_30min`, `Anzahl_Betroffene_60min`, `Prozent_Betroffene_60min`
- vector with `NA` and duplicates `c(30, NA, 60, 30)` → same result as `c(30, 60)`
- empty vector `numeric(0)` → baseline only (no threshold columns)
- values computed correctly for multi-threshold case
