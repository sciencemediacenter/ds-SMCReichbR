# To Do: Finalize ReichbR package

## Summary
`devtools::check()` currently stops at WARNINGs and NOTEs: the DESCRIPTION Description is not sentence-based and lists an R dependency with a non-zero patchlevel, a non-ASCII character remains in `R/Geofilter.R`, non-standard top-level files (`global.R` and this todo list) are still present, several Imports appear unused, and the example in `man/Fahrzeit_Zusammenfassung.Rd` exceeds 100 characters. Addressing these will allow the package to check cleanly.

## Steps
1. **Add test coverage** – create one or more `tests/testthat/*.R` scripts exercising key helpers, ensuring `tests/testthat.R` is satisfied and `testthat::test_check()` finds tests.
2. **Repair `DESCRIPTION` metadata** – rewrite the Description field as complete sentences, align R version requirements with `Depends` or `Imports`, and remove patch-level references that trigger warnings.
3. **Clean top-level layout** – decide whether the `global.R` script is necessary for the package; if not, move it into `inst/` or delete it so `R CMD check` stops warning about non-standard files.
4. **Ensure ASCII-only R code** – sanitize `R/Geofilter.R` by escaping or replacing non-ASCII characters (use `tools::showNonASCIIfile()` to detect them).
5. **Synchronize Imports** – review `Imports` and `NAMESPACE`, guaranteeing each package (`connections`, `leaflet`, `leaflet.extras`, `purrr`, `sf`, `stringr`, etc.) is actually used in the R code; remove unused entries.
6. **Wrap Rd example lines** – edit `man/Fahrzeit_Zusammenfassung.Rd` so example lines stay under 100 characters, especially the long `Fahrzeit_Zusammenfassung(...)` call.
7. **Re-run `devtools::check()`** – after the above fixes, ensure the check passes without errors, warnings, or notes.
