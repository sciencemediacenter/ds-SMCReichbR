utils::globalVariables(c(
  ".data",
  "Einwohner",
  "Fahrzeit_Differenz_Minuten",
  "Anzahl_Betroffene",
  "Prozent_Betroffene",
  "Einwohner_Gesamt",
  ".by",
  "Fahrzeit_Sekunden",
  "Fahrzeit_Minuten",
  "Ueber_Grenzwert",
  "Schwellenwert",
  "Mittlere_Gewichtete_Fahrzeit_formatted",
  "Einwohner_Gesamt_formatted",
  "Anzahl_Betroffene_formatted",
  "Prozent_Betroffene_formatted",
  "label",
  "Mittlere_Gewichtete_Fahrzeit_Differenz_formatted",
  "Anzahl_Gitterzellen",
  "Gitterzellen_ID",
  "Krankenhaus_Standortnummer",
  "Gemeindeschluessel",
  "Anzahl_bewohnte_Gitterzellen",
  "Prozent_Abgedeckt_pro_Gemeinde",
  "Bundesland",
  "Bundesland_ID"
))


# Internal helper: summarise affected population for one threshold value.
# Returns the grouping columns plus Anzahl_Betroffene, Prozent_Betroffene,
# and a Schwellenwert column identifying the threshold used.
summarise_threshold <- function(data, threshold, .by) {
  data |>
    mutate(Ueber_Grenzwert = Fahrzeit_Minuten > threshold) |>
    summarise(
      Anzahl_Betroffene = sum(Einwohner[Ueber_Grenzwert], na.rm = TRUE),
      Prozent_Betroffene = Anzahl_Betroffene /
        sum(Einwohner, na.rm = TRUE) *
        100,
      .by = {{ .by }}
    ) |>
    mutate(Schwellenwert = threshold)
}

#' Summarize Weighted Travel Times
#'
#' Calculates the weighted mean travel time and, if one or more thresholds are
#' given, the number and percentage of people exceeding each threshold.
#'
#' The input `data` is expected to be a tibble (as returned by dplyr/tidyverse
#' workflows).
#'
#' @param data A tibble with travel times and population columns. Must include
#'   columns: Fahrzeit_Sekunden, Einwohner, Gitterzellen_ID.
#' @param .by Grouping columns (tidyselect, e.g. `c(Gemeindename, Gemeindeschluessel)`).
#' @param Grenzwert_Minuten Optional numeric threshold(s) in minutes for
#'   "affected" population. May be `NULL` (no threshold columns), a single
#'   scalar (backward-compatible output without column suffixes), or a numeric
#'   vector (wide-format output with `_<n>min` suffixes). `NA` values and
#'   duplicates are silently ignored.
#'
#' @return A tibble summarizing, for each group, the total population, weighted
#'   mean travel time, number of grid cells, and — when thresholds are supplied
#'   — the number and percentage of affected population.
#'
#'   For a single threshold the affected-population columns are named
#'   `Anzahl_Betroffene` and `Prozent_Betroffene` (no suffix).
#'
#'   For multiple thresholds the columns are named
#'   `Anzahl_Betroffene_<n>min` and `Prozent_Betroffene_<n>min` for each
#'   threshold value `<n>`.
#'
#' @examples
#' # summary <- Fahrzeit_Zusammenfassung(
#' #     df,
#' #     c(Gemeindename, Gemeindeschluessel), Grenzwert_Minuten = 30
#' #)
#'
#' @export
Fahrzeit_Zusammenfassung <- function(
  data,
  .by,
  Grenzwert_Minuten = NULL
) {
  prepared <- data |>
    mutate(Fahrzeit_Minuten = Fahrzeit_Sekunden / 60)

  baseline <- prepared |>
    summarise(
      Einwohner_Gesamt = sum(Einwohner, na.rm = TRUE),
      Mittlere_Gewichtete_Fahrzeit = weighted.mean(
        Fahrzeit_Minuten,
        w = Einwohner,
        na.rm = TRUE
      ),
      Anzahl_Gitterzellen = dplyr::n_distinct(Gitterzellen_ID),
      .by = {{ .by }}
    )

  thresholds <- unique(na.omit(Grenzwert_Minuten))

  if (length(thresholds) == 0) {
    return(tibble::as_tibble(baseline))
  }

  if (length(thresholds) == 1) {
    threshold_summary <- summarise_threshold(prepared, thresholds, {{ .by }}) |>
      select(-Schwellenwert)
    # join_by() does not accept tidyselect expressions (e.g. all_of()),
    # so we resolve the grouping columns from the materialised tibbles
    # and splice them as symbols: join_by(!!!syms(c("col_a", "col_b")))
    # expands to join_by(col_a, col_b).
    by_cols <- intersect(names(baseline), names(threshold_summary))
    result <- baseline |>
      left_join(threshold_summary, by = dplyr::join_by(!!!rlang::syms(by_cols)))
    return(tibble::as_tibble(result))
  }

  threshold_summary <- purrr::map(thresholds, \(thresh) {
    summarise_threshold(prepared, thresh, {{ .by }})
  }) |>
    purrr::list_rbind() |>
    tidyr::pivot_wider(
      names_from = Schwellenwert,
      names_glue = "{.value}_{Schwellenwert}min",
      values_from = c(Anzahl_Betroffene, Prozent_Betroffene)
    )

  # See comment above for rationale on intersect + !!!syms pattern.
  by_cols <- intersect(names(baseline), names(threshold_summary))
  result <- baseline |>
    left_join(threshold_summary, by = dplyr::join_by(!!!rlang::syms(by_cols)))

  tibble::as_tibble(result)
}

#' Create HTML Labels for Travel Time Polygons
#'
#' Generates formatted HTML labels for map polygons summarizing travel times and population statistics.
#' The function formats numeric columns and conditionally includes affected population statistics if present.
#'
#' The input `data` is expected to be a tibble (as returned by dplyr/tidyverse workflows).
#'
#' @param data A tibble containing summary statistics, including at least columns for weighted mean travel time and total population.
#' @param Verwaltungsebene A string or symbol indicating the administrative level (e.g., municipality, district) to display in the label.
#'
#' @return A tibble with an added `label` column containing HTML-formatted labels for use in interactive maps.
#'
#' @examples
#' # result <- create_polygon_label(summary_df, "Gemeinde")
#'
#' @export
create_polygon_label <- function(data, Verwaltungsebene) {
  # Required columns
  base_cols <- c("Mittlere_Gewichtete_Fahrzeit", "Einwohner_Gesamt")
  threshold_cols <- c("Anzahl_Betroffene", "Prozent_Betroffene")
  has_threshold_data <- all(threshold_cols %in% names(data))

  # Error handling for required columns
  missing_base <- setdiff(base_cols, names(data))
  if (length(missing_base) > 0) {
    stop("Missing required columns: ", paste(missing_base, collapse = ", "))
  }

  # Format columns
  cols_to_format <- if (has_threshold_data) {
    c(base_cols, threshold_cols)
  } else {
    base_cols
  }
  result <- format_label_columns(data, cols_to_format)

  # Create the HTML label based on available data
  if (has_threshold_data) {
    result <- result |>
      mutate(
        label = paste0(
          "<strong>",
          {{ Verwaltungsebene }},
          "</strong>",
          "<br/>Fahrzeit in min: ",
          Mittlere_Gewichtete_Fahrzeit_formatted,
          "<br/>Einwohner:innen: ",
          Einwohner_Gesamt_formatted,
          "<br/>Betroffene Einwohner:innen: ",
          Anzahl_Betroffene_formatted,
          "<br/>Prozent Betroffene: ",
          Prozent_Betroffene_formatted,
          "%"
        )
      )
  } else {
    result <- result |>
      mutate(
        label = paste0(
          "<strong>",
          {{ Verwaltungsebene }},
          "</strong>",
          "<br/>Fahrzeit in min: ",
          Mittlere_Gewichtete_Fahrzeit_formatted,
          "<br/>Einwohner:innen: ",
          Einwohner_Gesamt_formatted
        )
      )
  }

  result <- result |>
    mutate(label = as.character(label))

  tibble::as_tibble(result)
}

#' Calculate Travel Time Summary for a Hospital Scenario
#'
#' Calculates travel time statistics for a given set of hospitals (scenario),
#' finding the minimum travel time to any hospital in the scenario for each
#' grid cell, then aggregating by administrative level.
#'
#' This function queries a DuckDB Komplettexport database to:
#' \enumerate{
#'   \item Filter travel times to only include hospitals in the scenario
#'   \item Find the nearest hospital (minimum travel time) for each grid cell
#'   \item Enrich with population and administrative data
#'   \item Aggregate statistics by administrative level
#' }
#'
#' @param krankenhaus_standortnummern_csv_path Path to CSV file containing
#'   a column `Krankenhaus_Standortnummer` with hospital site numbers.
#' @param con DuckDB connection to a Komplettexport database. The database must
#'   contain tables: Entfernungsdaten, Gitterzellen_mit_Einwohnern_Gemeinde_Mapping,
#'   and Verwaltungsgebiete_Mapping.
#' @param Grenzwert_Minuten Numeric threshold in minutes. Population with
#'   travel time exceeding this threshold is counted as "affected" (default: 30).
#' @param Verwaltungsebene Character string specifying aggregation level:
#'   "Gemeinde", "Kreis", "Regierungsbezirk", or "Bundesland" (default: "Gemeinde").
#' @param entfernungsdaten_table Character string specifying which distance table
#'   to use: `"Entfernungsdaten"` (default) or `"Entfernungsdaten_subset"`. Only
#'   these two values are accepted; any other value raises an error.
#' @param gitterzellen_layout Character string specifying the layout of the
#'   Gitterzellen tables in the database. Use
#'   `"Gemeinde_and_Einwohner_mapping_combined"` (default) when the database
#'   contains a single pre-joined `Gitterzellen_mit_Einwohnern_Gemeinde_Mapping`
#'   table (e.g. a Komplettexport database). Use
#'   `"Gemeinde_and_Einwohner_mapping_split"` when the database has the two
#'   separate source tables `Gitterzellen_Gemeinde_Mapping` and
#'   `Gitterzellen_Einwohner_Mapping` (e.g. a Klinikfilter-produced database).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{<ID column>}{Administrative unit ID (e.g., Gemeindeschluessel)}
#'   \item{<Name column>}{Administrative unit name (e.g., Gemeindename)}
#'   \item{Bundesland}{State name (always included for context)}
#'   \item{Bundesland_ID}{State ID (always included for context)}
#'   \item{Einwohner_Gesamt}{Total population in the unit}
#'   \item{Mittlere_Gewichtete_Fahrzeit}{Population-weighted mean travel time in minutes}
#'   \item{Anzahl_Betroffene}{Population exceeding the threshold}
#'   \item{Prozent_Betroffene}{Percentage of population exceeding threshold}
#'   \item{Anzahl_Gitterzellen}{Number of grid cells covered by the scenario}
#'   \item{Anzahl_bewohnte_Gitterzellen}{Total inhabited grid cells in the unit}
#'   \item{Prozent_Abgedeckt_pro_Gemeinde}{Percentage of inhabited grid cells covered by the scenario}
#' }
#'
#' @examples
#' \dontrun{
#' con <- connect_to_duckdb_db("data/nobackup/Komplettexport.duckdb")
#' result <- Szenario_Berechnung(
#'   krankenhaus_standortnummern_csv_path = "data/scenario_hospitals.csv",
#'   con = con,
#'   Grenzwert_Minuten = 30,
#'   Verwaltungsebene = "Gemeinde"
#' )
#' dbDisconnect(con, shutdown = TRUE)
#' }
#'
#' @export
Szenario_Berechnung <- function(
  krankenhaus_standortnummern_csv_path,
  con,
  Grenzwert_Minuten = 30,
  Verwaltungsebene = c("Gemeinde", "Kreis", "Regierungsbezirk", "Bundesland"),
  entfernungsdaten_table = c("Entfernungsdaten", "Entfernungsdaten_subset"),
  gitterzellen_layout = c(
    "Gemeinde_and_Einwohner_mapping_combined",
    "Gemeinde_and_Einwohner_mapping_split"
  )
) {
  # Validate inputs
  Verwaltungsebene <- match.arg(Verwaltungsebene)
  entfernungsdaten_table <- match.arg(entfernungsdaten_table)
  gitterzellen_layout <- match.arg(gitterzellen_layout)
  require_file_exists(krankenhaus_standortnummern_csv_path)

  # Read scenario CSV
  krankenhaus_ids <- read_csv(
    krankenhaus_standortnummern_csv_path,
    col_types = cols(Krankenhaus_Standortnummer = col_character())
  )
  if (!tibble::is_tibble(krankenhaus_ids)) {
    krankenhaus_ids <- as_tibble(krankenhaus_ids)
  }
  if (!"Krankenhaus_Standortnummer" %in% names(krankenhaus_ids)) {
    stop("CSV must contain column 'Krankenhaus_Standortnummer'")
  }

  # Register scenario as temporary table in DuckDB
  dbWriteTable(
    con,
    "temp_scenario",
    krankenhaus_ids,
    temporary = TRUE,
    overwrite = TRUE
  )

  # Quote the validated table name for safe injection into SQL
  entfernungsdaten_quoted <- DBI::dbQuoteIdentifier(con, entfernungsdaten_table)

  # Build the Gitterzellen SQL fragment based on layout
  gitterzellen_sql <- if (
    gitterzellen_layout == "Gemeinde_and_Einwohner_mapping_combined"
  ) {
    '"Gitterzellen_mit_Einwohnern_Gemeinde_Mapping"'
  } else {
    '(SELECT g."Gitterzellen_ID", g."Gemeindeschluessel", e."Einwohner"
      FROM "Gitterzellen_Gemeinde_Mapping" g
      LEFT JOIN "Gitterzellen_Einwohner_Mapping" e
        ON g."Gitterzellen_ID" = e."Gitterzellen_ID")'
  }

  # Query: find minimum travel time per grid cell for scenario hospitals
  query <- paste0(
    '
    WITH joined_data AS (
        SELECT
            e."Krankenhaus_Standortnummer",
            e."Gitterzellen_ID",
            e."Fahrzeit_Sekunden"
        FROM temp_scenario ts
        INNER JOIN ',
    entfernungsdaten_quoted,
    ' e
            ON ts."Krankenhaus_Standortnummer" = e."Krankenhaus_Standortnummer"
    ),
    min_times AS (
        SELECT
            "Gitterzellen_ID",
            MIN("Fahrzeit_Sekunden") AS min_fahrzeit
        FROM joined_data
        GROUP BY "Gitterzellen_ID"
    ),
    filtered_data AS (
        SELECT
            jd."Krankenhaus_Standortnummer",
            jd."Gitterzellen_ID",
            jd."Fahrzeit_Sekunden"
        FROM joined_data jd
        INNER JOIN min_times mt ON
            jd."Gitterzellen_ID" = mt."Gitterzellen_ID" AND
            jd."Fahrzeit_Sekunden" = mt.min_fahrzeit
    )
    SELECT
        fd."Krankenhaus_Standortnummer",
        fd."Gitterzellen_ID",
        fd."Fahrzeit_Sekunden",
        ge."Einwohner",
        ge."Gemeindeschluessel"
    FROM filtered_data fd
    LEFT JOIN ',
    gitterzellen_sql,
    ' ge
        ON fd."Gitterzellen_ID" = ge."Gitterzellen_ID"
  '
  )

  grid_data <- as_tibble(dbGetQuery(con, query))

  # Determine grouping columns based on Verwaltungsebene
  grouping_info <- list(
    Gemeinde = list(
      id_col = "Gemeindeschluessel",
      name_col = "Gemeindename"
    ),
    Kreis = list(
      id_col = "Kreis_ID",
      name_col = "Kreis"
    ),
    Regierungsbezirk = list(
      id_col = "Regierungsbezirk_ID",
      name_col = "Regierungsbezirk"
    ),
    Bundesland = list(
      id_col = "Bundesland_ID",
      name_col = "Bundesland"
    )
  )

  id_col <- grouping_info[[Verwaltungsebene]]$id_col
  name_col <- grouping_info[[Verwaltungsebene]]$name_col

  # Get Verwaltungsgebiete_Mapping for joining
  verwaltung <- as_tibble(dbGetQuery(
    con,
    'SELECT * FROM "Verwaltungsgebiete_Mapping"'
  ))

  # Join grid data with administrative mapping
  grid_data <- grid_data |>
    left_join(
      verwaltung |>
        select(
          Gemeindeschluessel,
          all_of(c(id_col, name_col, "Bundesland", "Bundesland_ID"))
        ),
      by = "Gemeindeschluessel"
    )

  # Calculate total inhabited grid cells per administrative unit
  # (This replaces mv_bewohnte_gitterzellen_pro_gemeinde)
  bewohnte_gitterzellen <- as_tibble(dbGetQuery(
    con,
    paste0(
      'SELECT
        vm."',
      id_col,
      '",
        COUNT(DISTINCT ge."Gitterzellen_ID") AS "Anzahl_bewohnte_Gitterzellen"
      FROM ',
      gitterzellen_sql,
      ' ge
      LEFT JOIN "Verwaltungsgebiete_Mapping" vm
        ON ge."Gemeindeschluessel" = vm."Gemeindeschluessel"
      GROUP BY vm."',
      id_col,
      '"'
    )
  ))

  # Aggregate using Fahrzeit_Zusammenfassung
  summary <- grid_data |>
    Fahrzeit_Zusammenfassung(
      .by = all_of(c(id_col, name_col, "Bundesland", "Bundesland_ID")),
      Grenzwert_Minuten = Grenzwert_Minuten
    )

  # Join with bewohnte_gitterzellen and calculate Prozent_Abgedeckt_pro_Gemeinde
  result <- summary |>
    left_join(bewohnte_gitterzellen, by = id_col) |>
    mutate(
      Prozent_Abgedeckt_pro_Gemeinde = Anzahl_Gitterzellen /
        Anzahl_bewohnte_Gitterzellen *
        100
    )

  tibble::as_tibble(result)
}
