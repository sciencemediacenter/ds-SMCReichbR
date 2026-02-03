utils::globalVariables(c(
  ".data",
  "Einwohner",
  "Fahrzeit_Differenz_Minuten",
  "Anzahl_Betroffene",
  "Einwohner_Gesamt",
  ".by",
  "Fahrzeit_Sekunden",
  "Fahrzeit_Minuten",
  "Ueber_Grenzwert",
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
  "Prozent_Abgedeckt",
  "Bundesland",
  "Bundesland_ID"
))


#' Summarize Weighted Travel Times
#'
#' Calculates the weighted mean travel time and, if a threshold is given, the number and percentage of people exceeding it.
#'
#' The input `data` is expected to be a tibble (as returned by dplyr/tidyverse workflows).
#'
#' @param data A tibble with travel times and population columns. Must include columns:
#'   Fahrzeit_Sekunden, Einwohner, Gitterzellen_ID.
#' @param .by Grouping columns (tidyselect, e.g. c(Gemeindename, Gemeindeschluessel)).
#' @param Grenzwert_Minuten Optional numeric threshold (in minutes) for "affected" population.
#'
#' @return A tibble summarizing, for each group, the total population, weighted mean travel time,
#'   number of grid cells, and, if threshold is set, the number and percent affected.
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
  result <- data |>
    mutate(
      Fahrzeit_Minuten = Fahrzeit_Sekunden / 60
    )

  if (!is.null(Grenzwert_Minuten)) {
    summary <- result |>
      mutate(Ueber_Grenzwert = Fahrzeit_Minuten > Grenzwert_Minuten) |>
      summarise(
        Einwohner_Gesamt = sum(Einwohner, na.rm = TRUE),
        Mittlere_Gewichtete_Fahrzeit = weighted.mean(
          Fahrzeit_Minuten,
          w = Einwohner,
          na.rm = TRUE
        ),
        Anzahl_Betroffene = sum(Einwohner[Ueber_Grenzwert], na.rm = TRUE),
        Prozent_Betroffene = Anzahl_Betroffene / Einwohner_Gesamt * 100,
        Anzahl_Gitterzellen = dplyr::n_distinct(Gitterzellen_ID),
        .by = {{ .by }}
      )
  } else {
    summary <- result |>
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
  }

  tibble::as_tibble(summary)
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

  # Convert to HTML tags row by row
  result <- result |>
    rowwise() |>
    mutate(label = HTML(label))

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
#'   \item{Prozent_Abgedeckt}{Percentage of inhabited grid cells covered by the scenario}
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
  Verwaltungsebene = c("Gemeinde", "Kreis", "Regierungsbezirk", "Bundesland")
) {
  # Validate inputs
  Verwaltungsebene <- match.arg(Verwaltungsebene)
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
  dbWriteTable(con, "temp_scenario", krankenhaus_ids, temporary = TRUE, overwrite = TRUE)
  
  # Query: find minimum travel time per grid cell for scenario hospitals
  query <- '
    WITH joined_data AS (
        SELECT
            e."Krankenhaus_Standortnummer",
            e."Gitterzellen_ID",
            e."Fahrzeit_Sekunden"
        FROM temp_scenario ts
        INNER JOIN "Entfernungsdaten" e 
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
    LEFT JOIN "Gitterzellen_mit_Einwohnern_Gemeinde_Mapping" ge
        ON fd."Gitterzellen_ID" = ge."Gitterzellen_ID"
  '
  
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
  verwaltung <- as_tibble(dbGetQuery(con, 'SELECT * FROM "Verwaltungsgebiete_Mapping"'))
  
  # Join grid data with administrative mapping
  grid_data <- grid_data |>
    left_join(
      verwaltung |> select(Gemeindeschluessel, all_of(c(id_col, name_col, "Bundesland", "Bundesland_ID"))),
      by = "Gemeindeschluessel"
    )
  
  # Calculate total inhabited grid cells per administrative unit
  # (This replaces mv_bewohnte_gitterzellen_pro_gemeinde)
  bewohnte_gitterzellen <- as_tibble(dbGetQuery(con, glue('
    SELECT 
      vm."{id_col}",
      COUNT(DISTINCT ge."Gitterzellen_ID") AS "Anzahl_bewohnte_Gitterzellen"
    FROM "Gitterzellen_mit_Einwohnern_Gemeinde_Mapping" ge
    LEFT JOIN "Verwaltungsgebiete_Mapping" vm
      ON ge."Gemeindeschluessel" = vm."Gemeindeschluessel"
    GROUP BY vm."{id_col}"
  ')))
  
  # Aggregate using Fahrzeit_Zusammenfassung
  summary <- grid_data |>
    Fahrzeit_Zusammenfassung(
      .by = all_of(c(id_col, name_col, "Bundesland", "Bundesland_ID")),
      Grenzwert_Minuten = Grenzwert_Minuten
    )
  
  # Join with bewohnte_gitterzellen and calculate Prozent_Abgedeckt
  result <- summary |>
    left_join(bewohnte_gitterzellen, by = id_col) |>
    mutate(
      Prozent_Abgedeckt = Anzahl_Gitterzellen / Anzahl_bewohnte_Gitterzellen * 100
    )
  
  tibble::as_tibble(result)
}
