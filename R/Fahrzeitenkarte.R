utils::globalVariables(c(
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
  "Mittlere_Gewichtete_Fahrzeit_Differenz_formatted"
))


#' Summarize Weighted Travel Times
#'
#' Calculates the weighted mean travel time and, if a threshold is given, the number and percentage of people exceeding it.
#'
#' The input `data` is expected to be a tibble (as returned by dplyr/tidyverse workflows).
#'
#' @param data A tibble with travel times and population columns.
#' @param .by Grouping columns (tidyselect, e.g. c(Gemeindename, Gemeindeschluessel)).
#' @param Grenzwert_Minuten Optional numeric threshold (in minutes) for "affected" population.
#'
#' @return A tibble summarizing, for each group, the total population, weighted mean travel time, and, if threshold is set, the number and percent affected.
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
