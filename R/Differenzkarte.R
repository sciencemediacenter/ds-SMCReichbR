utils::globalVariables(c(
    "Fahrzeit_Sekunden_Szenario",
    "Fahrzeit_Sekunden_Referenz"
)) # created during left_join

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


#' Summarize Weighted Travel Time Differences
#'
#' Calculates the weighted mean difference in travel times between a reference and a scenario dataset,
#' summarizing by municipality or other grouping variables. Also computes the total population,
#' the number and percentage of people affected by increased travel times.
#'
#' The inputs `reference_data` and `scenario_data` are expected to be tibbles (as returned by dplyr/tidyverse workflows).
#'
#' @param reference_data A tibble with reference travel times and grouping columns.
#' @param scenario_data A tibble with scenario travel times and grouping columns.
#' @param by Character vector of column names to join and group by (default: typical spatial and population columns).
#'
#' @return A tibble summarizing, for each group, the total population, weighted mean travel time difference,
#' number of affected people, and percent affected.
#'
#' @examples
#' # summary <- Fahrzeit_Differenz_Zusammenfassung(ref_df, scen_df)
#'
#' @export
Fahrzeit_Differenz_Zusammenfassung <- function(
    reference_data,
    scenario_data,
    by = c(
        "Gitterzellen_ID",
        "Einwohner",
        "Gemeindename",
        "Gemeindeschluessel",
        "Bundesland",
        "Regierungsbezirk",
        "Kreis",
        "Bundesland_ID",
        "Regierungsbezirk_ID",
        "Kreis_ID",
        "Gemeinde_ID",
        "Datum_Gitter"
    )
) {
    result <- reference_data |>
        left_join(
            scenario_data,
            by = by,
            suffix = c("_Referenz", "_Szenario")
        ) |>
        # Differenz: Szenario (tendenziell längere Fahrzeiten) - Referenz
        mutate(
            Fahrzeit_Differenz_Minuten = (Fahrzeit_Sekunden_Szenario -
                Fahrzeit_Sekunden_Referenz) /
                60
        )
    summary <- result |>
        summarise(
            Einwohner_Gesamt = sum(Einwohner, na.rm = TRUE),
            Mittlere_Gewichtete_Fahrzeit_Differenz = weighted.mean(
                Fahrzeit_Differenz_Minuten,
                w = Einwohner,
                na.rm = TRUE
            ),
            Anzahl_Betroffene = sum(
                Einwohner[Fahrzeit_Differenz_Minuten > 0],
                na.rm = TRUE
            ),
            Prozent_Betroffene = Anzahl_Betroffene / Einwohner_Gesamt * 100,
            .by = {{ .by }}
        )

    tibble::as_tibble(summary)
}

#' Create HTML Labels for Travel Time Difference Polygons
#'
#' Generates formatted HTML labels for map polygons summarizing travel time differences and population statistics.
#' The function formats numeric columns and conditionally includes affected population statistics if present.
#'
#' @param data A tibble containing summary statistics, including at least columns for weighted mean travel time difference and total population.
#' @param Verwaltungsebene A string or symbol indicating the administrative level (e.g., municipality, district) to display in the label.
#'
#' @return A tibble with an added `label` column containing HTML-formatted labels for use in interactive maps.
#'
#' @examples
#' # result <- create_polygon_label_differenz(summary_df, "Gemeinde")
#'
#' @export
create_polygon_label_differenz <- function(data, Verwaltungsebene) {
    # Define columns to format
    base_cols <- c("Mittlere_Gewichtete_Fahrzeit_Differenz", "Einwohner_Gesamt")
    threshold_cols <- c("Anzahl_Betroffene", "Prozent_Betroffene")
    has_threshold_data <- all(threshold_cols %in% names(data))

    # Check for required columns
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
                    "<br/>Fahrzeit-Differenz in min: ",
                    Mittlere_Gewichtete_Fahrzeit_Differenz_formatted,
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
                    "<br/>Fahrzeit-Differenz in min: ",
                    Mittlere_Gewichtete_Fahrzeit_Differenz_formatted,
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
