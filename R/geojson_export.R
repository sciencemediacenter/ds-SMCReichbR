utils::globalVariables(c(
  "Mittlere_Gewichtete_Fahrzeit",
  "Gemeindename",
  "fahrtzeit_scenario_1",
  "fahrtzeit_scenario_1_str",
  "fahrtzeit_scenario_2",
  "fahrtzeit_scenario_2_str",
  "fahrtzeit_difference",
  "fahrtzeit_difference_str",
  "AGS",
  "GEN",
  "EWZ",
  "geometry",
  "Bezeichnung",
  "Träger",
  "Adresse",
  "Bundesland",
  "Kreis",
  "Krankenhaus_Standortnummer",
  "Krankenhaus_Standort_ID"
))


# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------

#' Format Travel Time for GeoJSON Output
#'
#' Formats a numeric travel time (in minutes) to a display string such as
#' `"12,3 Minuten"`. Returns `"keine Daten"` for `NA` values.
#'
#' Aligned with `format_fahrtzeit()` in the Python API
#' (`intermediate_scripts/number_formatting.py`):
#' uses space as thousand separator, comma as decimal separator, and
#' `" Minuten"` as suffix.
#'
#' @param x Numeric vector of travel times in minutes.
#' @param digits Number of decimal places (default 1).
#'
#' @return Character vector of formatted travel time strings.
#'
#' @examples
#' format_fahrtzeit(12.345)
#' format_fahrtzeit(NA)
#'
#' @export
format_fahrtzeit <- function(x, digits = 1L) {
  ifelse(
    is.na(x),
    "keine Daten",
    paste0(
      formatC(
        x,
        format = "f",
        digits = digits,
        big.mark = " ",
        decimal.mark = ","
      ),
      " Minuten"
    )
  )
}


#' Format Population Count for GeoJSON Output
#'
#' Formats a numeric population count with an HTML thin-space as thousand
#' separator. Returns `"keine Daten"` for `NA` values.
#'
#' Aligned with `format_EWZ()` in the Python API
#' (`intermediate_scripts/number_formatting.py`):
#' uses `<span style="font-size:50%;"> </span>` as thousand separator
#' and comma as decimal separator. Integer values use 0 decimals; non-integer
#' values use 2 decimals.
#'
#' @param x Numeric vector of population counts.
#'
#' @return Character vector of formatted population strings (may contain HTML).
#'
#' @examples
#' format_ewz(1471508)
#' format_ewz(NA)
#'
#' @export
format_ewz <- function(x) {
  small_space <- '<span style="font-size:50%;"> </span>'
  mapply(
    function(val) {
      if (is.na(val)) {
        return("keine Daten")
      }
      if (val == floor(val)) {
        formatC(
          val,
          format = "f",
          digits = 0,
          big.mark = small_space,
          decimal.mark = ","
        )
      } else {
        formatC(
          val,
          format = "f",
          digits = 2,
          big.mark = small_space,
          decimal.mark = ","
        )
      }
    },
    x,
    USE.NAMES = FALSE
  )
}


# ---------------------------------------------------------------------------
# Coverage filter
# ---------------------------------------------------------------------------

#' Filter Scenario Tibble by Grid Cell Coverage
#'
#' Retains only administrative units where the percentage of inhabited grid
#' cells covered by the scenario is at or above a threshold. This mirrors the
#' Python API filter `Prozent_Abgedeckt_pro_Gemeinde >= 99`.
#'
#' @param szenario_tibble A tibble as returned by [Szenario_Berechnung()],
#'   must contain a `Prozent_Abgedeckt_pro_Gemeinde` column.
#' @param grenzwert_abdeckung Numeric threshold in percent (default 99).
#'
#' @return Filtered tibble.
#'
#' @export
filter_by_coverage <- function(szenario_tibble, grenzwert_abdeckung = 99) {
  stopifnot("Prozent_Abgedeckt_pro_Gemeinde" %in% names(szenario_tibble))
  dplyr::filter(
    szenario_tibble,
    Prozent_Abgedeckt_pro_Gemeinde >= grenzwert_abdeckung
  )
}


# ---------------------------------------------------------------------------
# Property builders
# ---------------------------------------------------------------------------

#' Build GeoJSON Feature Properties for a Single Scenario
#'
#' Selects and renames the columns from the scenario tibble that map to the
#' GeoJSON `FeatureCalculateProperties` schema used by the Python API.
#'
#' @param szenario_tibble Filtered tibble from [filter_by_coverage()].
#'
#' AGS: `Gemeindeschluessel`
#' GEN: `Gemeindename`
#' EWZ: `Einwohner_Gesamt` (formatted with `format_ewz()`)
#'
#' @return A tibble with columns `AGS`, `GEN`, `EWZ`,
#'   `fahrtzeit_scenario_1`, `fahrtzeit_scenario_1_str`.
#'
#' @export
build_scenario_properties <- function(szenario_tibble) {
  required <- c(
    "Gemeindeschluessel",
    "Gemeindename",
    "Einwohner_Gesamt",
    "Mittlere_Gewichtete_Fahrzeit"
  )
  missing <- setdiff(required, names(szenario_tibble))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  szenario_tibble |>
    dplyr::transmute(
      AGS = Gemeindeschluessel,
      GEN = Gemeindename,
      EWZ = format_ewz(Einwohner_Gesamt),
      fahrtzeit_scenario_1 = Mittlere_Gewichtete_Fahrzeit,
      fahrtzeit_scenario_1_str = format_fahrtzeit(Mittlere_Gewichtete_Fahrzeit)
    )
}


#' Add Empty Scenario-2 and Difference Columns
#'
#' Adds `NA`-valued columns for scenario 2 and the difference, matching the
#' `FeatureCalculateProperties` schema for single-scenario output.
#'
#' @param props_tibble Tibble returned by [build_scenario_properties()].
#'
#' @return The same tibble with additional columns:
#'   `fahrtzeit_scenario_2`, `fahrtzeit_scenario_2_str`,
#'   `fahrtzeit_difference`, `fahrtzeit_difference_str`.
#'
#' @export
add_empty_scenario2_columns <- function(props_tibble) {
  props_tibble |>
    dplyr::mutate(
      fahrtzeit_scenario_2 = NA_real_,
      fahrtzeit_scenario_2_str = NA_character_,
      fahrtzeit_difference = NA_real_,
      fahrtzeit_difference_str = NA_character_
    )
}


# ---------------------------------------------------------------------------
# Join scenario data onto polygon geometries
# ---------------------------------------------------------------------------

#' Join Scenario Properties onto Gemeinde Polygons
#'
#' Left-joins the scenario property tibble onto the full set of Gemeinde
#' polygon geometries. Municipalities not covered by the scenario receive
#' `NA` / `null` property values.
#'
#' @param gemeinde_sf An `sf` object with at least columns `Gemeindeschluessel`
#'   and a geometry column. If it contains a `Gemeindename` column, it is used
#'   as fallback for `GEN` when the scenario tibble did not cover a municipality.
#' @param scenario_props Tibble returned by [build_scenario_properties()] (or
#'   after [add_empty_scenario2_columns()]).
#'
#' @return An `sf` object with one row per polygon and all
#'   `FeatureCalculateProperties` columns.
#'
#' @export
join_properties_to_polygons <- function(gemeinde_sf, scenario_props) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required but not installed.")
  }
  stopifnot("Gemeindeschluessel" %in% names(gemeinde_sf))

  # Remember if polygons carry a name column we can use as fallback

  has_name_col <- "Gemeindename" %in% names(gemeinde_sf)

  joined <- gemeinde_sf |>
    dplyr::left_join(scenario_props, by = c("Gemeindeschluessel" = "AGS"))

  # Fill GEN from polygon's Gemeindename where scenario did not cover it

  if (has_name_col) {
    joined <- joined |>
      dplyr::mutate(GEN = ifelse(is.na(GEN), Gemeindename, GEN))
  }

  # Ensure AGS is always populated from the polygon key
  joined <- joined |>
    dplyr::mutate(AGS = Gemeindeschluessel)

  # Fill formatted columns for uncovered municipalities
  joined <- joined |>
    dplyr::mutate(
      EWZ = ifelse(is.na(EWZ), NA_character_, EWZ),
      fahrtzeit_scenario_1_str = ifelse(
        is.na(fahrtzeit_scenario_1_str),
        NA_character_,
        fahrtzeit_scenario_1_str
      )
    )

  joined
}


# ---------------------------------------------------------------------------
# GeoJSON assembly
# ---------------------------------------------------------------------------

#' Build CRS Metadata for GeoJSON
#'
#' Returns the CRS block used in the Python API's FeatureCollection output.
#'
#' @return A named list representing the CRS in OGC format.
#'
#' @export
build_crs_block <- function() {
  list(
    type = "name",
    properties = list(name = "urn:ogc:def:crs:OGC:1.3:CRS84")
  )
}


#' Build Scenario Summary Metadata
#'
#' Creates the `scenario1` (or `scenario2`) metadata block for the
#' FeatureCollection, containing the list of hospital IDs and coverage stats.
#'
#' @param scenario_ids Character vector of hospital IDs used in the scenario.
#' @param percentage_covered Numeric percentage of total grid cells covered.
#' @param created_at Timestamp string (default: current time).
#'
#' @return A named list matching the `ScenarioSummary` schema.
#'
#' @export
build_scenario_summary <- function(
  scenario_ids = character(0),
  percentage_covered = NULL,
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
) {
  list(
    list = as.list(scenario_ids),
    hash = NA_character_,
    percentage_covered = percentage_covered,
    created_at = created_at
  )
}


#' Compute Percentage of Grid Cells Covered
#'
#' Calculates the global coverage percentage from the scenario tibble, i.e.
#' the sum of grid cells in the scenario divided by total inhabited grid cells.
#'
#' @param szenario_tibble A tibble with `Anzahl_Gitterzellen` and
#'   `Anzahl_bewohnte_Gitterzellen` columns (before coverage filtering).
#'
#' @return Numeric scalar: percentage of total inhabited grid cells covered.
#'
#' @export
compute_percentage_covered <- function(szenario_tibble) {
  required <- c("Anzahl_Gitterzellen", "Anzahl_bewohnte_Gitterzellen")
  missing <- setdiff(required, names(szenario_tibble))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  total_scenario <- sum(szenario_tibble$Anzahl_Gitterzellen, na.rm = TRUE)
  total_inhabited <- sum(
    szenario_tibble$Anzahl_bewohnte_Gitterzellen,
    na.rm = TRUE
  )

  if (total_inhabited == 0) {
    return(0)
  }
  total_scenario / total_inhabited * 100
}


#' Convert sf Object to GeoJSON Feature List
#'
#' Converts an `sf` object with the target property columns into a list of
#' GeoJSON Feature objects (as R lists). Uses `sf::st_as_sf()` geometry
#' serialization under the hood.
#'
#' @param sf_data An `sf` object with the `FeatureCalculateProperties` columns
#'   and geometry.
#' @param property_cols Character vector of column names to include as Feature
#'   properties.
#'
#' @return A list of Feature lists, each with `type`, `properties`, and
#'   `geometry` elements.
#'
#' @export
sf_to_feature_list <- function(
  sf_data,
  property_cols = c(
    "AGS",
    "GEN",
    "EWZ",
    "fahrtzeit_scenario_1_str",
    "fahrtzeit_scenario_2_str",
    "fahrtzeit_difference_str",
    "fahrtzeit_scenario_1",
    "fahrtzeit_scenario_2",
    "fahrtzeit_difference"
  )
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required but not installed.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required but not installed.")
  }
  if (!requireNamespace("geojsonsf", quietly = TRUE)) {
    stop("Package 'geojsonsf' is required but not installed.")
  }

  # Ensure WGS84
  sf_data <- sf::st_transform(sf_data, 4326)

  # Pre-serialize all geometries to GeoJSON strings in one vectorized call
  geom_jsons <- geojsonsf::sfc_geojson(sf::st_geometry(sf_data))

  n <- nrow(sf_data)
  features <- vector("list", n)

  for (i in seq_len(n)) {
    row <- sf_data[i, ]

    # Build properties list, converting NA to NULL for JSON null
    props <- lapply(
      stats::setNames(property_cols, property_cols),
      function(col) {
        val <- row[[col]]
        if (length(val) == 0 || is.na(val)) NULL else val
      }
    )

    # Parse pre-serialized geometry JSON string to R list
    geom <- jsonlite::fromJSON(geom_jsons[[i]], simplifyVector = FALSE)

    features[[i]] <- list(
      type = "Feature",
      properties = props,
      geometry = geom
    )
  }

  features
}


#' Assemble a FeatureCollection from Features and Metadata
#'
#' Combines a feature list and scenario metadata into the final
#' `FeatureCollectionCalculate` structure matching the Python API schema.
#'
#' @param features List of Feature lists (from [sf_to_feature_list()]).
#' @param scenario1_summary Named list from [build_scenario_summary()].
#' @param scenario2_summary Named list from [build_scenario_summary()]
#'   (default: empty list for single-scenario output).
#'
#' @return A named list representing the full GeoJSON FeatureCollection.
#'
#' @export
assemble_feature_collection <- function(
  features,
  scenario1_summary,
  scenario2_summary = list()
) {
  list(
    type = "FeatureCollection",
    crs = build_crs_block(),
    features = features,
    mainHash = NA_character_,
    scenario1 = scenario1_summary,
    scenario2 = scenario2_summary
  )
}


# ---------------------------------------------------------------------------
# Main orchestrator
# ---------------------------------------------------------------------------

#' Convert Szenario_Berechnung Output to GeoJSON
#'
#' Transforms the tibble returned by [Szenario_Berechnung()] (at Gemeinde
#' level) into a GeoJSON FeatureCollection matching the structure produced by
#' the Python `/calculate` endpoint.
#'
#' The pipeline:
#' 1. Filter by coverage threshold ([filter_by_coverage()])
#' 2. Build scenario properties ([build_scenario_properties()])
#' 3. Add empty scenario-2 columns ([add_empty_scenario2_columns()])
#' 4. Join onto polygon geometries ([join_properties_to_polygons()])
#' 5. Convert to GeoJSON feature list ([sf_to_feature_list()])
#' 6. Assemble FeatureCollection ([assemble_feature_collection()])
#'
#' @param szenario_tibble A tibble as returned by [Szenario_Berechnung()]
#'   with `Verwaltungsebene = "Gemeinde"`.
#' @param gemeinde_sf An `sf` object of Gemeinde polygon geometries with at
#'   least a `Gemeindeschluessel` column. Optionally a `Gemeindename` column
#'   for fallback names.
#' @param scenario_ids Character vector of hospital `Standortnummer` IDs used
#'   to create the scenario (default: empty).
#' @param grenzwert_abdeckung Numeric coverage threshold in percent (default 99).
#' @param as_json Logical; if `TRUE` (default), return a JSON string. If
#'   `FALSE`, return the R list structure.
#'
#' @return Either a JSON string or a named list representing a GeoJSON
#'   `FeatureCollectionCalculate`.
#'
#' @examples
#' # geojson <- szenario_to_geojson(
#' #   szenario_tibble = Szenario_Berechnung(con, standort_nummern),
#' #   gemeinde_sf = my_gemeinde_polygons,
#' #   scenario_ids = standort_nummern
#' # )
#'
#' @export
szenario_to_geojson <- function(
  szenario_tibble,
  gemeinde_sf,
  scenario_ids = character(0),
  grenzwert_abdeckung = 99,
  as_json = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required but not installed.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required but not installed.")
  }

  # 1. Compute global coverage before filtering
  pct_covered <- compute_percentage_covered(szenario_tibble)

  # 2. Filter by coverage threshold
  filtered <- filter_by_coverage(szenario_tibble, grenzwert_abdeckung)

  # 3. Build scenario properties
  props <- build_scenario_properties(filtered)
  props <- add_empty_scenario2_columns(props)

  # 4. Join onto polygon geometries
  sf_joined <- join_properties_to_polygons(gemeinde_sf, props)

  # 5. Convert to feature list
  features <- sf_to_feature_list(sf_joined)

  # 6. Assemble FeatureCollection
  scenario1_meta <- build_scenario_summary(
    scenario_ids = scenario_ids,
    percentage_covered = pct_covered
  )

  fc <- assemble_feature_collection(
    features = features,
    scenario1_summary = scenario1_meta
  )

  if (as_json) {
    jsonlite::toJSON(
      fc,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      pretty = TRUE,
      force = TRUE
    )
  } else {
    fc
  }
}


# ---------------------------------------------------------------------------
# Clinics GeoJSON export
# ---------------------------------------------------------------------------

#' Convert Krankenhaus_Standortliste to GeoJSON
#'
#' Transforms `Krankenhaus_Standortliste` (sf tibble) combined with
#' `Verwaltungsgebiete_Mapping` into a GeoJSON FeatureCollection matching
#' the structure produced by the Python `/clinics` endpoint.
#'
#' @param krankenhaus_sf An `sf` tibble from `Krankenhaus_Standortliste` with
#'   at least columns `Krankenhaus_Standortnummer`, `Bezeichnung`, `Träger`,
#'   `Adresse`, `Gemeindeschluessel`, and a `geometry` column (Point, WGS84).
#' @param verwaltungsgebiete A tibble from `Verwaltungsgebiete_Mapping` with
#'   at least columns `Gemeindeschluessel`, `Gemeindename`, `Kreis`,
#'   `Bundesland`.
#' @param as_json Logical; if `TRUE` (default), return a JSON string. If
#'   `FALSE`, return the R list structure.
#'
#' @return Either a JSON string or a named list representing a
#'   `FeatureCollectionClinics` (type, crs, features).
#'
#' @examples
#' # geojson <- kliniken_to_geojson(
#' #   krankenhaus_sf = Krankenhaus_Standortliste,
#' #   verwaltungsgebiete = Verwaltungsgebiete_Mapping
#' # )
#'
#' @export
kliniken_to_geojson <- function(
  krankenhaus_sf,
  verwaltungsgebiete,
  as_json = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required but not installed.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required but not installed.")
  }
  if (!requireNamespace("geojsonsf", quietly = TRUE)) {
    stop("Package 'geojsonsf' is required but not installed.")
  }

  joined <- dplyr::left_join(
    krankenhaus_sf,
    dplyr::select(
      verwaltungsgebiete,
      "Gemeindeschluessel", "Gemeindename", "Kreis", "Bundesland"
    ),
    by = "Gemeindeschluessel"
  ) |>
    dplyr::arrange(Krankenhaus_Standort_ID) |>
    dplyr::distinct(Krankenhaus_Standort_ID, .keep_all = TRUE)

  kliniken_sf <- dplyr::mutate(
    joined,
    id = Krankenhaus_Standortnummer,
    name = dplyr::if_else(
      is.na(Träger),
      Bezeichnung,
      paste0(Bezeichnung, " (", Träger, ")")
    ),
    adress = Adresse,
    bundesland = Bundesland,
    kreis = Kreis,
    gemeinde = Gemeindename
  )

  features <- sf_to_feature_list(
    kliniken_sf,
    property_cols = c("id", "name", "adress", "bundesland", "kreis", "gemeinde")
  )

  fc <- list(
    type = "FeatureCollection",
    crs = build_crs_block(),
    features = features
  )

  if (as_json) {
    jsonlite::toJSON(
      fc,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      pretty = TRUE,
      force = TRUE
    )
  } else {
    fc
  }
}
