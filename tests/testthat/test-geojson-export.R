# tests/testthat/test-geojson-export.R
# Tests for GeoJSON export functions (no database dependencies)

# ============================================================================
# Tests for format_fahrtzeit()
# ============================================================================

test_that("format_fahrtzeit formats numeric values", {
  expect_equal(format_fahrtzeit(12.345), "12,3 Minuten")
})

test_that("format_fahrtzeit returns 'keine Daten' for NA", {
  expect_equal(format_fahrtzeit(NA_real_), "keine Daten")
})

test_that("format_fahrtzeit handles zero", {
  expect_equal(format_fahrtzeit(0), "0,0 Minuten")
})

test_that("format_fahrtzeit respects digits parameter", {
  expect_equal(format_fahrtzeit(12.345, digits = 2L), "12,35 Minuten")
})

test_that("format_fahrtzeit uses space as thousand separator", {
  expect_equal(format_fahrtzeit(1234.5), "1 234,5 Minuten")
})

test_that("format_fahrtzeit is vectorized", {
  result <- format_fahrtzeit(c(10, NA, 20.5))
  expect_equal(result, c("10,0 Minuten", "keine Daten", "20,5 Minuten"))
})


# ============================================================================
# Tests for format_ewz()
# ============================================================================

small_space <- '<span style="font-size:50%;"> </span>'

test_that("format_ewz formats large integers with HTML thin-space separator", {
  expected <- paste0("1", small_space, "471", small_space, "508")
  expect_equal(format_ewz(1471508), expected)
})

test_that("format_ewz returns 'keine Daten' for NA", {
  expect_equal(format_ewz(NA_real_), "keine Daten")
})

test_that("format_ewz handles zero", {
  expect_equal(format_ewz(0), "0")
})

test_that("format_ewz uses 0 decimals for integer values", {
  expected <- paste0("1", small_space, "000")
  expect_equal(format_ewz(1000), expected)
})

test_that("format_ewz uses 2 decimals for non-integer values", {
  expected <- paste0("1", small_space, "000,50")
  expect_equal(format_ewz(1000.5), expected)
})

test_that("format_ewz is vectorized", {
  result <- format_ewz(c(1000, NA, 500))
  expect_equal(result, c(
    paste0("1", small_space, "000"),
    "keine Daten",
    "500"
  ))
})


# ============================================================================
# Tests for filter_by_coverage()
# ============================================================================

test_that("filter_by_coverage filters at default threshold", {
  data <- tibble::tibble(
    Gemeindeschluessel = c("A", "B", "C"),
    Prozent_Abgedeckt_pro_Gemeinde = c(100, 99, 98)
  )

  result <- filter_by_coverage(data)

  expect_equal(nrow(result), 2)
  expect_equal(result$Gemeindeschluessel, c("A", "B"))
})

test_that("filter_by_coverage uses custom threshold", {
  data <- tibble::tibble(
    Gemeindeschluessel = c("A", "B"),
    Prozent_Abgedeckt_pro_Gemeinde = c(95, 90)
  )

  result <- filter_by_coverage(data, grenzwert_abdeckung = 95)

  expect_equal(nrow(result), 1)
  expect_equal(result$Gemeindeschluessel, "A")
})

test_that("filter_by_coverage errors on missing column", {
  data <- tibble::tibble(Gemeindeschluessel = "A")
  expect_error(filter_by_coverage(data))
})


# ============================================================================
# Tests for build_scenario_properties()
# ============================================================================

test_that("build_scenario_properties creates expected columns", {
  data <- tibble::tibble(
    Gemeindeschluessel = "09162000",
    Gemeindename = "München",
    Einwohner_Gesamt = 1471508,
    Mittlere_Gewichtete_Fahrzeit = 12.345
  )

  result <- build_scenario_properties(data)

  expect_equal(names(result), c(
    "AGS", "GEN", "EWZ",
    "fahrtzeit_scenario_1", "fahrtzeit_scenario_1_str"
  ))
  expect_equal(result$AGS, "09162000")
  expect_equal(result$GEN, "München")
  expect_equal(result$fahrtzeit_scenario_1, 12.345)
})

test_that("build_scenario_properties formats EWZ correctly", {
  data <- tibble::tibble(
    Gemeindeschluessel = "A",
    Gemeindename = "Test",
    Einwohner_Gesamt = 50000,
    Mittlere_Gewichtete_Fahrzeit = 10
  )

  result <- build_scenario_properties(data)

  expect_equal(result$EWZ, paste0("50", small_space, "000"))
})

test_that("build_scenario_properties formats fahrtzeit_str correctly", {
  data <- tibble::tibble(
    Gemeindeschluessel = "A",
    Gemeindename = "Test",
    Einwohner_Gesamt = 100,
    Mittlere_Gewichtete_Fahrzeit = 15.67
  )

  result <- build_scenario_properties(data)

  expect_equal(result$fahrtzeit_scenario_1_str, "15,7 Minuten")
})

test_that("build_scenario_properties errors on missing columns", {
  data <- tibble::tibble(Gemeindeschluessel = "A")
  expect_error(build_scenario_properties(data), "Missing required columns")
})


# ============================================================================
# Tests for add_empty_scenario2_columns()
# ============================================================================

test_that("add_empty_scenario2_columns adds NA columns", {
  data <- tibble::tibble(AGS = "A", GEN = "Test", EWZ = "100",
                         fahrtzeit_scenario_1 = 10,
                         fahrtzeit_scenario_1_str = "10,0 Minuten")

  result <- add_empty_scenario2_columns(data)

  expect_true(all(c(
    "fahrtzeit_scenario_2", "fahrtzeit_scenario_2_str",
    "fahrtzeit_difference", "fahrtzeit_difference_str"
  ) %in% names(result)))
  expect_true(all(is.na(result$fahrtzeit_scenario_2)))
  expect_true(all(is.na(result$fahrtzeit_difference)))
})


# ============================================================================
# Tests for compute_percentage_covered()
# ============================================================================

test_that("compute_percentage_covered calculates correctly", {
  data <- tibble::tibble(
    Anzahl_Gitterzellen = c(80, 90),
    Anzahl_bewohnte_Gitterzellen = c(100, 100)
  )

  result <- compute_percentage_covered(data)

  # (80 + 90) / (100 + 100) * 100 = 85
  expect_equal(result, 85)
})

test_that("compute_percentage_covered returns 0 for no inhabited cells", {
  data <- tibble::tibble(
    Anzahl_Gitterzellen = c(10),
    Anzahl_bewohnte_Gitterzellen = c(0)
  )

  expect_equal(compute_percentage_covered(data), 0)
})

test_that("compute_percentage_covered errors on missing columns", {
  data <- tibble::tibble(x = 1)
  expect_error(compute_percentage_covered(data), "Missing required columns")
})


# ============================================================================
# Tests for build_crs_block()
# ============================================================================

test_that("build_crs_block returns correct structure", {
  crs <- build_crs_block()

  expect_equal(crs$type, "name")
  expect_equal(crs$properties$name, "urn:ogc:def:crs:OGC:1.3:CRS84")
})


# ============================================================================
# Tests for build_scenario_summary()
# ============================================================================

test_that("build_scenario_summary returns correct structure", {
  result <- build_scenario_summary(
    scenario_ids = c("770012345", "770067890"),
    percentage_covered = 85.5
  )

  expect_equal(result$list, list("770012345", "770067890"))
  expect_equal(result$percentage_covered, 85.5)
  expect_true(is.na(result$hash))
  expect_true(!is.null(result$created_at))
})

test_that("build_scenario_summary handles empty ids", {
  result <- build_scenario_summary()

  expect_equal(result$list, list())
  expect_null(result$percentage_covered)
})


# ============================================================================
# Tests for assemble_feature_collection()
# ============================================================================

test_that("assemble_feature_collection creates valid structure", {
  features <- list(
    list(type = "Feature", properties = list(AGS = "A"), geometry = list(type = "Point", coordinates = c(0, 0)))
  )
  s1 <- build_scenario_summary(c("id1"), 90)

  result <- assemble_feature_collection(features, s1)

  expect_equal(result$type, "FeatureCollection")
  expect_equal(length(result$features), 1)
  expect_equal(result$crs$type, "name")
  expect_equal(result$scenario1$percentage_covered, 90)
  expect_equal(result$scenario2, list())
})


# ============================================================================
# Tests for join_properties_to_polygons() (requires sf)
# ============================================================================

test_that("join_properties_to_polygons joins correctly", {
  skip_if_not_installed("sf")

  # Create a simple sf object with 2 polygons
  poly1 <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  poly2 <- sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)))
  gemeinde_sf <- sf::st_sf(
    Gemeindeschluessel = c("A", "B"),
    Gemeindename = c("Stadt A", "Stadt B"),
    geometry = sf::st_sfc(poly1, poly2, crs = 4326)
  )

  # Scenario only covers A
  props <- tibble::tibble(
    AGS = "A",
    GEN = "Stadt A",
    EWZ = paste0("1", small_space, "000"),
    fahrtzeit_scenario_1 = 15.0,
    fahrtzeit_scenario_1_str = "15,0 Minuten",
    fahrtzeit_scenario_2 = NA_real_,
    fahrtzeit_scenario_2_str = NA_character_,
    fahrtzeit_difference = NA_real_,
    fahrtzeit_difference_str = NA_character_
  )

  result <- join_properties_to_polygons(gemeinde_sf, props)

  expect_equal(nrow(result), 2)
  # A should have data
  row_a <- result[result$Gemeindeschluessel == "A", ]
  expect_equal(row_a$fahrtzeit_scenario_1, 15.0)
  expect_equal(row_a$EWZ, paste0("1", small_space, "000"))
  # B should have NA values
  row_b <- result[result$Gemeindeschluessel == "B", ]
  expect_true(is.na(row_b$fahrtzeit_scenario_1))
  # B should still have GEN from fallback
  expect_equal(row_b$GEN, "Stadt B")
})


# ============================================================================
# Tests for sf_to_feature_list() (requires sf)
# ============================================================================

test_that("sf_to_feature_list creates correct feature structure", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sf_data <- sf::st_sf(
    AGS = "A",
    GEN = "Test",
    EWZ = "100",
    fahrtzeit_scenario_1_str = "10,0 Minuten",
    fahrtzeit_scenario_2_str = NA_character_,
    fahrtzeit_difference_str = NA_character_,
    fahrtzeit_scenario_1 = 10.0,
    fahrtzeit_scenario_2 = NA_real_,
    fahrtzeit_difference = NA_real_,
    geometry = sf::st_sfc(poly, crs = 4326)
  )

  features <- sf_to_feature_list(sf_data)

  expect_length(features, 1)
  f <- features[[1]]
  expect_equal(f$type, "Feature")
  expect_equal(f$properties$AGS, "A")
  expect_equal(f$properties$GEN, "Test")
  expect_equal(f$properties$fahrtzeit_scenario_1, 10.0)
  # NA values should become NULL
  expect_null(f$properties$fahrtzeit_scenario_2)
  expect_null(f$properties$fahrtzeit_difference)
  # Geometry should be present
  expect_equal(f$geometry$type, "Polygon")
  expect_true(!is.null(f$geometry$coordinates))
})


# ============================================================================
# Tests for szenario_to_geojson() (integration, requires sf)
# ============================================================================

test_that("szenario_to_geojson produces valid GeoJSON structure", {
  skip_if_not_installed("sf")

  # Minimal scenario tibble
  szenario <- tibble::tibble(
    Gemeindeschluessel = c("A", "B"),
    Gemeindename = c("Stadt A", "Stadt B"),
    Einwohner_Gesamt = c(1000, 500),
    Mittlere_Gewichtete_Fahrzeit = c(12.5, 20.3),
    Anzahl_Betroffene = c(100, 200),
    Prozent_Betroffene = c(10, 40),
    Anzahl_Gitterzellen = c(50, 40),
    Anzahl_bewohnte_Gitterzellen = c(50, 40),
    Prozent_Abgedeckt_pro_Gemeinde = c(100, 100),
    Bundesland = c("Bayern", "Bayern"),
    Bundesland_ID = c("09", "09")
  )

  poly1 <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  poly2 <- sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)))
  poly3 <- sf::st_polygon(list(matrix(c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0), ncol = 2, byrow = TRUE)))
  gemeinde_sf <- sf::st_sf(
    Gemeindeschluessel = c("A", "B", "C"),
    Gemeindename = c("Stadt A", "Stadt B", "Stadt C"),
    geometry = sf::st_sfc(poly1, poly2, poly3, crs = 4326)
  )

  result <- szenario_to_geojson(
    szenario, gemeinde_sf,
    scenario_ids = c("H1", "H2"),
    as_json = FALSE
  )

  # Top-level structure
  expect_equal(result$type, "FeatureCollection")
  expect_equal(result$crs$type, "name")
  expect_equal(length(result$features), 3)  # All 3 polygons

  # Scenario1 metadata
  expect_equal(result$scenario1$list, list("H1", "H2"))
  expect_true(result$scenario1$percentage_covered == 100)

  # Scenario2 should be empty list

  expect_equal(result$scenario2, list())

  # Feature A should have scenario data
  f_a <- result$features[[1]]
  expect_equal(f_a$properties$AGS, "A")
  expect_equal(f_a$properties$fahrtzeit_scenario_1, 12.5)

  # Feature C should have null properties (not in scenario)
  f_c <- result$features[[3]]
  expect_equal(f_c$properties$AGS, "C")
  expect_null(f_c$properties$fahrtzeit_scenario_1)
  expect_null(f_c$properties$EWZ)
})

test_that("szenario_to_geojson returns JSON string when as_json = TRUE", {
  skip_if_not_installed("sf")

  szenario <- tibble::tibble(
    Gemeindeschluessel = "A",
    Gemeindename = "Test",
    Einwohner_Gesamt = 100,
    Mittlere_Gewichtete_Fahrzeit = 10,
    Anzahl_Betroffene = 10,
    Prozent_Betroffene = 10,
    Anzahl_Gitterzellen = 5,
    Anzahl_bewohnte_Gitterzellen = 5,
    Prozent_Abgedeckt_pro_Gemeinde = 100,
    Bundesland = "Test",
    Bundesland_ID = "00"
  )

  poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  gemeinde_sf <- sf::st_sf(
    Gemeindeschluessel = "A",
    Gemeindename = "Test",
    geometry = sf::st_sfc(poly, crs = 4326)
  )

  result <- szenario_to_geojson(szenario, gemeinde_sf, as_json = TRUE)

  expect_true(is.character(result))
  # Should be valid JSON
  parsed <- jsonlite::fromJSON(result, simplifyVector = FALSE)
  expect_equal(parsed$type, "FeatureCollection")
})

test_that("szenario_to_geojson respects coverage filter", {
  skip_if_not_installed("sf")

  szenario <- tibble::tibble(
    Gemeindeschluessel = c("A", "B"),
    Gemeindename = c("Covered", "Partial"),
    Einwohner_Gesamt = c(1000, 500),
    Mittlere_Gewichtete_Fahrzeit = c(10, 20),
    Anzahl_Betroffene = c(0, 0),
    Prozent_Betroffene = c(0, 0),
    Anzahl_Gitterzellen = c(100, 50),
    Anzahl_bewohnte_Gitterzellen = c(100, 100),
    Prozent_Abgedeckt_pro_Gemeinde = c(100, 50),
    Bundesland = c("X", "X"),
    Bundesland_ID = c("01", "01")
  )

  poly1 <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  poly2 <- sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)))
  gemeinde_sf <- sf::st_sf(
    Gemeindeschluessel = c("A", "B"),
    Gemeindename = c("Covered", "Partial"),
    geometry = sf::st_sfc(poly1, poly2, crs = 4326)
  )

  result <- szenario_to_geojson(szenario, gemeinde_sf, as_json = FALSE)

  # Both polygons should be in features (all polygons included)
  expect_equal(length(result$features), 2)

  # A should have scenario data (100% coverage >= 99% threshold)
  f_a <- result$features[[1]]
  expect_equal(f_a$properties$fahrtzeit_scenario_1, 10)

  # B should NOT have scenario data (50% coverage < 99% threshold)
  f_b <- result$features[[2]]
  expect_null(f_b$properties$fahrtzeit_scenario_1)
})
