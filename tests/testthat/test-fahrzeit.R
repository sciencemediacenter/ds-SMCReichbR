# tests/testthat/test-fahrzeit.R
# Tests for travel time analysis functions (no database dependencies)

# ============================================================================
# Tests for Fahrzeit_Zusammenfassung()
# ============================================================================

# --- Basic functionality ---

test_that("Fahrzeit_Zusammenfassung returns tibble", {
  data <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Gitterzellen_ID = c("G1", "G2"),
    Fahrzeit_Sekunden = c(1200, 1800),
    Einwohner = c(100, 200)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde)

  expect_s3_class(result, "tbl_df")
})

test_that("Fahrzeit_Zusammenfassung calculates Einwohner_Gesamt correctly", {
  data <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Gitterzellen_ID = c("G1", "G2"),
    Fahrzeit_Sekunden = c(1200, 1800),
    Einwohner = c(100, 200)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde)

  expect_equal(result$Einwohner_Gesamt, 300)
})

test_that("Fahrzeit_Zusammenfassung calculates weighted mean correctly", {
  data <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Gitterzellen_ID = c("G1", "G2"),
    Fahrzeit_Sekunden = c(1200, 1800), # 20 min, 30 min
    Einwohner = c(100, 200)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde)

  # Weighted mean: (20*100 + 30*200) / 300 = 8000/300 = 26.666...
  expected_mean <- (20 * 100 + 30 * 200) / 300
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit, expected_mean)
})

test_that("Fahrzeit_Zusammenfassung converts seconds to minutes", {
  data <- tibble::tibble(
    Gemeinde = "A",
    Gitterzellen_ID = "G1",
    Fahrzeit_Sekunden = 1800, # 30 minutes
    Einwohner = 100
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde)

  expect_equal(result$Mittlere_Gewichtete_Fahrzeit, 30)
})

test_that("Fahrzeit_Zusammenfassung groups by .by parameter", {
  data <- tibble::tibble(
    Gemeinde = c("A", "A", "B", "B"),
    Gitterzellen_ID = c("G1", "G2", "G3", "G4"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400, 3600),
    Einwohner = c(100, 200, 150, 50)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde)

  expect_equal(nrow(result), 2)
  expect_true("A" %in% result$Gemeinde)
  expect_true("B" %in% result$Gemeinde)
})

test_that("Fahrzeit_Zusammenfassung counts Anzahl_Gitterzellen correctly", {
  data <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Gitterzellen_ID = c("G1", "G2", "G3"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),
    Einwohner = c(100, 200, 150)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde)

  expect_equal(result$Anzahl_Gitterzellen, 3)
})

test_that("Fahrzeit_Zusammenfassung counts distinct Gitterzellen_ID", {
  data <- tibble::tibble(
    Gemeinde = c("A", "A", "A", "A"),
    Gitterzellen_ID = c("G1", "G2", "G1", "G2"), # Only 2 distinct
    Fahrzeit_Sekunden = c(1200, 1800, 1200, 1800),
    Einwohner = c(100, 200, 50, 75)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde)

  expect_equal(result$Anzahl_Gitterzellen, 2)
})

# --- Threshold behavior ---

test_that("Fahrzeit_Zusammenfassung without threshold omits Betroffene columns", {
  data <- tibble::tibble(
    Gemeinde = "A",
    Gitterzellen_ID = "G1",
    Fahrzeit_Sekunden = 1800,
    Einwohner = 100
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde, Grenzwert_Minuten = NULL)

  expect_false("Anzahl_Betroffene" %in% names(result))
  expect_false("Prozent_Betroffene" %in% names(result))
  expect_true("Anzahl_Gitterzellen" %in% names(result))
})

test_that("Fahrzeit_Zusammenfassung with threshold includes Betroffene columns", {
  data <- tibble::tibble(
    Gemeinde = "A",
    Gitterzellen_ID = "G1",
    Fahrzeit_Sekunden = 1800,
    Einwohner = 100
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde, Grenzwert_Minuten = 25)

  expect_true("Anzahl_Betroffene" %in% names(result))
  expect_true("Prozent_Betroffene" %in% names(result))
  expect_true("Anzahl_Gitterzellen" %in% names(result))
})

test_that("Fahrzeit_Zusammenfassung calculates Anzahl_Betroffene correctly", {
  data <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Gitterzellen_ID = c("G1", "G2", "G3"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400), # 20, 30, 40 min
    Einwohner = c(100, 200, 150)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde, Grenzwert_Minuten = 25)

  # Only rows with 30 min and 40 min exceed threshold of 25
  # Affected: 200 + 150 = 350
  expect_equal(result$Anzahl_Betroffene, 350)
})

test_that("Fahrzeit_Zusammenfassung calculates Prozent_Betroffene correctly", {
  data <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Gitterzellen_ID = c("G1", "G2", "G3"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400), # 20, 30, 40 min
    Einwohner = c(100, 200, 150)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde, Grenzwert_Minuten = 25)

  # Affected: 350, Total: 450
  # Percentage: 350/450 * 100 = 77.777...
  expected_percent <- (350 / 450) * 100
  expect_equal(result$Prozent_Betroffene, expected_percent)
})

# --- Edge cases ---

test_that("Fahrzeit_Zusammenfassung handles NA values in Fahrzeit", {
  data <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Gitterzellen_ID = c("G1", "G2", "G3"),
    Fahrzeit_Sekunden = c(1200, NA, 1800),
    Einwohner = c(100, 200, 150)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde)

  # Should calculate weighted mean excluding NA
  # (20*100 + 30*150) / (100 + 150) = 6500/250 = 26
  expected_mean <- (20 * 100 + 30 * 150) / 250
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit, expected_mean)
})

test_that("Fahrzeit_Zusammenfassung handles NA values in Einwohner", {
  data <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Gitterzellen_ID = c("G1", "G2", "G3"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),
    Einwohner = c(100, NA, 150)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde)

  # Should sum Einwohner excluding NA: 100 + 150 = 250
  expect_equal(result$Einwohner_Gesamt, 250)
})

test_that("Fahrzeit_Zusammenfassung handles empty tibble", {
  data <- tibble::tibble(
    Gemeinde = character(0),
    Gitterzellen_ID = character(0),
    Fahrzeit_Sekunden = numeric(0),
    Einwohner = numeric(0)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# ============================================================================
# Tests for create_polygon_label()
# ============================================================================

# --- Basic functionality ---

test_that("create_polygon_label creates label column", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit = 26.67,
    Einwohner_Gesamt = 300
  )

  result <- create_polygon_label(data, Gemeindename)

  expect_true("label" %in% names(result))
})

test_that("create_polygon_label returns tibble", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit = 26.67,
    Einwohner_Gesamt = 300
  )

  result <- create_polygon_label(data, Gemeindename)

  expect_s3_class(result, "tbl_df")
})

test_that("create_polygon_label includes Verwaltungsebene in label", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit = 26.67,
    Einwohner_Gesamt = 300
  )

  result <- create_polygon_label(data, Gemeindename)

  label_text <- as.character(result$label[[1]])
  expect_true(grepl("TestGemeinde", label_text))
})

test_that("create_polygon_label generates HTML tags", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit = 26.67,
    Einwohner_Gesamt = 300
  )

  result <- create_polygon_label(data, Gemeindename)

  label_text <- as.character(result$label[[1]])
  expect_true(grepl("<strong>", label_text, fixed = TRUE))
  expect_true(grepl("<br/>", label_text, fixed = TRUE))
  expect_true(grepl("</strong>", label_text, fixed = TRUE))
})

test_that("create_polygon_label label contains HTML content", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit = 26.67,
    Einwohner_Gesamt = 300
  )

  result <- create_polygon_label(data, Gemeindename)

  expect_type(result$label, "character")
  # Should contain HTML tags
  expect_true(grepl("<strong>", as.character(result$label[[1]]), fixed = TRUE))
  expect_true(grepl("<br/>", as.character(result$label[[1]]), fixed = TRUE))
})

# --- Error handling ---

test_that("create_polygon_label errors on missing Mittlere_Gewichtete_Fahrzeit", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Einwohner_Gesamt = 300
  )

  expect_error(
    create_polygon_label(data, Gemeindename),
    "Missing required columns"
  )
})

test_that("create_polygon_label errors on missing Einwohner_Gesamt", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit = 26.67
  )

  expect_error(
    create_polygon_label(data, Gemeindename),
    "Missing required columns"
  )
})

# --- Conditional behavior ---

test_that("create_polygon_label handles data without threshold columns", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit = 26.67,
    Einwohner_Gesamt = 300
  )

  result <- create_polygon_label(data, Gemeindename)

  label_text <- as.character(result$label[[1]])
  # Should NOT contain "Betroffene" text
  expect_false(grepl("Betroffene", label_text))
})

test_that("create_polygon_label handles data with threshold columns", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit = 26.67,
    Einwohner_Gesamt = 300,
    Anzahl_Betroffene = 200,
    Prozent_Betroffene = 66.67
  )

  result <- create_polygon_label(data, Gemeindename)

  label_text <- as.character(result$label[[1]])
  # Should contain "Betroffene" text
  expect_true(grepl("Betroffene", label_text))
})

test_that("create_polygon_label includes all threshold fields in label", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit = 26.67,
    Einwohner_Gesamt = 300,
    Anzahl_Betroffene = 200,
    Prozent_Betroffene = 66.67
  )

  result <- create_polygon_label(data, Gemeindename)

  label_text <- as.character(result$label[[1]])
  expect_true(grepl("Prozent Betroffene", label_text))
  expect_true(grepl("Betroffene Einwohner", label_text))
})

# --- Edge cases ---

test_that("create_polygon_label handles multiple rows", {
  data <- tibble::tibble(
    Gemeindename = c("A", "B", "C"),
    Mittlere_Gewichtete_Fahrzeit = c(20, 30, 40),
    Einwohner_Gesamt = c(100, 200, 300)
  )

  result <- create_polygon_label(data, Gemeindename)

  expect_equal(nrow(result), 3)
  expect_equal(length(result$label), 3)

  # Check each label is unique and contains correct name
  expect_true(grepl("A", as.character(result$label[[1]])))
  expect_true(grepl("B", as.character(result$label[[2]])))
  expect_true(grepl("C", as.character(result$label[[3]])))
})

test_that("create_polygon_label handles empty tibble", {
  data <- tibble::tibble(
    Gemeindename = character(0),
    Mittlere_Gewichtete_Fahrzeit = numeric(0),
    Einwohner_Gesamt = numeric(0)
  )

  result <- create_polygon_label(data, Gemeindename)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("label" %in% names(result))
})

# ============================================================================
# Tests for Fahrzeit_Zusammenfassung() — multi-threshold support
# ============================================================================

test_that("Fahrzeit_Zusammenfassung NULL threshold returns baseline only", {
  data <- tibble::tibble(
    Gemeinde = "A",
    Gitterzellen_ID = c("G1", "G2"),
    Fahrzeit_Sekunden = c(1200, 2400),
    Einwohner = c(100, 200)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde, Grenzwert_Minuten = NULL)

  expect_false("Anzahl_Betroffene" %in% names(result))
  expect_false("Prozent_Betroffene" %in% names(result))
  expect_true("Einwohner_Gesamt" %in% names(result))
  expect_true("Mittlere_Gewichtete_Fahrzeit" %in% names(result))
})

test_that("Fahrzeit_Zusammenfassung empty vector treated as NULL", {
  data <- tibble::tibble(
    Gemeinde = "A",
    Gitterzellen_ID = c("G1", "G2"),
    Fahrzeit_Sekunden = c(1200, 2400),
    Einwohner = c(100, 200)
  )

  result_null <- Fahrzeit_Zusammenfassung(
    data,
    Gemeinde,
    Grenzwert_Minuten = NULL
  )
  result_empty <- Fahrzeit_Zusammenfassung(
    data,
    Gemeinde,
    Grenzwert_Minuten = numeric(0)
  )

  expect_equal(names(result_null), names(result_empty))
  expect_equal(result_null$Einwohner_Gesamt, result_empty$Einwohner_Gesamt)
})

test_that("Fahrzeit_Zusammenfassung scalar threshold produces unsuffixed columns", {
  data <- tibble::tibble(
    Gemeinde = "A",
    Gitterzellen_ID = c("G1", "G2", "G3"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400), # 20, 30, 40 min
    Einwohner = c(100, 200, 150)
  )

  result <- Fahrzeit_Zusammenfassung(data, Gemeinde, Grenzwert_Minuten = 25)

  expect_true("Anzahl_Betroffene" %in% names(result))
  expect_true("Prozent_Betroffene" %in% names(result))
  expect_false(any(grepl("_min$", names(result))))
})

test_that("Fahrzeit_Zusammenfassung vector threshold produces suffixed wide columns", {
  data <- tibble::tibble(
    Gemeinde = "A",
    Gitterzellen_ID = c("G1", "G2", "G3"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400), # 20, 30, 40 min
    Einwohner = c(100, 200, 150)
  )

  result <- Fahrzeit_Zusammenfassung(
    data,
    Gemeinde,
    Grenzwert_Minuten = c(30, 60)
  )

  expect_true("Anzahl_Betroffene_30min" %in% names(result))
  expect_true("Prozent_Betroffene_30min" %in% names(result))
  expect_true("Anzahl_Betroffene_60min" %in% names(result))
  expect_true("Prozent_Betroffene_60min" %in% names(result))
  expect_false("Anzahl_Betroffene" %in% names(result))
  expect_false("Prozent_Betroffene" %in% names(result))
})

test_that("Fahrzeit_Zusammenfassung vector threshold computes correct values", {
  data <- tibble::tibble(
    Gemeinde = "A",
    Gitterzellen_ID = c("G1", "G2", "G3"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400), # 20, 30, 40 min
    Einwohner = c(100, 200, 150)
  )

  result <- Fahrzeit_Zusammenfassung(
    data,
    Gemeinde,
    Grenzwert_Minuten = c(30, 60)
  )

  # threshold 30: only 40 min row exceeds → 150 affected
  expect_equal(result$Anzahl_Betroffene_30min, 150)
  expect_equal(result$Prozent_Betroffene_30min, 150 / 450 * 100)

  # threshold 60: no row exceeds → 0 affected
  expect_equal(result$Anzahl_Betroffene_60min, 0)
  expect_equal(result$Prozent_Betroffene_60min, 0)
})

test_that("Fahrzeit_Zusammenfassung vector with NA and duplicates same as clean vector", {
  data <- tibble::tibble(
    Gemeinde = "A",
    Gitterzellen_ID = c("G1", "G2", "G3"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),
    Einwohner = c(100, 200, 150)
  )

  result_clean <- Fahrzeit_Zusammenfassung(
    data,
    Gemeinde,
    Grenzwert_Minuten = c(30, 60)
  )
  result_dirty <- Fahrzeit_Zusammenfassung(
    data,
    Gemeinde,
    Grenzwert_Minuten = c(30, NA, 60, 30)
  )

  expect_equal(names(result_clean), names(result_dirty))
  expect_equal(
    result_clean$Anzahl_Betroffene_30min,
    result_dirty$Anzahl_Betroffene_30min
  )
  expect_equal(
    result_clean$Anzahl_Betroffene_60min,
    result_dirty$Anzahl_Betroffene_60min
  )
})

test_that("Fahrzeit_Zusammenfassung vector threshold preserves baseline columns", {
  data <- tibble::tibble(
    Gemeinde = "A",
    Gitterzellen_ID = c("G1", "G2"),
    Fahrzeit_Sekunden = c(1200, 2400),
    Einwohner = c(100, 200)
  )

  result <- Fahrzeit_Zusammenfassung(
    data,
    Gemeinde,
    Grenzwert_Minuten = c(30, 60)
  )

  expect_true("Einwohner_Gesamt" %in% names(result))
  expect_true("Mittlere_Gewichtete_Fahrzeit" %in% names(result))
  expect_true("Anzahl_Gitterzellen" %in% names(result))
})
