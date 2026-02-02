# tests/testthat/test-differenz.R
# Tests for travel time difference analysis functions (no database dependencies)

# ============================================================================
# Tests for Fahrzeit_Differenz_Zusammenfassung()
# ============================================================================

# --- Basic functionality ---

test_that("Fahrzeit_Differenz_Zusammenfassung returns tibble", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800),
    Einwohner = c(100, 200)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1500, 2100),
    Einwohner = c(100, 200)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  expect_s3_class(result, "tbl_df")
})

test_that("Fahrzeit_Differenz_Zusammenfassung calculates Einwohner_Gesamt correctly", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800),
    Einwohner = c(100, 200)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1500, 2100),
    Einwohner = c(100, 200)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  expect_equal(result$Einwohner_Gesamt, 300)
})

test_that("Fahrzeit_Differenz_Zusammenfassung calculates positive difference correctly", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800),  # 20 min, 30 min
    Einwohner = c(100, 200)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1500, 2100),  # 25 min, 35 min
    Einwohner = c(100, 200)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Weighted mean difference: ((25-20)*100 + (35-30)*200) / 300 = 1500/300 = 5
  expected_diff <- ((25 - 20) * 100 + (35 - 30) * 200) / 300
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit_Differenz, expected_diff)
})

test_that("Fahrzeit_Differenz_Zusammenfassung converts seconds to minutes", {
  reference <- tibble::tibble(
    Gemeinde = "A",
    Fahrzeit_Sekunden = 1800,  # 30 minutes
    Einwohner = 100
  )
  scenario <- tibble::tibble(
    Gemeinde = "A",
    Fahrzeit_Sekunden = 2100,  # 35 minutes
    Einwohner = 100
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Difference: 35 - 30 = 5 minutes
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit_Differenz, 5)
})

test_that("Fahrzeit_Differenz_Zusammenfassung groups by .by parameter", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "B", "B"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400, 3600),
    Einwohner = c(100, 200, 150, 50)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A", "B", "B"),
    Fahrzeit_Sekunden = c(1500, 2100, 2700, 3900),
    Einwohner = c(100, 200, 150, 50)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  expect_equal(nrow(result), 2)
  expect_true("A" %in% result$Gemeinde)
  expect_true("B" %in% result$Gemeinde)
})

# --- Difference calculation variations ---

test_that("Fahrzeit_Differenz_Zusammenfassung handles negative differences", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1800, 2400),  # 30 min, 40 min
    Einwohner = c(100, 200)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800),  # 20 min, 30 min
    Einwohner = c(100, 200)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Weighted mean difference: ((20-30)*100 + (30-40)*200) / 300 = -3000/300 = -10
  expected_diff <- ((20 - 30) * 100 + (30 - 40) * 200) / 300
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit_Differenz, expected_diff)
})

test_that("Fahrzeit_Differenz_Zusammenfassung handles zero differences", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800),
    Einwohner = c(100, 200)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800),  # Same as reference
    Einwohner = c(100, 200)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit_Differenz, 0)
})

test_that("Fahrzeit_Differenz_Zusammenfassung calculates weighted mean correctly", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),  # 20, 30, 40 min
    Einwohner = c(100, 200, 150)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1500, 2100, 2700),  # 25, 35, 45 min
    Einwohner = c(100, 200, 150)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Weighted mean: (5*100 + 5*200 + 5*150) / 450 = 2250/450 = 5
  expected_mean <- (5 * 100 + 5 * 200 + 5 * 150) / 450
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit_Differenz, expected_mean)
})

test_that("Fahrzeit_Differenz_Zusammenfassung handles mixed positive and negative differences", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),  # 20, 30, 40 min
    Einwohner = c(100, 200, 150)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1500, 1800, 2100),  # 25, 30, 35 min
    Einwohner = c(100, 200, 150)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Differences: +5, 0, -5 minutes
  # Weighted mean: (5*100 + 0*200 + (-5)*150) / 450 = -250/450 = -0.555...
  expected_mean <- (5 * 100 + 0 * 200 + (-5) * 150) / 450
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit_Differenz, expected_mean)
})

test_that("Fahrzeit_Differenz_Zusammenfassung negative differences affect weighted mean but not affected count", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),  # 20, 30, 40 min
    Einwohner = c(100, 200, 300)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1500, 1500, 1800),  # 25, 25, 30 min
    Einwohner = c(100, 200, 300)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Differences: +5, -5, -10 minutes
  # Weighted mean: (5*100 + (-5)*200 + (-10)*300) / 600 = -3500/600 = -5.833...
  expected_mean <- (5 * 100 + (-5) * 200 + (-10) * 300) / 600
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit_Differenz, expected_mean)
  
  # Only positive differences count toward affected
  # Only first row has positive difference: 100 people
  expect_equal(result$Anzahl_Betroffene, 100)
})

# --- Affected people calculations ---

test_that("Fahrzeit_Differenz_Zusammenfassung counts Anzahl_Betroffene only for positive differences", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),  # 20, 30, 40 min
    Einwohner = c(100, 200, 150)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1500, 2100, 2700),  # 25, 35, 45 min
    Einwohner = c(100, 200, 150)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # All differences are positive (+5, +5, +5)
  # All people are affected: 100 + 200 + 150 = 450
  expect_equal(result$Anzahl_Betroffene, 450)
})

test_that("Fahrzeit_Differenz_Zusammenfassung zero affected when all differences are negative or zero", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1800, 2400, 3000),  # 30, 40, 50 min
    Einwohner = c(100, 200, 150)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1200, 2400, 2400),  # 20, 40, 40 min
    Einwohner = c(100, 200, 150)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Differences: -10, 0, -10 minutes
  # No positive differences, so no one is "affected" (increased travel time)
  expect_equal(result$Anzahl_Betroffene, 0)
})

test_that("Fahrzeit_Differenz_Zusammenfassung calculates Prozent_Betroffene correctly", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),  # 20, 30, 40 min
    Einwohner = c(100, 200, 150)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1500, 2100, 2100),  # 25, 35, 35 min
    Einwohner = c(100, 200, 150)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Differences: +5, +5, -5 minutes
  # Affected: 100 + 200 = 300
  # Total: 450
  # Percentage: 300/450 * 100 = 66.666...
  expected_percent <- (300 / 450) * 100
  expect_equal(result$Prozent_Betroffene, expected_percent)
})

# --- NA handling ---

test_that("Fahrzeit_Differenz_Zusammenfassung handles NA in reference Fahrzeit_Sekunden", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1200, NA, 1800),
    Einwohner = c(100, 200, 150)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1500, 2100, 2100),
    Einwohner = c(100, 200, 150)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Only rows without NA contribute: (5*100 + 5*150) / (100 + 150) = 1250/250 = 5
  expected_mean <- (5 * 100 + 5 * 150) / 250
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit_Differenz, expected_mean)
})

test_that("Fahrzeit_Differenz_Zusammenfassung handles NA in scenario Fahrzeit_Sekunden", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),
    Einwohner = c(100, 200, 150)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1500, NA, 2700),
    Einwohner = c(100, 200, 150)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Only rows without NA contribute: (5*100 + 5*150) / (100 + 150) = 1250/250 = 5
  expected_mean <- (5 * 100 + 5 * 150) / 250
  expect_equal(result$Mittlere_Gewichtete_Fahrzeit_Differenz, expected_mean)
})

test_that("Fahrzeit_Differenz_Zusammenfassung handles NA in Einwohner", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),
    Einwohner = c(100, NA, 150)
  )
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A", "A"),
    Fahrzeit_Sekunden = c(1500, 2100, 2700),
    Einwohner = c(100, NA, 150)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # Should sum Einwohner excluding NA: 100 + 150 = 250
  expect_equal(result$Einwohner_Gesamt, 250)
})

# --- Edge cases ---

test_that("Fahrzeit_Differenz_Zusammenfassung handles empty tibble", {
  reference <- tibble::tibble(
    Gemeinde = character(0),
    Fahrzeit_Sekunden = numeric(0),
    Einwohner = numeric(0)
  )
  scenario <- tibble::tibble(
    Gemeinde = character(0),
    Fahrzeit_Sekunden = numeric(0),
    Einwohner = numeric(0)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("Fahrzeit_Differenz_Zusammenfassung handles mismatched rows from left_join", {
  reference <- tibble::tibble(
    Gemeinde = c("A", "A", "B"),
    Fahrzeit_Sekunden = c(1200, 1800, 2400),
    Einwohner = c(100, 200, 150)
  )
  # Scenario is missing data for Gemeinde "B"
  scenario <- tibble::tibble(
    Gemeinde = c("A", "A"),
    Fahrzeit_Sekunden = c(1500, 2100),
    Einwohner = c(100, 200)
  )
  
  result <- Fahrzeit_Differenz_Zusammenfassung(
    reference, 
    scenario, 
    .by = Gemeinde,
    by = c("Gemeinde", "Einwohner")
  )
  
  # left_join should keep all reference rows
  # Result has 2 groups: A and B
  expect_equal(nrow(result), 2)
  
  # Gemeinde A: Weighted mean (5*100 + 5*200) / 300 = 5
  result_a <- result[result$Gemeinde == "A", ]
  expect_equal(result_a$Mittlere_Gewichtete_Fahrzeit_Differenz, 5)
  
  # Gemeinde B: Has NA due to missing scenario data
  result_b <- result[result$Gemeinde == "B", ]
  expect_true(is.nan(result_b$Mittlere_Gewichtete_Fahrzeit_Differenz))
})

# ============================================================================
# Tests for create_polygon_label_differenz()
# ============================================================================

# --- Basic functionality ---

test_that("create_polygon_label_differenz creates label column", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit_Differenz = 5.5,
    Einwohner_Gesamt = 300
  )
  
  result <- create_polygon_label_differenz(data, Gemeindename)
  
  expect_true("label" %in% names(result))
})

test_that("create_polygon_label_differenz returns tibble", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit_Differenz = 5.5,
    Einwohner_Gesamt = 300
  )
  
  result <- create_polygon_label_differenz(data, Gemeindename)
  
  expect_s3_class(result, "tbl_df")
})

test_that("create_polygon_label_differenz includes Verwaltungsebene in label", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit_Differenz = 5.5,
    Einwohner_Gesamt = 300
  )
  
  result <- create_polygon_label_differenz(data, Gemeindename)
  
  label_text <- as.character(result$label[[1]])
  expect_true(grepl("TestGemeinde", label_text))
})

test_that("create_polygon_label_differenz generates HTML tags", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit_Differenz = 5.5,
    Einwohner_Gesamt = 300
  )
  
  result <- create_polygon_label_differenz(data, Gemeindename)
  
  label_text <- as.character(result$label[[1]])
  expect_true(grepl("<strong>", label_text, fixed = TRUE))
  expect_true(grepl("<br/>", label_text, fixed = TRUE))
  expect_true(grepl("</strong>", label_text, fixed = TRUE))
})

test_that("create_polygon_label_differenz label contains Fahrzeit-Differenz text", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit_Differenz = 5.5,
    Einwohner_Gesamt = 300
  )
  
  result <- create_polygon_label_differenz(data, Gemeindename)
  
  label_text <- as.character(result$label[[1]])
  # Key difference from regular label: should say "Fahrzeit-Differenz"
  expect_true(grepl("Fahrzeit-Differenz in min:", label_text))
})

# --- Error handling ---

test_that("create_polygon_label_differenz errors on missing Mittlere_Gewichtete_Fahrzeit_Differenz", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Einwohner_Gesamt = 300
  )
  
  expect_error(
    create_polygon_label_differenz(data, Gemeindename),
    "Missing required columns"
  )
})

test_that("create_polygon_label_differenz errors on missing Einwohner_Gesamt", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit_Differenz = 5.5
  )
  
  expect_error(
    create_polygon_label_differenz(data, Gemeindename),
    "Missing required columns"
  )
})

# --- Conditional behavior ---

test_that("create_polygon_label_differenz handles data without threshold columns", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit_Differenz = 5.5,
    Einwohner_Gesamt = 300
  )
  
  result <- create_polygon_label_differenz(data, Gemeindename)
  
  label_text <- as.character(result$label[[1]])
  # Should NOT contain "Betroffene" text
  expect_false(grepl("Betroffene", label_text))
})

test_that("create_polygon_label_differenz handles data with threshold columns", {
  data <- tibble::tibble(
    Gemeindename = "TestGemeinde",
    Mittlere_Gewichtete_Fahrzeit_Differenz = 5.5,
    Einwohner_Gesamt = 300,
    Anzahl_Betroffene = 200,
    Prozent_Betroffene = 66.67
  )
  
  result <- create_polygon_label_differenz(data, Gemeindename)
  
  label_text <- as.character(result$label[[1]])
  # Should contain "Betroffene" text
  expect_true(grepl("Betroffene", label_text))
  expect_true(grepl("Prozent Betroffene", label_text))
})

# --- Edge cases ---

test_that("create_polygon_label_differenz handles multiple rows", {
  data <- tibble::tibble(
    Gemeindename = c("A", "B", "C"),
    Mittlere_Gewichtete_Fahrzeit_Differenz = c(5, -3, 0),
    Einwohner_Gesamt = c(100, 200, 300)
  )
  
  result <- create_polygon_label_differenz(data, Gemeindename)
  
  expect_equal(nrow(result), 3)
  expect_equal(length(result$label), 3)
  
  # Check each label is unique and contains correct name
  expect_true(grepl("A", as.character(result$label[[1]])))
  expect_true(grepl("B", as.character(result$label[[2]])))
  expect_true(grepl("C", as.character(result$label[[3]])))
})

test_that("create_polygon_label_differenz handles empty tibble", {
  data <- tibble::tibble(
    Gemeindename = character(0),
    Mittlere_Gewichtete_Fahrzeit_Differenz = numeric(0),
    Einwohner_Gesamt = numeric(0)
  )
  
  result <- create_polygon_label_differenz(data, Gemeindename)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("label" %in% names(result))
})
