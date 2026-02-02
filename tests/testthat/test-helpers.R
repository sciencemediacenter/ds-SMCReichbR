# tests/testthat/test-helpers.R
# Tests for pure helper functions (no database dependencies)

# ============================================================================
# Tests for format_label_columns()
# ============================================================================

test_that("format_label_columns formats numeric columns correctly", {
  data <- tibble::tibble(value = 1234.567)
  result <- format_label_columns(data, "value")

  expect_true("value_formatted" %in% names(result))
  # Default: digits = 1, big.mark = " ", decimal.mark = ","
})

test_that("format_label_columns uses custom digits parameter", {
  data <- tibble::tibble(value = 1234.567)

  # digits = 0 (rounds to 1235, with default space as thousand separator)
  result_0 <- format_label_columns(data, "value", digits = 0)
  # Remove spaces to check the numeric value
  expect_true(grepl("1.*235", result_0$value_formatted))

  # digits = 2
  result_2 <- format_label_columns(data, "value", digits = 2)
  expect_true(grepl("57", result_2$value_formatted))
})

test_that("format_label_columns uses custom separators", {
  data <- tibble::tibble(value = 1234.5)

  # US-style formatting
  result <- format_label_columns(
    data,
    "value",
    big.mark = ",",
    decimal.mark = "."
  )
  expect_true(grepl(",", result$value_formatted))
  expect_true(grepl("\\.", result$value_formatted))
})

test_that("format_label_columns handles non-numeric columns", {
  data <- tibble::tibble(name = "test_value")
  result <- format_label_columns(data, "name")

  expect_true("name_formatted" %in% names(result))
  expect_equal(trimws(result$name_formatted), "test_value")
})

test_that("format_label_columns ignores columns not in data", {
  data <- tibble::tibble(value = 123)
  result <- format_label_columns(data, c("value", "missing_column"))

  # Should have value_formatted but not missing_column_formatted
  expect_true("value_formatted" %in% names(result))
  expect_false("missing_column_formatted" %in% names(result))
})
test_that("format_label_columns returns a tibble", {
  data <- tibble::tibble(value = 123)
  result <- format_label_columns(data, "value")

  expect_s3_class(result, "tbl_df")
})

test_that("format_label_columns handles empty tibble", {
  data <- tibble::tibble(value = numeric(0))
  result <- format_label_columns(data, "value")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("value_formatted" %in% names(result))
})

test_that("format_label_columns handles NA values", {
  data <- tibble::tibble(value = c(123, NA, 456))
  result <- format_label_columns(data, "value")

  expect_true("value_formatted" %in% names(result))
  expect_equal(length(result$value_formatted), 3)
})

test_that("format_label_columns handles large numbers with thousand separators", {
  data <- tibble::tibble(value = 1234567890.123)
  result <- format_label_columns(data, "value", big.mark = " ")

  # Should contain spaces as thousand separators
  expect_true(grepl(" ", result$value_formatted))
})

test_that("format_label_columns handles multiple columns", {
  data <- tibble::tibble(
    col1 = 1234.5,
    col2 = 6789.1
  )
  result <- format_label_columns(data, c("col1", "col2"))

  expect_true("col1_formatted" %in% names(result))
  expect_true("col2_formatted" %in% names(result))
})

# ============================================================================
# Tests for ensure_dir_exists()
# ============================================================================

test_that("ensure_dir_exists creates new directory", {
  tmp_base <- withr::local_tempdir()
  new_dir <- file.path(tmp_base, "new_test_dir")

  expect_false(dir.exists(new_dir))
  ensure_dir_exists(new_dir)
  expect_true(dir.exists(new_dir))
})

test_that("ensure_dir_exists creates nested directories", {
  tmp_base <- withr::local_tempdir()
  nested_dir <- file.path(tmp_base, "level1", "level2", "level3")

  expect_false(dir.exists(nested_dir))
  ensure_dir_exists(nested_dir)
  expect_true(dir.exists(nested_dir))
})

test_that("ensure_dir_exists is idempotent for existing directory", {
  tmp_base <- withr::local_tempdir()
  existing_dir <- file.path(tmp_base, "existing")
  dir.create(existing_dir)

  expect_true(dir.exists(existing_dir))
  # Should not error when called on existing directory
  expect_no_error(ensure_dir_exists(existing_dir))
  expect_true(dir.exists(existing_dir))
})

# ============================================================================
# Tests for require_file_exists()
# ============================================================================

test_that("require_file_exists throws error for missing file", {
  missing_path <- "/nonexistent/path/to/file.txt"

  expect_error(
    require_file_exists(missing_path),
    "File not found"
  )
})

test_that("require_file_exists succeeds for existing file", {
  tmp_file <- withr::local_tempfile()
  writeLines("test content", tmp_file)

  expect_no_error(require_file_exists(tmp_file))
})

test_that("require_file_exists error message contains file path", {
  missing_path <- "/some/specific/missing/file.csv"

  expect_error(
    require_file_exists(missing_path),
    missing_path,
    fixed = TRUE
  )
})
