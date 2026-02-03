# Tests for source strategies

# Helper functions for skipping tests
skip_if_no_duckdb <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    skip("duckdb package not available")
  }
}

skip_if_no_postgres <- function() {
  tryCatch(
    {
      con <- SMCReichbR:::connect_to_postgres_db(
        expose_connection_to_viewer = FALSE,
        print_connection_info = FALSE
      )
      if (is.null(con) || !DBI::dbIsValid(con)) {
        skip("PostgreSQL tests disabled - no test database available")
      }
      DBI::dbDisconnect(con)
    },
    error = function(e) {
      skip("PostgreSQL tests disabled - no test database available")
    }
  )
}

# ============================================================================
# postgres_source() tests
# ============================================================================

test_that("postgres_source creates valid configuration with defaults", {
  src <- postgres_source()

  expect_s3_class(src, "pg_source")
  expect_equal(src$type, "postgres")

  # Check default settings
  expect_equal(src$settings$work_mem, "512MB")
  expect_equal(src$settings$maintenance_work_mem, "512MB")
  expect_equal(src$settings$effective_cache_size, "4GB")
  expect_equal(src$settings$random_page_cost, 1.1)
  expect_equal(src$settings$effective_io_concurrency, 200)
  expect_equal(src$settings$max_parallel_workers_per_gather, 4)
})

test_that("postgres_source allows overriding settings", {
  src <- postgres_source(
    work_mem = "1GB",
    max_parallel_workers_per_gather = 8
  )

  expect_equal(src$settings$work_mem, "1GB")
  expect_equal(src$settings$max_parallel_workers_per_gather, 8)
  # Other settings should still have defaults
  expect_equal(src$settings$maintenance_work_mem, "512MB")
})

test_that("postgres_source allows NULL values for individual settings", {
  src <- postgres_source(work_mem = NULL)

  expect_null(src$settings$work_mem)
  # Other settings should still have defaults
  expect_equal(src$settings$maintenance_work_mem, "512MB")
})

test_that("postgres_source with all NULL settings disables optimizations", {
  src <- postgres_source(
    work_mem = NULL,
    maintenance_work_mem = NULL,
    effective_cache_size = NULL,
    random_page_cost = NULL,
    effective_io_concurrency = NULL,
    max_parallel_workers_per_gather = NULL
  )

  # All settings should be NULL
  expect_true(all(sapply(src$settings, is.null)))
})

test_that("postgres_source includes new optimization parameters with NULL defaults", {
  src <- postgres_source()

  # New parameters should exist and default to NULL
  expect_true("enable_hashjoin" %in% names(src$settings))
  expect_null(src$settings$enable_hashjoin)
  expect_null(src$settings$enable_mergejoin)
  expect_null(src$settings$enable_nestloop)
  expect_null(src$settings$min_parallel_table_scan_size)
  expect_null(src$settings$parallel_setup_cost)
  expect_null(src$settings$parallel_tuple_cost)
  expect_null(src$settings$seq_page_cost)
})

test_that("postgres_source allows setting new optimization parameters", {
  src <- postgres_source(
    enable_hashjoin = "off",
    enable_mergejoin = "off",
    enable_nestloop = "on",
    min_parallel_table_scan_size = "8MB",
    parallel_setup_cost = 0,
    parallel_tuple_cost = 0,
    seq_page_cost = 10
  )

  expect_equal(src$settings$enable_hashjoin, "off")
  expect_equal(src$settings$enable_mergejoin, "off")
  expect_equal(src$settings$enable_nestloop, "on")
  expect_equal(src$settings$min_parallel_table_scan_size, "8MB")
  expect_equal(src$settings$parallel_setup_cost, 0)
  expect_equal(src$settings$parallel_tuple_cost, 0)
  expect_equal(src$settings$seq_page_cost, 10)
})

# ============================================================================
# duckdb_source() tests
# ============================================================================

test_that("duckdb_source creates valid configuration", {
  skip_if_no_duckdb()

  # Create a temporary DuckDB file for testing
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), temp_db)
  DBI::dbDisconnect(con, shutdown = TRUE)

  src <- duckdb_source(path = temp_db)

  expect_s3_class(src, "duckdb_source")
  expect_equal(src$type, "duckdb")
  expect_equal(src$path, temp_db)
  expect_null(src$settings$threads)
  expect_null(src$settings$memory_limit)

  unlink(temp_db)
})

test_that("duckdb_source allows custom settings", {
  skip_if_no_duckdb()

  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), temp_db)
  DBI::dbDisconnect(con, shutdown = TRUE)

  src <- duckdb_source(
    path = temp_db,
    threads = 8,
    memory_limit = "16GB"
  )

  expect_equal(src$settings$threads, 8)
  expect_equal(src$settings$memory_limit, "16GB")

  unlink(temp_db)
})

test_that("duckdb_source requires path parameter", {
  expect_error(
    duckdb_source(),
    "path is required"
  )
})

test_that("duckdb_source validates file existence", {
  expect_error(
    duckdb_source(path = "/nonexistent/path/to/database.duckdb"),
    "does not exist"
  )
})

# ============================================================================
# create_source_config() tests
# ============================================================================

test_that("create_source_config creates postgres source by default", {
  src <- create_source_config()

  expect_s3_class(src, "pg_source")
  expect_equal(src$type, "postgres")
})

test_that("create_source_config merges pg_options with defaults", {
  src <- create_source_config(
    source = "postgres",
    pg_options = list(work_mem = "2GB")
  )

  expect_equal(src$settings$work_mem, "2GB")
  # Other settings should have defaults
  expect_equal(src$settings$maintenance_work_mem, "512MB")
})

test_that("create_source_config overrides specific settings", {
  src <- create_source_config(
    source = "postgres",
    pg_options = list(work_mem = "2GB", maintenance_work_mem = "1GB")
  )

  expect_equal(src$settings$work_mem, "2GB")
  expect_equal(src$settings$maintenance_work_mem, "1GB")
  # Other settings should still have defaults
  expect_equal(src$settings$effective_cache_size, "4GB")
})

test_that("create_source_config merges new pg_options parameters", {
  src <- create_source_config(
    source = "postgres",
    pg_options = list(
      enable_hashjoin = "off",
      enable_nestloop = "on",
      work_mem = "2GB"
    )
  )

  expect_equal(src$settings$enable_hashjoin, "off")
  expect_equal(src$settings$enable_nestloop, "on")
  expect_equal(src$settings$work_mem, "2GB")
  # Unset new params should remain NULL
  expect_null(src$settings$enable_mergejoin)
  expect_null(src$settings$seq_page_cost)
})

test_that("create_source_config creates duckdb source when specified", {
  skip_if_no_duckdb()

  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), temp_db)
  DBI::dbDisconnect(con, shutdown = TRUE)

  src <- create_source_config(
    source = "duckdb",
    source_duckdb_path = temp_db
  )

  expect_s3_class(src, "duckdb_source")
  expect_equal(src$path, temp_db)

  unlink(temp_db)
})

test_that("create_source_config requires path for duckdb source", {
  expect_error(
    create_source_config(source = "duckdb"),
    "source_duckdb_path is required"
  )
})

test_that("create_source_config rejects invalid source type", {
  expect_error(
    create_source_config(source = "invalid"),
    "must be 'postgres' or 'duckdb'"
  )
})

test_that("create_source_config merges duckdb_options", {
  skip_if_no_duckdb()

  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), temp_db)
  DBI::dbDisconnect(con, shutdown = TRUE)

  src <- create_source_config(
    source = "duckdb",
    source_duckdb_path = temp_db,
    duckdb_options = list(threads = 4, memory_limit = "8GB")
  )

  expect_equal(src$settings$threads, 4)
  expect_equal(src$settings$memory_limit, "8GB")

  unlink(temp_db)
})

# ============================================================================
# get_source_table_ref() tests
# ============================================================================

test_that("get_source_table_ref returns correct PostgreSQL reference", {
  src <- postgres_source()

  ref <- get_source_table_ref(src, "Entfernungsdaten", "public")
  expect_equal(ref, "pg_src.public.Entfernungsdaten")

  ref2 <- get_source_table_ref(src, "MyTable", "myschema")
  expect_equal(ref2, "pg_src.myschema.MyTable")
})

test_that("get_source_table_ref returns correct DuckDB reference", {
  skip_if_no_duckdb()

  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), temp_db)
  DBI::dbDisconnect(con, shutdown = TRUE)

  src <- duckdb_source(path = temp_db)

  ref <- get_source_table_ref(src, "Entfernungsdaten", "public")
  expect_equal(ref, "source_db.Entfernungsdaten")

  unlink(temp_db)
})

# ============================================================================
# apply_source_optimizations() tests
# ============================================================================

test_that("apply_source_optimizations applies DuckDB settings", {
  skip_if_no_duckdb()

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Create a mock duckdb_source with settings
  src <- structure(
    list(
      type = "duckdb",
      path = ":memory:",
      settings = list(threads = 2)
    ),
    class = "duckdb_source"
  )

  # Should not error
  expect_silent(apply_source_optimizations(src, con))

  # Verify setting was applied
  result <- DBI::dbGetQuery(con, "SELECT current_setting('threads') as threads")
  expect_equal(as.integer(result$threads), 2)
})

test_that("apply_source_optimizations skips NULL settings", {
  skip_if_no_duckdb()

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  src <- structure(
    list(
      type = "duckdb",
      path = ":memory:",
      settings = list(threads = NULL, memory_limit = NULL)
    ),
    class = "duckdb_source"
  )

  # Should not error even with all NULL settings
  expect_silent(apply_source_optimizations(src, con))
})

test_that("apply_source_optimizations applies PostgreSQL settings", {
  skip_if_no_postgres()

  con <- connect_to_postgres_db(
    expose_connection_to_viewer = FALSE,
    print_connection_info = FALSE
  )
  on.exit(DBI::dbDisconnect(con))

  src <- postgres_source(work_mem = "999MB")

  apply_source_optimizations(src, con)

  # Verify setting was applied
  after <- DBI::dbGetQuery(con, "SHOW work_mem")$work_mem
  expect_equal(after, "999MB")
})
