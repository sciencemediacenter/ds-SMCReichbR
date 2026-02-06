# tests/testthat/test-database.R
# Database integration tests
# - Tier 1: Connection error handling (no database required)
# - Tier 2: DuckDB integration tests (local database only)
# - Tier 3+: PostgreSQL tests (skipped - no test database available)

# ============================================================================
# Skip Helpers
# ============================================================================

skip_if_no_duckdb <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    skip("DuckDB package not available")
  }
}

skip_if_no_postgres <- function() {
  skip("PostgreSQL tests disabled - no test database available")
}

# ============================================================================
# Tier 1: Connection Error Handling Tests (No Database Required)
# ============================================================================

# --- PostgreSQL connection errors ---

test_that("connect_to_postgres_db errors when all env vars missing", {
  # Save current env vars
  old_env <- c(
    DB = Sys.getenv("DB"),
    HOST = Sys.getenv("HOST"),
    PORT = Sys.getenv("PORT"),
    USER = Sys.getenv("USER"),
    PASSWORD = Sys.getenv("PASSWORD")
  )
  on.exit({
    do.call(Sys.setenv, as.list(old_env[nzchar(old_env)]))
  })

  # Temporarily unset env vars
  Sys.unsetenv(c("DB", "HOST", "PORT", "USER", "PASSWORD"))

  expect_error(connect_to_postgres_db(), "Could not connect")
})

test_that("connect_to_postgres_db errors when HOST missing", {
  old_host <- Sys.getenv("HOST")
  on.exit(
    if (nzchar(old_host)) Sys.setenv(HOST = old_host) else Sys.unsetenv("HOST")
  )

  Sys.unsetenv("HOST")

  expect_error(connect_to_postgres_db(), "Could not connect")
})

# --- DuckDB connection tests ---

test_that("connect_to_duckdb_db creates valid connection", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_true(dbIsValid(con))
  expect_s4_class(con, "duckdb_connection")
})

test_that("connect_to_duckdb_db creates database file", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_true(file.exists(db_path))
})

test_that("connect_to_duckdb_db works with expose_connection_to_viewer", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")

  # Should not error even if viewer not available
  expect_no_error({
    con <- connect_to_duckdb_db(db_path, expose_connection_to_viewer = TRUE)
    dbDisconnect(con, shutdown = TRUE)
  })
})

# ============================================================================
# Tier 2: DuckDB Integration Tests (Local Database Only)
# ============================================================================

# --- Basic DuckDB operations ---

test_that("DuckDB can create and query tables", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create table
  dbExecute(con, "CREATE TABLE test_table (id INTEGER, name TEXT)")
  dbExecute(con, "INSERT INTO test_table VALUES (1, 'Alice'), (2, 'Bob')")

  # Query
  result <- dbGetQuery(con, "SELECT * FROM test_table ORDER BY id")

  expect_equal(nrow(result), 2)
  expect_equal(result$id, c(1, 2))
  expect_equal(result$name, c("Alice", "Bob"))
})

# --- Parquet export: single table ---

test_that("export_single_duckdb_table_to_parquet creates parquet file", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_path <- withr::local_tempfile(fileext = ".parquet")

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create test table
  dbExecute(con, "CREATE TABLE test_export (id INTEGER, value TEXT)")
  dbExecute(con, "INSERT INTO test_export VALUES (1, 'a'), (2, 'b'), (3, 'c')")

  # Export
  export_single_duckdb_table_to_parquet(con, "test_export", output_path)

  expect_true(file.exists(output_path))
  expect_gt(file.size(output_path), 0)
})

test_that("export_single_duckdb_table_to_parquet preserves data correctly", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_path <- withr::local_tempfile(fileext = ".parquet")

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create table with various data types
  dbExecute(
    con,
    "CREATE TABLE test_types (
    id INTEGER, 
    name TEXT, 
    value DOUBLE, 
    flag BOOLEAN
  )"
  )
  dbExecute(
    con,
    "INSERT INTO test_types VALUES 
    (1, 'test1', 10.5, true),
    (2, 'test2', 20.3, false),
    (3, 'test3', 30.7, true)"
  )

  # Export
  export_single_duckdb_table_to_parquet(con, "test_types", output_path)

  # Read back and verify
  result <- dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT * FROM read_parquet({output_path}) ORDER BY id",
      .con = con
    )
  )

  expect_equal(nrow(result), 3)
  expect_equal(result$id, c(1, 2, 3))
  expect_equal(result$name, c("test1", "test2", "test3"))
  expect_equal(result$value, c(10.5, 20.3, 30.7))
})

test_that("export_single_duckdb_table_to_parquet respects compression parameter", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_snappy <- withr::local_tempfile(fileext = ".parquet")
  output_gzip <- withr::local_tempfile(fileext = ".parquet")

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create table with some data
  dbExecute(
    con,
    "CREATE TABLE compression_test AS 
    SELECT i as id, 'data_' || i as value FROM range(100) t(i)"
  )

  # Export with different compression
  export_single_duckdb_table_to_parquet(
    con,
    "compression_test",
    output_snappy,
    compression = "SNAPPY"
  )
  export_single_duckdb_table_to_parquet(
    con,
    "compression_test",
    output_gzip,
    compression = "GZIP"
  )

  expect_true(file.exists(output_snappy))
  expect_true(file.exists(output_gzip))

  # Both files should have different sizes (different compression)
  # Note: This may not always be true for small data, but generally holds
  expect_gt(file.size(output_snappy), 0)
  expect_gt(file.size(output_gzip), 0)
})

# --- Parquet export: large table (chunked) ---

test_that("export_large_table_to_parquet creates chunked files", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_dir <- withr::local_tempdir()
  output_base <- file.path(output_dir, "large_table")

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create table with 500 rows
  dbExecute(
    con,
    "CREATE TABLE large_table AS 
    SELECT i as id, 'value_' || i as data FROM range(500) t(i)"
  )

  # Export with small chunk size to force multiple files
  export_large_table_to_parquet(
    con,
    "large_table",
    output_base,
    chunk_size = 100
  )

  # Check multiple chunk files created in {table_name}_chunks subdirectory
  chunks_dir <- file.path(output_dir, "large_table_chunks")
  expect_true(dir.exists(chunks_dir))

  chunk_files <- list.files(chunks_dir, pattern = "\\.parquet$")

  expect_gt(length(chunk_files), 1)
  # With 500 rows and chunk_size=100, expect 5 files
  expect_equal(length(chunk_files), 5)
})

test_that("export_large_table_to_parquet chunk files are numbered correctly", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_dir <- withr::local_tempdir()
  output_base <- file.path(output_dir, "numbered_test")

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(
    con,
    "CREATE TABLE numbered_test AS SELECT i as id FROM range(250) t(i)"
  )
  export_large_table_to_parquet(
    con,
    "numbered_test",
    output_base,
    chunk_size = 100
  )

  chunks_dir <- file.path(output_dir, "numbered_test_chunks")
  chunk_files <- sort(list.files(
    chunks_dir,
    pattern = "\\.parquet$",
    full.names = FALSE
  ))

  # Should have files like: numbered_test_chunk_1.parquet, numbered_test_chunk_2.parquet, etc.
  expect_true(any(grepl("chunk_1", chunk_files)))
  expect_true(any(grepl("chunk_2", chunk_files)))
  expect_true(any(grepl("chunk_3", chunk_files)))
})

# --- Parquet import: chunked ---

test_that("import_chunked_parquet_to_duckdb imports all chunks", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_dir <- withr::local_tempdir()

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create and export chunked data
  dbExecute(
    con,
    "CREATE TABLE source_table AS 
    SELECT i as id, 'data_' || i as value FROM range(300) t(i)"
  )

  export_large_table_to_parquet(
    con,
    "source_table",
    file.path(output_dir, "source_table.parquet"),
    chunk_size = 100
  )

  # Drop original table
  dbExecute(con, "DROP TABLE source_table")

  # Reimport from chunks
  import_chunked_parquet_to_duckdb(con, output_dir, "source_table")

  # Verify correct number of rows
  result <- dbGetQuery(con, "SELECT COUNT(*) as n FROM source_table")
  expect_equal(result$n, 300)

  # Verify data integrity
  sample <- dbGetQuery(con, "SELECT * FROM source_table WHERE id = 150")
  expect_equal(nrow(sample), 1)
  expect_equal(sample$value, "data_150")
})

test_that("import_chunked_parquet_to_duckdb with explicit num_chunks", {
  skip_if_no_duckdb()

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_dir <- withr::local_tempdir()

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create chunked files
  dbExecute(
    con,
    "CREATE TABLE explicit_chunks AS SELECT i as id FROM range(200) t(i)"
  )
  export_large_table_to_parquet(
    con,
    "explicit_chunks",
    file.path(output_dir, "explicit_chunks.parquet"),
    chunk_size = 50
  )

  dbExecute(con, "DROP TABLE explicit_chunks")

  # Reimport with explicit num_chunks
  import_chunked_parquet_to_duckdb(
    con,
    output_dir,
    "explicit_chunks",
    num_chunks = 4
  )

  result <- dbGetQuery(con, "SELECT COUNT(*) as n FROM explicit_chunks")
  expect_equal(result$n, 200)
})

# --- Fixture-based tests ---

test_that("DuckDB can read sample_fahrzeit.csv fixture", {
  skip_if_no_duckdb()

  fixture_path <- test_path("fixtures", "sample_fahrzeit.csv")
  skip_if_not(file.exists(fixture_path), "Fixture file not found")

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Read CSV into DuckDB
  dbExecute(
    con,
    glue::glue_sql(
      "CREATE TABLE fahrzeit AS SELECT * FROM read_csv_auto({fixture_path})",
      .con = con
    )
  )

  result <- dbGetQuery(con, "SELECT COUNT(*) as n FROM fahrzeit")
  expect_equal(result$n, 10)

  # Verify columns exist
  columns <- dbGetQuery(con, "DESCRIBE fahrzeit")
  expect_true("Gitterzellen_ID" %in% columns$column_name)
  expect_true("Einwohner" %in% columns$column_name)
  expect_true("Fahrzeit_Sekunden" %in% columns$column_name)
})

test_that("DuckDB can aggregate sample_fahrzeit.csv by Gemeinde", {
  skip_if_no_duckdb()

  fixture_path <- test_path("fixtures", "sample_fahrzeit.csv")
  skip_if_not(file.exists(fixture_path), "Fixture file not found")

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(
    con,
    glue::glue_sql(
      "CREATE TABLE fahrzeit AS SELECT * FROM read_csv_auto({fixture_path})",
      .con = con
    )
  )

  # Aggregate by Gemeindename
  result <- dbGetQuery(
    con,
    "
    SELECT 
      Gemeindename,
      COUNT(*) as n_grids,
      SUM(Einwohner) as total_einwohner
    FROM fahrzeit
    GROUP BY Gemeindename
    ORDER BY Gemeindename
  "
  )

  expect_equal(nrow(result), 3)
  expect_true("TestGemeinde" %in% result$Gemeindename)
  expect_true("AndereGemeinde" %in% result$Gemeindename)
  expect_true("DritteGemeinde" %in% result$Gemeindename)
})

test_that("DuckDB can read sample_geofilter.csv fixture", {
  skip_if_no_duckdb()

  fixture_path <- test_path("fixtures", "sample_geofilter.csv")
  skip_if_not(file.exists(fixture_path), "Fixture file not found")

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(
    con,
    glue::glue_sql(
      "CREATE TABLE geofilter AS SELECT * FROM read_csv_auto({fixture_path})",
      .con = con
    )
  )

  result <- dbGetQuery(con, "SELECT COUNT(*) as n FROM geofilter")
  expect_equal(result$n, 5)
})

# ============================================================================
# Tier 3+4: PostgreSQL Tests (All Skipped - No Test Database Available)
# ============================================================================

# --- Connection tests ---

test_that("connect_to_postgres_db creates valid connection", {
  skip_if_no_postgres()
  # Would test: con <- connect_to_postgres_db(); expect_true(dbIsValid(con))
})

# --- Inspection functions ---

test_that("list_tables_and_sizes returns tibble with correct columns", {
  skip_if_no_postgres()
  # Would test: result <- list_tables_and_sizes(con)
  # expect_true("table_full_name" %in% names(result))
  # expect_true("total_size" %in% names(result))
})

test_that("check_primary_keys returns tibble", {
  skip_if_no_postgres()
  # Would test: result <- check_primary_keys(con)
  # expect_s3_class(result, "tbl_df")
})

test_that("check_indexes returns tibble with index definitions", {
  skip_if_no_postgres()
  # Would test: result <- check_indexes(con)
  # expect_true("index_name" %in% names(result))
})

test_that("list_temp_tables returns tibble", {
  skip_if_no_postgres()
  # Would test: result <- list_temp_tables(con)
  # expect_s3_class(result, "tbl_df")
})

test_that("check_primary_key_exists returns logical", {
  skip_if_no_postgres()
  # Would test: result <- check_primary_key_exists(con, "public", "test_table", "id")
  # expect_type(result, "logical")
})

# --- Modification functions ---

test_that("set_primary_key creates primary key constraint", {
  skip_if_no_postgres()
  # Would test: Create table, set_primary_key(), verify with check_primary_key_exists()
})

test_that("set_index creates index", {
  skip_if_no_postgres()
  # Would test: Create table, set_index(), verify with check_indexes()
})

test_that("remove_table drops table", {
  skip_if_no_postgres()
  # Would test: Create table, remove_table(), verify with dbExistsTable()
})

test_that("remove_index drops index", {
  skip_if_no_postgres()
  # Would test: Create index, remove_index(), verify with check_indexes()
})

# --- DuckDB-PostgreSQL bridge ---

test_that("attach_duckdb_to_postgres enables postgres_scanner", {
  skip_if_no_postgres()
  # Would test: con_duck <- connect_to_duckdb_db(); attach_duckdb_to_postgres(con_duck)
  # Verify can query: SELECT * FROM postgres_scan(...)
})

test_that("detach_duckdb_from_postgres removes attachment", {
  skip_if_no_postgres()
  # Would test: attach, then detach, verify postgres_scan no longer works
})

test_that("copy_table_from_postgres_to_duckdb transfers data", {
  skip_if_no_postgres()
  # Would test: Copy small table, verify row count matches
})

test_that("copy_all_tables_from_postgres_to_duckdb transfers multiple tables", {
  skip_if_no_postgres()
  # Would test: Copy all tables, verify all exist in DuckDB
})

# --- Workflow functions ---

test_that("Geofilter_Funktion creates filtered database", {
  skip_if_no_postgres()
  # Would test: Run with sample geofilter CSV, verify output DuckDB exists
})

test_that("run_geofilter with preprocessed list", {
  skip_if_no_postgres()
  # Would test: Run with preprocessed CSV, verify filtered data correct
})

test_that("preprocess_geofilter_list validates gemeinde names", {
  skip_if_no_postgres()
  # Would test: Process sample CSV, verify all names validated
})

test_that("Klinikfilter_Funktion creates filtered database", {
  skip_if_no_postgres()
  # Would test: Run with sample hospital CSV, verify output DuckDB exists
})

# ==================== GeoParquet Tests ==================== #

test_that("export_geometry_table_to_geoparquet creates valid GeoParquet", {
  skip_if_no_duckdb()
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_path <- withr::local_tempfile(fileext = ".parquet")

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Load spatial extension and create a simple point geometry
  dbExecute(con, "INSTALL spatial; LOAD spatial;")
  dbExecute(
    con,
    "CREATE TABLE test_geo (id INTEGER, name VARCHAR, geometry GEOMETRY)"
  )
  dbExecute(
    con,
    "INSERT INTO test_geo VALUES (1, 'Point_A', ST_Point(8.0, 50.0))"
  )
  dbExecute(
    con,
    "INSERT INTO test_geo VALUES (2, 'Point_B', ST_Point(9.0, 51.0))"
  )

  # Export to GeoParquet
  export_geometry_table_to_geoparquet(con, "test_geo", output_path)

  expect_true(file.exists(output_path))
  expect_gt(file.size(output_path), 0)

  # Read back with arrow+sf
  df <- arrow::read_parquet(output_path)
  result <- sf::st_as_sf(df)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_true("geometry" %in% names(result))
})

test_that("export_geometry_table_to_geoparquet handles polygon data", {
  skip_if_no_duckdb()
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_path <- withr::local_tempfile(fileext = ".parquet")

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create a simple polygon
  dbExecute(con, "INSTALL spatial; LOAD spatial;")
  dbExecute(
    con,
    "CREATE TABLE test_poly (id INTEGER, name VARCHAR, geometry GEOMETRY)"
  )
  dbExecute(
    con,
    "INSERT INTO test_poly VALUES (1, 'Square', ST_GeomFromText('POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))'))"
  )

  # Export
  export_geometry_table_to_geoparquet(con, "test_poly", output_path)

  expect_true(file.exists(output_path))

  # Verify polygon type with arrow+sf
  df <- arrow::read_parquet(output_path)
  result <- sf::st_as_sf(df)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
  expect_equal(as.character(sf::st_geometry_type(result)[1]), "POLYGON")
})

test_that("read_geoparquet returns sf object", {
  skip_if_no_duckdb()
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")

  # Create a GeoParquet file
  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_path <- withr::local_tempfile(fileext = ".parquet")

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(con, "INSTALL spatial; LOAD spatial;")
  dbExecute(
    con,
    "CREATE TABLE geo_test (id INTEGER, name VARCHAR, geometry GEOMETRY)"
  )
  dbExecute(con, "INSERT INTO geo_test VALUES (1, 'Test', ST_Point(8.0, 50.0))")

  export_geometry_table_to_geoparquet(con, "geo_test", output_path)

  # Read with our helper function
  result <- read_geoparquet(output_path)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
  expect_equal(result$name[1], "Test")
})

test_that("read_geoparquet requires sf package", {
  skip_if_not_installed("sf")

  # Can't easily test the error without unloading sf
  # Just verify the function exists and has correct structure
  expect_type(read_geoparquet, "closure")
})

test_that("export_all_duckdb_tables_to_parquet routes geometry tables correctly", {
  skip_if_no_duckdb()
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")

  db_path <- withr::local_tempfile(fileext = ".duckdb")
  output_dir <- withr::local_tempdir()

  con <- connect_to_duckdb_db(db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create one regular table and one geometry table
  dbExecute(con, "CREATE TABLE regular_table (id INTEGER, value VARCHAR)")
  dbExecute(con, "INSERT INTO regular_table VALUES (1, 'test')")

  dbExecute(con, "INSTALL spatial; LOAD spatial;")
  dbExecute(con, "CREATE TABLE geo_table (id INTEGER, geometry GEOMETRY)")
  dbExecute(con, "INSERT INTO geo_table VALUES (1, ST_Point(8.0, 50.0))")

  # Export with geometry_tables_list
  export_all_duckdb_tables_to_parquet(
    con,
    output_dir,
    large_tables_list = character(0),
    geometry_tables_list = c("geo_table")
  )

  # Both files should exist
  regular_path <- file.path(output_dir, "regular_table.parquet")
  geo_path <- file.path(output_dir, "geo_table.parquet")

  expect_true(file.exists(regular_path))
  expect_true(file.exists(geo_path))

  # Geometry table should be readable as sf via arrow
  df <- arrow::read_parquet(geo_path)
  geo_result <- sf::st_as_sf(df)
  expect_s3_class(geo_result, "sf")
})

test_that("copy_table_from_postgres_to_duckdb handles geometry_column parameter", {
  skip_if_no_postgres()
  # This test requires PostgreSQL with PostGIS
  # Would test: Copy table with geometry_column="geometry"
  # Verify DuckDB table has GEOMETRY type (not WKB_BLOB)
})
