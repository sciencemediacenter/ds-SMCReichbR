#' Run Klinikfilter
#'
#' Applies a hospital filter to the distance dataset based on a list of hospital site numbers.
#' Supports both PostgreSQL and DuckDB as data sources.
#' All tabular inputs and outputs are tibbles.
#'
#' @param krankenhaus_standortnummern_csv_path Path to the CSV file with hospital site numbers to filter by.
#' @param duckdb_path Path to the output DuckDB database file (will be created if it doesn't exist).
#' @param output_dir Directory to save the output parquet files (will be created if it doesn't exist).
#' @param source Data source type: "postgres" or "duckdb" (default: "postgres").
#' @param source_duckdb_path Path to DuckDB source database (required if source = "duckdb").
#' @param pg_options List of PostgreSQL optimization settings (merged with defaults).
#'   Available options: work_mem, maintenance_work_mem, effective_cache_size,
#'   random_page_cost, effective_io_concurrency, max_parallel_workers_per_gather.
#'   Set a value to NULL to use database default for that setting.
#' @param duckdb_options List of DuckDB optimization settings.
#'   Available options: threads, memory_limit.
#' @param pg_schema Schema name in the PostgreSQL database (default: "public").
#' @param relevant_tables Vector of table names to copy from source to DuckDB.
#' @param geometry_tables Vector of table names with geometry columns (for GeoParquet export).
#' @param geometry_column Name of the geometry column in geometry tables (default: "geometry").
#' @param tables_to_copy Optional subset of tables to copy (default: all relevant_tables).
#' @param filter_table_name Name of the filter table in DuckDB.
#' @param filter_column_name Name of the column to filter on.
#' @param target_table_name Name of the target table in DuckDB for filtered data.
#' @param compression Compression algorithm for parquet files.
#' @param expose_connections_to_viewer Whether to expose connections to the viewer.
#' @param print_connection_info Whether to print connection info.
#' @param disconnect_on_exit Whether to disconnect from databases on exit.
#'
#' @return A list with paths to the created database and parquet files.
#'
#' @examples
#' \dontrun{
#' # Using PostgreSQL source (default)
#' result <- Klinikfilter_Funktion(
#'   krankenhaus_standortnummern_csv_path = "data/TEMPLATE_Krankenhaus_Standortnummern_large.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Klinikfilter.duckdb",
#'   output_dir = "data/nobackup/Klinikfilter"
#' )
#'
#' # Using DuckDB Komplettexport as source
#' result <- Klinikfilter_Funktion(
#'   krankenhaus_standortnummern_csv_path = "data/TEMPLATE_Krankenhaus_Standortnummern_large.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Klinikfilter.duckdb",
#'   output_dir = "data/nobackup/Klinikfilter",
#'   source = "duckdb",
#'   source_duckdb_path = "data/nobackup/Komplettexport.duckdb"
#' )
#'
#' # PostgreSQL with custom optimization settings
#' result <- Klinikfilter_Funktion(
#'   krankenhaus_standortnummern_csv_path = "data/TEMPLATE_Krankenhaus_Standortnummern_large.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Klinikfilter.duckdb",
#'   output_dir = "data/nobackup/Klinikfilter",
#'   pg_options = list(work_mem = "1GB")
#' )
#' }
#' @export
Klinikfilter_Funktion <- function(
  krankenhaus_standortnummern_csv_path = "data/TEMPLATE_Krankenhaus_Standortnummern_large.csv",
  duckdb_path = file.path(
    "data",
    "nobackup",
    "Entfernungsdaten_Klinikfilter.duckdb"
  ),
  output_dir = "data/nobackup/Klinikfilter",
  source = "postgres",
  source_duckdb_path = NULL,
  pg_options = list(),
  duckdb_options = list(),
  pg_schema = "public",
  relevant_tables = c(
    "Gitterzellen_Gemeinde_Mapping",
    "Gitterzellen_Einwohner_Mapping",
    "Verwaltungsgebiete_Mapping",
    "Gemeindegrenzen_Polygone",
    "Kreisgrenzen_Polygone",
    "Landesgrenzen_Polygone",
    "Regierungsbezirksgrenzen_Polygone",
    "Krankenhaus_Standortliste"
  ),
  geometry_tables = c(
    "Gemeindegrenzen_Polygone",
    "Kreisgrenzen_Polygone",
    "Landesgrenzen_Polygone",
    "Regierungsbezirksgrenzen_Polygone",
    "Krankenhaus_Standortliste"
  ),
  geometry_column = "geometry",
  tables_to_copy = NULL,
  filter_table_name = "Klinikfilter",
  filter_column_name = "Krankenhaus_Standortnummer",
  target_table_name = "Entfernungsdaten_subset",
  compression = "SNAPPY",
  expose_connections_to_viewer = TRUE,
  print_connection_info = FALSE,
  disconnect_on_exit = TRUE
) {
  # --- Error handling and setup ---
  require_file_exists(krankenhaus_standortnummern_csv_path)
  ensure_dir_exists(dirname(duckdb_path))
  ensure_dir_exists(output_dir)

  # -------------------------------------------------------------------- #
  # Create source configuration
  # -------------------------------------------------------------------- #
  source_config <- create_source_config(
    source = source,
    source_duckdb_path = source_duckdb_path,
    pg_options = pg_options,
    duckdb_options = duckdb_options
  )

  # -------------------------------------------------------------------- #
  # Load Klinikliste
  # -------------------------------------------------------------------- #
  krankenhaus_ids <- read_csv(
    krankenhaus_standortnummern_csv_path,
    col_types = cols(
      .default = col_character(),
      Krankenhaus_Standortnummer = col_character()
    )
  )
  if (!tibble::is_tibble(krankenhaus_ids)) {
    krankenhaus_ids <- as_tibble(krankenhaus_ids)
  }
  if (!"Krankenhaus_Standortnummer" %in% names(krankenhaus_ids)) {
    stop("Missing required column 'Krankenhaus_Standortnummer' in input CSV.")
  }

  # -------------------------------------------------------------------- #
  # Create connection to target DuckDB database
  # -------------------------------------------------------------------- #
  con_duck <- connect_to_duckdb_db(
    db_path = duckdb_path,
    expose_connection_to_viewer = expose_connections_to_viewer
  )

  # -------------------------------------------------------------------- #
  # Connect to data source
  # -------------------------------------------------------------------- #
  cat("Connecting to data source (", source, ")...\n", sep = "")
  source_conn <- connect_to_source(
    source = source_config,
    con_duck = con_duck,
    expose_connection_to_viewer = expose_connections_to_viewer,
    print_connection_info = print_connection_info
  )

  # -------------------------------------------------------------------- #
  # Apply source-specific optimizations
  # -------------------------------------------------------------------- #
  if (inherits(source_config, "pg_source")) {
    apply_source_optimizations(source_config, source_conn$con)
    # Reset some PostgreSQL settings to DEFAULT for Klinikfilter
    # (different optimization strategy than Geofilter)
    dbExecute(source_conn$con, "SET enable_hashjoin = DEFAULT;")
    dbExecute(source_conn$con, "SET enable_mergejoin = DEFAULT;")
    dbExecute(source_conn$con, "SET enable_nestloop = DEFAULT;")
    dbExecute(source_conn$con, "SET random_page_cost = DEFAULT;")
    dbExecute(source_conn$con, "SET effective_io_concurrency = DEFAULT;")
  } else if (inherits(source_config, "duckdb_source")) {
    apply_source_optimizations(source_config, con_duck)
  }

  # -------------------------------------------------------------------- #
  # Create filter table in target DuckDB
  # -------------------------------------------------------------------- #
  if (inherits(source_config, "pg_source")) {
    # PostgreSQL: Create temporary table with index for optimization
    dbWriteTable(
      source_conn$con,
      name = "temp_krankenhaus_ids",
      value = krankenhaus_ids,
      temporary = TRUE,
      overwrite = TRUE
    )
    dbExecute(
      source_conn$con,
      'CREATE INDEX ON temp_krankenhaus_ids ("Krankenhaus_Standortnummer");'
    )
    dbExecute(source_conn$con, "ANALYZE temp_krankenhaus_ids;")
  }

  # Create filter table in target DuckDB
  dbExecute(
    con_duck,
    glue_sql(
      "CREATE TABLE IF NOT EXISTS {`filter_table_name`} ({`filter_column_name`} VARCHAR)",
      .con = con_duck
    )
  )

  if (inherits(source_config, "pg_source")) {
    temp_data <- dbGetQuery(source_conn$con, "SELECT * FROM temp_krankenhaus_ids")
  } else {
    temp_data <- krankenhaus_ids
  }
  dbAppendTable(con_duck, filter_table_name, temp_data)

  # -------------------------------------------------------------------- #
  # Apply Klinikfilter to Entfernungsdaten and save result to DuckDB
  # -------------------------------------------------------------------- #
  cat("Filtering data and streaming from ", source, " to DuckDB...\n", sep = "")

  entfernungsdaten_ref <- get_source_table_ref(source_config, "Entfernungsdaten", pg_schema)

  system.time({
    dbExecute(
      con_duck,
      glue_sql(
        "CREATE TABLE IF NOT EXISTS {`target_table_name`} AS
         SELECT
           {DBI::SQL(entfernungsdaten_ref)}.Gitterzellen_ID,
           {DBI::SQL(entfernungsdaten_ref)}.Krankenhaus_Standortnummer,
           {DBI::SQL(entfernungsdaten_ref)}.Fahrzeit_Sekunden,
           {DBI::SQL(entfernungsdaten_ref)}.Fahrstrecke_Meter
         FROM {`filter_table_name`}
         INNER JOIN {DBI::SQL(entfernungsdaten_ref)}
           ON {`filter_table_name`}.{`filter_column_name`} = {DBI::SQL(entfernungsdaten_ref)}.{`filter_column_name`}",
        .con = con_duck
      )
    )
  })

  # -------------------------------------------------------------------- #
  # Copy supporting tables from source to target DuckDB
  # -------------------------------------------------------------------- #
  cat("Copying supporting tables...\n")
  tables_to_copy_final <- if (!is.null(tables_to_copy)) tables_to_copy else relevant_tables

  system.time({
    copy_tables_from_source(
      source = source_config,
      con_source = source_conn$con,
      con_duck = con_duck,
      pg_schema = pg_schema,
      tables = tables_to_copy_final,
      geometry_tables = geometry_tables,
      geometry_column = geometry_column
    )
  })

  # -------------------------------------------------------------------- #
  # Disconnect from source
  # -------------------------------------------------------------------- #
  cat("Disconnecting from source...\n")
  disconnect_source(source_config, source_conn)

  # -------------------------------------------------------------------- #
  # Get row count of all tables
  # -------------------------------------------------------------------- #
  tables_list <- dbListTables(con_duck)
  table_counts <- list()
  for (table in tables_list) {
    final_count <- dbGetQuery(
      con_duck,
      glue_sql("SELECT COUNT(*) FROM {`table`}", .con = con_duck)
    )
    table_counts[[table]] <- final_count[[1]]
    print(glue("Final table {table} has {final_count[[1]]} rows\n"))
  }

  # -------------------------------------------------------------------- #
  # Export to parquet
  # -------------------------------------------------------------------- #
  cat("Exporting to Parquet...\n")
  export_all_duckdb_tables_to_parquet(
    con = con_duck,
    output_dir = output_dir,
    geometry_tables_list = geometry_tables,
    compression = compression
  )

  # -------------------------------------------------------------------- #
  # Disconnect if requested
  # -------------------------------------------------------------------- #
  if (disconnect_on_exit) {
    dbDisconnect(con_duck, shutdown = TRUE)
  }

  # Return relevant information
  list(
    duckdb_path = duckdb_path,
    output_dir = output_dir,
    table_counts = table_counts,
    source = source
  )
}
