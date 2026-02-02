#' Run Klinikfilter
#'
#' Applies a hospital filter to the distance dataset based on a list of hospital site numbers.
#' All tabular inputs and outputs are tibbles.
#'
#' @param krankenhaus_standortnummern_csv_path Path to the CSV file with hospital site numbers to filter by.
#' @param duckdb_path Path to the output DuckDB database file (will be created if it doesn't exist).
#' @param output_dir Directory to save the output parquet files (will be created if it doesn't exist).
#' @param pg_schema Schema name in the PostgreSQL database (default: "public").
#' @param relevant_tables Vector of table names to copy from Postgres to DuckDB.
#' @param geometry_tables Vector of table names with geometry columns (for GeoParquet export).
#' @param geometry_column Name of the geometry column in geometry tables (default: "geometry").
#' @param filter_table_name Name of the filter table in DuckDB.
#' @param filter_column_name Name of the column to filter on.
#' @param target_table_name Name of the target table in DuckDB for filtered data.
#' @param compression Compression algorithm for parquet files.
#' @param work_mem PostgreSQL work_mem setting.
#'
#' @return A list with paths to the created database and parquet files.
#'
#' @examples
#' \dontrun{
#' result <- Klinikfilter_Funktion(
#'   krankenhaus_standortnummern_csv_path = "data/TEMPLATE_Krankenhaus_Standortnummern_large.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Klinikfilter.duckdb",
#'   output_dir = "data/nobackup/Klinikfilter"
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
  filter_table_name = "Klinikfilter",
  filter_column_name = "Krankenhaus_Standortnummer",
  target_table_name = "Entfernungsdaten_subset",
  compression = "SNAPPY",
  work_mem = "512MB"
) {
  # --- Error handling and setup ---
  require_file_exists(krankenhaus_standortnummern_csv_path)
  ensure_dir_exists(dirname(duckdb_path))
  ensure_dir_exists(output_dir)

  # -------------------------------------------------------------------- #
  # Connect to Postgres database
  # -------------------------------------------------------------------- #
  con <- connect_to_postgres_db(
    expose_connection_to_viewer = TRUE,
    print_connection_info = TRUE
  )
  if (is.null(con) || !dbIsValid(con)) {
    stop("Failed to connect to Postgres database.")
  }

  # -------------------------------------------------------------------- #
  # Optimize Postgres settings for the query
  # -------------------------------------------------------------------- #
  dbExecute(con, "SET enable_hashjoin = DEFAULT;")
  dbExecute(con, "SET enable_mergejoin = DEFAULT;")
  dbExecute(con, "SET enable_nestloop = DEFAULT;")
  dbExecute(con, glue::glue("SET work_mem = '{work_mem}';"))
  dbExecute(con, "SET random_page_cost = DEFAULT;")
  dbExecute(con, "SET effective_io_concurrency = DEFAULT;")

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
  # Create temporary table with Krankenhaus_Standortnummern
  # -------------------------------------------------------------------- #
  dbWriteTable(
    con,
    name = "temp_krankenhaus_ids",
    value = krankenhaus_ids,
    temporary = TRUE,
    overwrite = TRUE
  )
  dbExecute(
    con,
    'CREATE INDEX ON temp_krankenhaus_ids ("Krankenhaus_Standortnummer");'
  )
  dbExecute(con, "ANALYZE temp_krankenhaus_ids;")

  # -------------------------------------------------------------------- #
  # Create connection to DuckDB database
  # -------------------------------------------------------------------- #
  con_duck <- connect_to_duckdb_db(
    db_path = duckdb_path,
    expose_connection_to_viewer = TRUE
  )

  # -------------------------------------------------------------------- #
  # Attach to DuckDB to remote Postgres database
  # -------------------------------------------------------------------- #
  attach_duckdb_to_postgres(con = con_duck)

  # -------------------------------------------------------------------- #
  # Copy temporary table to local database
  # -------------------------------------------------------------------- #
  dbExecute(
    con_duck,
    glue_sql(
      "CREATE TABLE IF NOT EXISTS {`filter_table_name`} ({`filter_column_name`} VARCHAR)",
      .con = con_duck
    )
  )
  temp_data <- dbGetQuery(con, "SELECT * FROM temp_krankenhaus_ids")
  dbAppendTable(con_duck, filter_table_name, temp_data)

  # -------------------------------------------------------------------- #
  # Apply Klinikfilter to Entfernungsdaten and save result to DuckDB
  # -------------------------------------------------------------------- #
  cat(
    "Filtering data and streaming from PostgreSQL to DuckDB using ATTACH...\n"
  )
  system.time({
    dbExecute(
      con_duck,
      glue_sql(
        "CREATE TABLE IF NOT EXISTS {`target_table_name`} AS
         SELECT 
           pg_src.public.Entfernungsdaten.Gitterzellen_ID,
           pg_src.public.Entfernungsdaten.Krankenhaus_Standortnummer,
           pg_src.public.Entfernungsdaten.Fahrzeit_Sekunden,
           pg_src.public.Entfernungsdaten.Fahrstrecke_Meter
         FROM {`filter_table_name`}
         INNER JOIN pg_src.public.Entfernungsdaten
           ON {`filter_table_name`}.{`filter_column_name`} = pg_src.public.Entfernungsdaten.{`filter_column_name`}",
        .con = con_duck
      )
    )
  })

  # -------------------------------------------------------------------- #
  # Copy tables to local database
  # -------------------------------------------------------------------- #
  system.time({
    copy_all_tables_from_postgres_to_duckdb(
      con_duck = con_duck,
      con_pg = con,
      pg_schema = pg_schema,
      relevant_tables = relevant_tables,
      geometry_tables = geometry_tables,
      geometry_column = geometry_column
    )
  })

  # -------------------------------------------------------------------- #
  # Detach DuckDB from Postgres
  # -------------------------------------------------------------------- #
  detach_duckdb_from_postgres(con_duck)

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
  export_all_duckdb_tables_to_parquet(
    con = con_duck,
    output_dir = output_dir,
    geometry_tables_list = geometry_tables,
    compression = compression
  )

  # -------------------------------------------------------------------- #
  # Disconnect
  # -------------------------------------------------------------------- #
  dbDisconnect(con, shutdown = TRUE)
  dbDisconnect(con_duck, shutdown = TRUE)

  # Return relevant information
  list(
    duckdb_path = duckdb_path,
    output_dir = output_dir,
    table_counts = table_counts
  )
}
