#' Export Complete PostgreSQL Database to DuckDB and Parquet
#'
#' Creates a complete data dump of all tables from the PostgreSQL database to a local
#' DuckDB database and exports them to Parquet files. This function does not apply any
#' filters - it exports all data from all tables.
#'
#' All tabular inputs and outputs are expected to be tibbles.
#'
#' @param duckdb_path Path to the output DuckDB database file (will be created if it doesn't exist).
#' @param output_dir Directory to save the output parquet files (will be created if it doesn't exist).
#' @param pg_schema Schema name in the PostgreSQL database (default: "public").
#' @param relevant_tables Vector of table names to copy from Postgres to DuckDB.
#'   Use "all" to copy all tables from the schema.
#' @param geometry_tables Vector of table names with geometry columns (for GeoParquet export).
#' @param geometry_column Name of the geometry column in geometry tables (default: "geometry").
#' @param compression Compression algorithm for parquet files (default: "SNAPPY").
#' @param expose_connections_to_viewer Whether to expose connections to the RStudio viewer.
#' @param print_connection_info Whether to print connection info.
#' @param disconnect_on_exit Whether to disconnect from databases on exit.
#'
#' @return A list with:
#' \describe{
#'   \item{duckdb_path}{Path to the created DuckDB database file.}
#'   \item{output_dir}{Path to the directory containing Parquet files.}
#'   \item{table_counts}{Named list of row counts for each exported table.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- Komplettexport_Funktion(
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Komplettexport.duckdb",
#'   output_dir = "data/nobackup/Komplettexport"
#' )
#' }
#' @export
Komplettexport_Funktion <- function(
  duckdb_path = file.path(
    "data",
    "nobackup",
    "Entfernungsdaten_Komplettexport.duckdb"
  ),
  output_dir = file.path("data", "nobackup", "Komplettexport"),
  pg_schema = "public",
  relevant_tables = "all",
  geometry_tables = c(
    "Gemeindegrenzen_Polygone",
    "Kreisgrenzen_Polygone",
    "Landesgrenzen_Polygone",
    "Regierungsbezirksgrenzen_Polygone"
  ),
  geometry_column = "geometry",
  compression = "SNAPPY",
  expose_connections_to_viewer = TRUE,
  print_connection_info = FALSE,
  disconnect_on_exit = TRUE
) {
  # -------------------------------------------------------------------- #
  # Error handling and setup
  # -------------------------------------------------------------------- #
  ensure_dir_exists(dirname(duckdb_path))
  ensure_dir_exists(output_dir)

  # -------------------------------------------------------------------- #
  # Connect to Postgres database
  # -------------------------------------------------------------------- #
  cat("Connecting to PostgreSQL database...\n")
  con <- connect_to_postgres_db(
    expose_connection_to_viewer = expose_connections_to_viewer,
    print_connection_info = print_connection_info
  )
  if (is.null(con) || !dbIsValid(con)) {
    stop("Failed to connect to Postgres database.")
  }

  # -------------------------------------------------------------------- #
  # Create connection to DuckDB database
  # -------------------------------------------------------------------- #
  cat("Creating DuckDB database at: ", duckdb_path, "\n")
  con_duck <- connect_to_duckdb_db(
    db_path = duckdb_path,
    expose_connection_to_viewer = expose_connections_to_viewer
  )

  # -------------------------------------------------------------------- #
  # Attach DuckDB to remote Postgres database
  # -------------------------------------------------------------------- #
  cat("Attaching DuckDB to PostgreSQL for streaming reads...\n")
  attach_duckdb_to_postgres(con = con_duck)

  # -------------------------------------------------------------------- #
  # Copy all tables from Postgres to DuckDB
  # -------------------------------------------------------------------- #
  cat("Copying tables from PostgreSQL to DuckDB...\n")
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
  cat("Detaching DuckDB from PostgreSQL...\n")
  detach_duckdb_from_postgres(con_duck)

  # -------------------------------------------------------------------- #
  # Get row count of all tables
  # -------------------------------------------------------------------- #
  cat("Counting rows in exported tables...\n")
  tables_list <- dbListTables(con_duck)
  table_counts <- list()
  for (table in tables_list) {
    final_count <- dbGetQuery(
      con_duck,
      glue_sql("SELECT COUNT(*) FROM {`table`}", .con = con_duck)
    )
    table_counts[[table]] <- final_count[[1]]
    cat(glue("  Table {table}: {final_count[[1]]} rows\n"))
  }

  # -------------------------------------------------------------------- #
  # Export to parquet
  # -------------------------------------------------------------------- #
  cat("Exporting tables to Parquet files...\n")
  export_all_duckdb_tables_to_parquet(
    con = con_duck,
    output_dir = output_dir,
    geometry_tables_list = geometry_tables,
    compression = compression
  )
  cat("Parquet files saved to: ", output_dir, "\n")

  # -------------------------------------------------------------------- #
  # Disconnect if requested
  # -------------------------------------------------------------------- #
  if (disconnect_on_exit) {
    dbDisconnect(con, shutdown = TRUE)
    dbDisconnect(con_duck, shutdown = TRUE)
    cat("Disconnected from DuckDB and PostgreSQL databases.\n")
  }

  # Return relevant information
  list(
    duckdb_path = duckdb_path,
    output_dir = output_dir,
    table_counts = table_counts
  )
}
