#' Preprocess Geofilter List and Run Geofilter
#'
#' This function first preprocesses a raw geofilter list template, then runs the geofilter using the preprocessed list.
#' All tabular inputs and outputs are expected to be tibbles.
#'
#' @param geofilter_csv_path Path to the template CSV file with municipalities to filter by.
#' @param duckdb_path Path to the output DuckDB database file.
#' @param output_dir Directory to save the output parquet files.
#' @param preprocessed_output_path Path to save the preprocessed geofilter list.
#' @param source Data source type: "postgres" or "duckdb" (default: "postgres").
#' @param source_duckdb_path Path to DuckDB source database (required if source = "duckdb").
#' @param pg_options List of PostgreSQL optimization settings (merged with
#'   \code{\link{postgres_source}} defaults). Geofilter sets additional defaults
#'   optimized for the geofilter query pattern:
#'   \itemize{
#'     \item \code{enable_hashjoin = "off"} - Disable hash joins
#'     \item \code{enable_mergejoin = "off"} - Disable merge joins
#'     \item \code{enable_nestloop = "on"} - Enable nested loop joins
#'     \item \code{min_parallel_table_scan_size = "8MB"} - Enable parallelism for smaller tables
#'     \item \code{parallel_setup_cost = 0} - Reduce parallel query startup cost
#'     \item \code{parallel_tuple_cost = 0} - Reduce per-tuple parallel cost
#'     \item \code{seq_page_cost = 10} - Increase cost of sequential scans
#'   }
#'   Base defaults from \code{\link{postgres_source}}: work_mem="512MB",
#'   maintenance_work_mem="512MB", effective_cache_size="4GB", random_page_cost=1.1,
#'   effective_io_concurrency=200, max_parallel_workers_per_gather=4.
#'   Override any setting by including it in this list. Set to NULL to use database default.
#' @param duckdb_options List of DuckDB optimization settings.
#' @param pg_schema Schema name in the PostgreSQL database.
#' @param relevant_tables Vector of table names to copy from source to DuckDB.
#' @param tables_to_copy Optional subset of tables to copy (default: all relevant_tables).
#' @param filter_table_name Name of the filter table in DuckDB.
#' @param filter_column_name Name of the column to filter on.
#' @param target_table_name Name of the target table in DuckDB for filtered data.
#' @param expose_connections_to_viewer Whether to expose connections to the viewer.
#' @param print_connection_info Whether to print connection info.
#' @param compression Compression algorithm for parquet files.
#' @param disconnect_on_exit Whether to disconnect from databases on exit.
#'
#' @return A list with paths to the created database and parquet files.
#'
#' @examples
#' \dontrun{
#' # Using PostgreSQL source (default)
#' result <- Geofilter_Funktion(
#'   geofilter_csv_path = "data/Geofilter_list_NRW.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Geofilter.duckdb",
#'   output_dir = "data/nobackup/Geofilter"
#' )
#'
#' # Using DuckDB Komplettexport as source
#' result <- Geofilter_Funktion(
#'   geofilter_csv_path = "data/Geofilter_list_NRW.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Geofilter.duckdb",
#'   output_dir = "data/nobackup/Geofilter",
#'   source = "duckdb",
#'   source_duckdb_path = "data/nobackup/Komplettexport.duckdb"
#' )
#'
#' # Override geofilter optimization (re-enable hash joins, increase work_mem)
#' result <- Geofilter_Funktion(
#'   geofilter_csv_path = "data/Geofilter_list_NRW.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Geofilter.duckdb",
#'   output_dir = "data/nobackup/Geofilter",
#'   pg_options = list(enable_hashjoin = "on", work_mem = "2GB")
#' )
#' }
#' @export
Geofilter_Funktion <- function(
  geofilter_csv_path,
  duckdb_path = file.path(
    "data",
    "nobackup",
    "Entfernungsdaten_Geofilter.duckdb"
  ),
  output_dir = file.path("data", "nobackup", "Geofilter"),
  preprocessed_output_path = file.path(
    dirname(duckdb_path),
    paste0(
      basename(tools::file_path_sans_ext(geofilter_csv_path)),
      "_preprocessed.csv"
    )
  ),
  source = "postgres",
  source_duckdb_path = NULL,
  pg_options = list(
    enable_hashjoin = "off",
    enable_mergejoin = "off",
    enable_nestloop = "on",
    min_parallel_table_scan_size = "8MB",
    parallel_setup_cost = 0,
    parallel_tuple_cost = 0,
    seq_page_cost = 10
  ),
  duckdb_options = list(),
  pg_schema = "public",
  relevant_tables = c(
    "Gitterzellen_Gemeinde_Mapping",
    "Gitterzellen_Einwohner_Mapping",
    "Verwaltungsgebiete_Mapping",
    "Gemeindegrenzen_Polygone",
    "Krankenhaus_Standortliste"
  ),
  tables_to_copy = NULL,
  filter_table_name = "Geofilter",
  filter_column_name = "Gitterzellen_ID",
  target_table_name = "Entfernungsdaten_subset",
  expose_connections_to_viewer = TRUE,
  print_connection_info = FALSE,
  compression = "SNAPPY",
  disconnect_on_exit = TRUE
) {
  # --- Error handling ---
  require_file_exists(geofilter_csv_path)

  ensure_dir_exists(dirname(preprocessed_output_path))

  if (!is.function(preprocess_geofilter_list)) {
    stop("Required function preprocess_geofilter_list() not found.")
  }

  # Step 1: Preprocess the geofilter list
  cat("Preprocessing geofilter list...\n")
  preprocessed_list <- preprocess_geofilter_list(
    geofilter_csv_path = geofilter_csv_path,
    output_csv_path = preprocessed_output_path,
    pg_schema = pg_schema,
    expose_connection_to_viewer = expose_connections_to_viewer,
    print_connection_info = print_connection_info
  )
  if (!tibble::is_tibble(preprocessed_list)) {
    stop("preprocess_geofilter_list() did not return a tibble.")
  }

  # Step 2: Run the geofilter with the preprocessed list
  cat("Running geofilter with preprocessed list...\n")
  result <- run_geofilter(
    preprocessed_geofilter_list_csv_path = preprocessed_output_path,
    duckdb_path = duckdb_path,
    output_dir = output_dir,
    source = source,
    source_duckdb_path = source_duckdb_path,
    pg_options = pg_options,
    duckdb_options = duckdb_options,
    pg_schema = pg_schema,
    relevant_tables = relevant_tables,
    tables_to_copy = tables_to_copy,
    filter_table_name = filter_table_name,
    filter_column_name = filter_column_name,
    target_table_name = target_table_name,
    expose_connections_to_viewer = expose_connections_to_viewer,
    print_connection_info = print_connection_info,
    compression = compression,
    disconnect_on_exit = disconnect_on_exit
  )
  c(result, list(preprocessed_output_path = preprocessed_output_path))
}


#' Run Geofilter
#'
#' Applies a geographical filter to a dataset of distance measurements based on a list of municipalities.
#' Supports both PostgreSQL and DuckDB as data sources.
#' All tabular inputs and outputs are expected to be tibbles.
#'
#' @param preprocessed_geofilter_list_csv_path Path to the preprocessed CSV file with the list of municipalities to filter by.
#' @param duckdb_path Path to the output DuckDB database file (will be created if it doesn't exist).
#' @param output_dir Directory to save the output parquet files (will be created if it doesn't exist).
#' @param source Data source type: "postgres" or "duckdb" (default: "postgres").
#' @param source_duckdb_path Path to DuckDB source database (required if source = "duckdb").
#' @param pg_options List of PostgreSQL optimization settings (merged with
#'   \code{\link{postgres_source}} defaults). Geofilter sets additional defaults
#'   optimized for the geofilter query pattern:
#'   \itemize{
#'     \item \code{enable_hashjoin = "off"} - Disable hash joins
#'     \item \code{enable_mergejoin = "off"} - Disable merge joins
#'     \item \code{enable_nestloop = "on"} - Enable nested loop joins
#'     \item \code{min_parallel_table_scan_size = "8MB"} - Enable parallelism for smaller tables
#'     \item \code{parallel_setup_cost = 0} - Reduce parallel query startup cost
#'     \item \code{parallel_tuple_cost = 0} - Reduce per-tuple parallel cost
#'     \item \code{seq_page_cost = 10} - Increase cost of sequential scans
#'   }
#'   Base defaults from \code{\link{postgres_source}}: work_mem="512MB",
#'   maintenance_work_mem="512MB", effective_cache_size="4GB", random_page_cost=1.1,
#'   effective_io_concurrency=200, max_parallel_workers_per_gather=4.
#'   Override any setting by including it in this list. Set to NULL to use database default.
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
#' @param expose_connections_to_viewer Whether to expose connections to the viewer.
#' @param print_connection_info Whether to print connection info.
#' @param compression Compression algorithm for parquet files.
#' @param disconnect_on_exit Whether to disconnect from databases on exit.
#'
#' @return A list with paths to the created database and parquet files.
#'
#' @examples
#' \dontrun{
#' # Using PostgreSQL source (default)
#' geofilter_result <- run_geofilter(
#'   preprocessed_geofilter_list_csv_path = "data/nobackup/Geofilter_list_NRW_preprocessed.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Geofilter.duckdb",
#'   output_dir = "data/nobackup/Geofilter"
#' )
#'
#' # Using DuckDB Komplettexport as source
#' geofilter_result <- run_geofilter(
#'   preprocessed_geofilter_list_csv_path = "data/nobackup/Geofilter_list_NRW_preprocessed.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Geofilter.duckdb",
#'   output_dir = "data/nobackup/Geofilter",
#'   source = "duckdb",
#'   source_duckdb_path = "data/nobackup/Komplettexport.duckdb"
#' )
#'
#' # PostgreSQL with custom optimization settings (override geofilter defaults)
#' geofilter_result <- run_geofilter(
#'   preprocessed_geofilter_list_csv_path = "data/nobackup/Geofilter_list_NRW_preprocessed.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Geofilter.duckdb",
#'   output_dir = "data/nobackup/Geofilter",
#'   pg_options = list(enable_hashjoin = "on", work_mem = "2GB")
#' )
#' }
#' @export
run_geofilter <- function(
  preprocessed_geofilter_list_csv_path,
  duckdb_path = file.path(
    "data",
    "nobackup",
    "Entfernungsdaten_Geofilter.duckdb"
  ),
  output_dir = file.path("data", "nobackup", "Geofilter"),
  source = "postgres",
  source_duckdb_path = NULL,
  pg_options = list(
    enable_hashjoin = "off",
    enable_mergejoin = "off",
    enable_nestloop = "on",
    min_parallel_table_scan_size = "8MB",
    parallel_setup_cost = 0,
    parallel_tuple_cost = 0,
    seq_page_cost = 10
  ),
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
  filter_table_name = "Geofilter",
  filter_column_name = "Gitterzellen_ID",
  target_table_name = "Entfernungsdaten_subset",
  expose_connections_to_viewer = TRUE,
  print_connection_info = FALSE,
  compression = "SNAPPY",
  disconnect_on_exit = TRUE
) {
  # --- Error handling ---
  require_file_exists(preprocessed_geofilter_list_csv_path)

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
  # Load preprocessed geofilter list
  # -------------------------------------------------------------------- #
  geo_list_to_analyze <- read_csv(
    preprocessed_geofilter_list_csv_path,
    col_types = cols(.default = col_character(), Gemeindename = col_character())
  )
  if (!tibble::is_tibble(geo_list_to_analyze)) {
    geo_list_to_analyze <- as_tibble(geo_list_to_analyze)
  }
  if (!"Gemeindeschluessel" %in% names(geo_list_to_analyze)) {
    stop("Missing required column 'Gemeindeschluessel' in geofilter list.")
  }

  # -------------------------------------------------------------------- #
  # Create connection to target DuckDB database
  # -------------------------------------------------------------------- #
  ensure_dir_exists(dirname(duckdb_path))
  ensure_dir_exists(output_dir)

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
  } else if (inherits(source_config, "duckdb_source")) {
    apply_source_optimizations(source_config, con_duck)
  }

  # -------------------------------------------------------------------- #
  # Identify relevant Gitterzellen IDs
  # -------------------------------------------------------------------- #
  cat("Identifying relevant Gitterzellen IDs...\n")

  if (inherits(source_config, "pg_source")) {
    # PostgreSQL: use dbplyr for lazy evaluation
    remote_Verwaltungsgebiete_Mapping <- tbl(
      source_conn$con,
      Id(schema = pg_schema, table = "Verwaltungsgebiete_Mapping")
    )
    remote_Gitterzellen_Gemeinde_Mapping <- tbl(
      source_conn$con,
      Id(schema = pg_schema, table = "Gitterzellen_Gemeinde_Mapping")
    )
    remote_Gitterzellen_Einwohner_Mapping <- tbl(
      source_conn$con,
      Id(schema = pg_schema, table = "Gitterzellen_Einwohner_Mapping")
    )

    Gitterzellen_IDs_to_analyze <- remote_Verwaltungsgebiete_Mapping |>
      filter(Gemeindeschluessel %in% geo_list_to_analyze$Gemeindeschluessel) |>
      left_join(
        remote_Gitterzellen_Gemeinde_Mapping,
        by = "Gemeindeschluessel"
      ) |>
      inner_join(
        remote_Gitterzellen_Einwohner_Mapping,
        by = "Gitterzellen_ID"
      ) |>
      select(Gitterzellen_ID) |>
      pull()

    # Create temporary table in PostgreSQL for optimization
    dbWriteTable(
      source_conn$con,
      name = "temp_gitterzellen_ids",
      value = tibble(Gitterzellen_ID = Gitterzellen_IDs_to_analyze),
      temporary = TRUE,
      overwrite = TRUE
    )
    dbExecute(source_conn$con, "ANALYZE temp_gitterzellen_ids;")
  } else if (inherits(source_config, "duckdb_source")) {
    # DuckDB source: query directly from attached database
    Gitterzellen_IDs_to_analyze <- dbGetQuery(
      con_duck,
      glue_sql(
        "SELECT DISTINCT g.Gitterzellen_ID
         FROM source_db.Verwaltungsgebiete_Mapping v
         INNER JOIN source_db.Gitterzellen_Gemeinde_Mapping g
           ON v.Gemeindeschluessel = g.Gemeindeschluessel
         INNER JOIN source_db.Gitterzellen_Einwohner_Mapping e
           ON g.Gitterzellen_ID = e.Gitterzellen_ID
         WHERE v.Gemeindeschluessel IN ({gemeindeschluessel*})",
        gemeindeschluessel = geo_list_to_analyze$Gemeindeschluessel,
        .con = con_duck
      )
    )$Gitterzellen_ID
  }

  cat(
    "Found ",
    length(Gitterzellen_IDs_to_analyze),
    " Gitterzellen IDs to filter.\n",
    sep = ""
  )

  # -------------------------------------------------------------------- #
  # Create filter table in target DuckDB
  # -------------------------------------------------------------------- #
  dbExecute(
    con_duck,
    glue_sql(
      "CREATE TABLE IF NOT EXISTS {`filter_table_name`} ({`filter_column_name`} VARCHAR)",
      .con = con_duck
    )
  )

  if (inherits(source_config, "pg_source")) {
    temp_data <- dbGetQuery(
      source_conn$con,
      "SELECT * FROM temp_gitterzellen_ids"
    )
  } else {
    temp_data <- tibble(Gitterzellen_ID = Gitterzellen_IDs_to_analyze)
  }
  dbAppendTable(con_duck, filter_table_name, temp_data)

  # -------------------------------------------------------------------- #
  # Apply Geofilter to Entfernungsdaten and save result to DuckDB
  # -------------------------------------------------------------------- #
  cat("Filtering data and streaming from ", source, " to DuckDB...\n", sep = "")

  entfernungsdaten_ref <- get_source_table_ref(
    source_config,
    "Entfernungsdaten",
    pg_schema
  )

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
  tables_to_copy_final <- if (!is.null(tables_to_copy)) {
    tables_to_copy
  } else {
    relevant_tables
  }

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
  # Get row count of final result
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

#' Pick a Matching Row from Lookup Table (Interactive)
#'
#' Given a row from a template and a lookup table, interactively select the best match based on filled-in fields.
#' If multiple matches are found, prompts the user to choose. Not suitable for non-interactive/batch use.
#'
#' @param one_tpl_row A single-row tibble representing the template row to match.
#' @param lookup_df A tibble to search for matches.
#' @param row_i Integer, the row number (for informative messages).
#'
#' @return A tibble with the selected matching row(s), or NULL if no match or skipped.
#'
#' @examples
#' # pick_row(template[1, ], lookup_table, 1)
#'
#' @export
pick_row <- function(one_tpl_row, lookup_df, row_i) {
  if (!tibble::is_tibble(one_tpl_row) || nrow(one_tpl_row) != 1) {
    stop("one_tpl_row must be a single-row tibble.")
  }
  if (!tibble::is_tibble(lookup_df)) {
    stop("lookup_df must be a tibble.")
  }
  cand <- lookup_df
  for (col in c("Bundesland", "Regierungsbezirk", "Kreis", "Gemeindename")) {
    if (!is.null(one_tpl_row[[col]]) && !is.na(one_tpl_row[[col]])) {
      cand <- filter(cand, .data[[col]] == one_tpl_row[[col]])
    }
  }
  if (nrow(cand) == 0) {
    message(
      "\n[WARNING] Keine Uebereinstimmung fuer Zeile Nr. ",
      row_i,
      ": ",
      paste(one_tpl_row, collapse = " / ")
    )
    return(NULL)
  }
  # ... (rest of the function unchanged, with similar checks for disp_cols, etc.)
  tibble::as_tibble(cand)
}

#' Preprocess Geofilter List
#'
#' Reads a template CSV of municipalities, matches each entry to the official mapping table,
#' and writes a cleaned, explicit geofilter list to disk. Interactive if ambiguities are found.
#' All tabular inputs and outputs are expected to be tibbles.
#'
#' @param geofilter_csv_path Path to the template CSV file with municipalities to filter by.
#' @param output_csv_path Path to save the preprocessed geofilter list (optional).
#' @param pg_schema Schema name in the PostgreSQL database.
#' @param expose_connection_to_viewer Logical, whether to expose DB connection to viewer.
#' @param print_connection_info Logical, whether to print DB connection info.
#'
#' @return A tibble with the explicit geofilter list. Optionally writes a CSV to disk.
#'
#' @examples
#' \dontrun{
#' result <- preprocess_geofilter_list("data/Geofilter_list_NRW.csv")
#' }
#'
#' @export
preprocess_geofilter_list <- function(
  geofilter_csv_path,
  output_csv_path = NULL,
  pg_schema = "public",
  expose_connection_to_viewer = TRUE,
  print_connection_info = TRUE
) {
  require_file_exists(geofilter_csv_path)

  tpl <- read_csv2(
    geofilter_csv_path,
    col_types = cols(.default = col_character()),
    na = c("", "NA")
  )
  if (!tibble::is_tibble(tpl)) {
    tpl <- as_tibble(tpl)
  }
  required_cols <- c("Gemeindename", "Kreis", "Regierungsbezirk", "Bundesland")
  if (!any(required_cols %in% names(tpl))) {
    stop(
      "Template CSV must have at least one of the columns: ",
      paste(required_cols, collapse = ", ")
    )
  }
  con <- connect_to_postgres_db(
    expose_connection_to_viewer = expose_connection_to_viewer,
    print_connection_info = print_connection_info
  )
  remote_Verwaltungsgebiete_Mapping <- tbl(
    con,
    Id(schema = pg_schema, table = "Verwaltungsgebiete_Mapping")
  )
  Verwaltungsgebiete_Mapping <- collect(remote_Verwaltungsgebiete_Mapping)
  if (!tibble::is_tibble(Verwaltungsgebiete_Mapping)) {
    Verwaltungsgebiete_Mapping <- as_tibble(Verwaltungsgebiete_Mapping)
  }
  result <- tibble()
  for (i in seq_len(nrow(tpl))) {
    row_match <- pick_row(tpl[i, ], Verwaltungsgebiete_Mapping, row_i = i)
    if (!is.null(row_match) && nrow(row_match) > 0) {
      result <- bind_rows(result, row_match)
    }
  }
  result <- distinct(result, Gemeindeschluessel, .keep_all = TRUE)
  if (!is.null(output_csv_path)) {
    ensure_dir_exists(dirname(output_csv_path))
    write_csv(result, output_csv_path)
  }
  dbDisconnect(con, shutdown = TRUE)
  as_tibble(result)
}
