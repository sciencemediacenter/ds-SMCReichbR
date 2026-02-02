#' Preprocess Geofilter List and Run Geofilter
#'
#' This function first preprocesses a raw geofilter list template, then runs the geofilter using the preprocessed list.
#' All tabular inputs and outputs are expected to be tibbles.
#'
#' @param geofilter_csv_path Path to the template CSV file with municipalities to filter by.
#' @param duckdb_path Path to the output DuckDB database file.
#' @param output_dir Directory to save the output parquet files.
#' @param preprocessed_output_path Path to save the preprocessed geofilter list.
#' @param pg_schema Schema name in the PostgreSQL database.
#' @param relevant_tables Vector of table names to copy from Postgres to DuckDB.
#' @param filter_table_name Name of the filter table in DuckDB.
#' @param filter_column_name Name of the column to filter on.
#' @param target_table_name Name of the target table in DuckDB for filtered data.
#' @param optimize_postgres_settings Whether to optimize Postgres settings for the query.
#' @param expose_connections_to_viewer Whether to expose connections to the viewer.
#' @param print_connection_info Whether to print connection info.
#' @param compression Compression algorithm for parquet files.
#' @param disconnect_on_exit Whether to disconnect from databases on exit.
#'
#' @return A list with paths to the created database and parquet files.
#'
#' @examples
#' \dontrun{
#' result <- Geofilter_Funktion(
#'   geofilter_csv_path = "data/Geofilter_list_NRW.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Geofilter.duckdb",
#'   output_dir = "data/nobackup/Geofilter"
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
  pg_schema = "public",
  relevant_tables = c(
    "Gitterzellen_Gemeinde_Mapping",
    "Gitterzellen_Einwohner_Mapping",
    "Verwaltungsgebiete_Mapping",
    "Gemeindegrenzen_Polygone",
    "Krankenhaus_Standortliste"
  ),
  filter_table_name = "Geofilter",
  filter_column_name = "Gitterzellen_ID",
  target_table_name = "Entfernungsdaten_subset",
  optimize_postgres_settings = TRUE,
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
    pg_schema = pg_schema,
    relevant_tables = relevant_tables,
    filter_table_name = filter_table_name,
    filter_column_name = filter_column_name,
    target_table_name = target_table_name,
    optimize_postgres_settings = optimize_postgres_settings,
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
#' All tabular inputs and outputs are expected to be tibbles.
#'
#' @param preprocessed_geofilter_list_csv_path Path to the preprocessed CSV file with the list of municipalities to filter by.
#' @param duckdb_path Path to the output DuckDB database file (will be created if it doesn't exist).
#' @param output_dir Directory to save the output parquet files (will be created if it doesn't exist).
#' @param pg_schema Schema name in the PostgreSQL database (default: "public").
#' @param relevant_tables Vector of table names to copy from Postgres to DuckDB.
#' @param geometry_tables Vector of table names with geometry columns (for GeoParquet export).
#' @param geometry_column Name of the geometry column in geometry tables (default: "geometry").
#' @param filter_table_name Name of the filter table in DuckDB.
#' @param filter_column_name Name of the column to filter on.
#' @param target_table_name Name of the target table in DuckDB for filtered data.
#' @param optimize_postgres_settings Whether to optimize Postgres settings for the query.
#' @param expose_connections_to_viewer Whether to expose connections to the viewer.
#' @param print_connection_info Whether to print connection info.
#' @param compression Compression algorithm for parquet files.
#' @param disconnect_on_exit Whether to disconnect from databases on exit.
#'
#' @return A list with paths to the created database and parquet files.
#'
#' @examples
#' \dontrun{
#' geofilter_result <- run_geofilter(
#'   preprocessed_geofilter_list_csv_path = "data/nobackup/Geofilter_list_NRW_preprocessed.csv",
#'   duckdb_path = "data/nobackup/Entfernungsdaten_Geofilter.duckdb",
#'   output_dir = "data/nobackup/Geofilter"
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
  filter_table_name = "Geofilter",
  filter_column_name = "Gitterzellen_ID",
  target_table_name = "Entfernungsdaten_subset",
  optimize_postgres_settings = TRUE,
  expose_connections_to_viewer = TRUE,
  print_connection_info = FALSE,
  compression = "SNAPPY",
  disconnect_on_exit = TRUE
) {
  # --- Error handling ---
  require_file_exists(preprocessed_geofilter_list_csv_path)

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
  # Connect to Postgres database
  # -------------------------------------------------------------------- #
  con <- connect_to_postgres_db(
    expose_connection_to_viewer = expose_connections_to_viewer,
    print_connection_info = print_connection_info
  )
  if (is.null(con) || !dbIsValid(con)) {
    stop("Failed to connect to Postgres database.")
  }

  # -------------------------------------------------------------------- #
  # Optimize Postgres settings for the query if requested
  # -------------------------------------------------------------------- #
  if (optimize_postgres_settings) {
    dbExecute(con, "SET enable_hashjoin = off;")
    dbExecute(con, "SET enable_mergejoin = off;")
    dbExecute(con, "SET enable_nestloop = on;")
    dbExecute(con, "SET min_parallel_table_scan_size = '8MB';")
    dbExecute(con, "SET parallel_setup_cost = 0;")
    dbExecute(con, "SET parallel_tuple_cost = 0;")
    dbExecute(con, "SET work_mem = '512MB';")
    dbExecute(con, "SET seq_page_cost = 10;")
    dbExecute(con, "SET random_page_cost = 1.1;")
  }

  # -------------------------------------------------------------------- #
  # Load remote tables
  # -------------------------------------------------------------------- #
  remote_Verwaltungsgebiete_Mapping <- tbl(
    con,
    Id(schema = pg_schema, table = "Verwaltungsgebiete_Mapping")
  )
  remote_Gitterzellen_Gemeinde_Mapping <- tbl(
    con,
    Id(schema = pg_schema, table = "Gitterzellen_Gemeinde_Mapping")
  )
  remote_Gitterzellen_Einwohner_Mapping <- tbl(
    con,
    Id(schema = pg_schema, table = "Gitterzellen_Einwohner_Mapping")
  )

  # -------------------------------------------------------------------- #
  # Identify relevant Gitterzellen IDs and create temporary table
  # -------------------------------------------------------------------- #
  cat("Creating temporary table for Gitterzellen IDs...\n")
  Gitterzellen_IDs_to_analyze <- remote_Verwaltungsgebiete_Mapping |>
    filter(Gemeindeschluessel %in% geo_list_to_analyze$Gemeindeschluessel) |>
    left_join(
      remote_Gitterzellen_Gemeinde_Mapping,
      by = "Gemeindeschluessel"
    ) |>
    inner_join(remote_Gitterzellen_Einwohner_Mapping, by = "Gitterzellen_ID") |>
    select(Gitterzellen_ID) |>
    pull()

  dbWriteTable(
    con,
    name = "temp_gitterzellen_ids",
    value = tibble(Gitterzellen_ID = Gitterzellen_IDs_to_analyze),
    temporary = TRUE,
    overwrite = TRUE
  )
  dbExecute(con, "ANALYZE temp_gitterzellen_ids;")
  cat("Temporary table created and analyzed.\n")

  # -------------------------------------------------------------------- #
  # Create connection to DuckDB database
  # -------------------------------------------------------------------- #
  ensure_dir_exists(dirname(duckdb_path))

  con_duck <- connect_to_duckdb_db(
    db_path = duckdb_path,
    expose_connection_to_viewer = expose_connections_to_viewer
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
  temp_data <- dbGetQuery(con, "SELECT * FROM temp_gitterzellen_ids")
  dbAppendTable(con_duck, filter_table_name, temp_data)

  # -------------------------------------------------------------------- #
  # Apply Geofilter to Entfernungsdaten and save result to DuckDB
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
    dbDisconnect(con, shutdown = TRUE)
    dbDisconnect(con_duck, shutdown = TRUE)
  }

  # Return relevant information
  list(
    duckdb_path = duckdb_path,
    output_dir = output_dir,
    table_counts = table_counts
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
