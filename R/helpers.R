utils::globalVariables(c(
  ".data",
  "Gemeindeschluessel",
  "Gitterzellen_ID",
  "Id",
  "connection_view"
))

#' Connect to a Postgres database
#'
#' Reads connection parameters from environment variables.
#' @param expose_connection_to_viewer Logical, show in connections pane.
#' @param sslmode SSL mode for connection.
#' @param print_connection_info Logical, print connection info.
#' @return A DBI connection object.
#' @export
connect_to_postgres_db <- function(
  expose_connection_to_viewer = FALSE,
  sslmode = "prefer",
  print_connection_info = FALSE
) {
  con <- tryCatch(
    dbConnect(
      drv = Postgres(),
      dbname = Sys.getenv("DB"),
      host = Sys.getenv("HOST"),
      port = as.integer(Sys.getenv("PORT")),
      user = Sys.getenv("USER"),
      password = Sys.getenv("PASSWORD"),
      sslmode = sslmode
    ),
    error = function(e) stop("Could not connect to Postgres: ", e$message)
  )
  if (expose_connection_to_viewer) {
    connection_view(con)
  }
  if (print_connection_info) {
    print(dbGetInfo(con))
  }
  con
}

#' List tables and their sizes in Postgres
#' @param con A DBI connection.
#' @return A tibble with table names and sizes.
#' @export
list_tables_and_sizes <- function(con) {
  as_tibble(dbGetQuery(
    con,
    "SELECT
      schemaname || '.' || relname AS table_full_name,
      pg_size_pretty(pg_table_size(relid)) AS data_size,
      pg_size_pretty(pg_indexes_size(relid)) AS index_size,
      pg_size_pretty(pg_total_relation_size(relid) - pg_table_size(relid) - pg_indexes_size(relid)) AS toast_size,
      pg_size_pretty(pg_total_relation_size(relid)) AS total_size
    FROM pg_catalog.pg_statio_user_tables
    ORDER BY pg_total_relation_size(relid) DESC"
  ))
}

#' List primary keys in Postgres
#' @param con A DBI connection.
#' @return A tibble with schema, table, and column names.
#' @export
check_primary_keys <- function(con) {
  as_tibble(dbGetQuery(
    con,
    "SELECT
      tc.table_schema,
      tc.table_name,
      kcu.column_name
    FROM information_schema.table_constraints AS tc
    JOIN information_schema.key_column_usage AS kcu
      ON tc.constraint_name = kcu.constraint_name
     AND tc.table_schema  = kcu.table_schema
    WHERE tc.constraint_type = 'PRIMARY KEY'
      AND tc.table_schema NOT IN ('pg_catalog','information_schema')
    ORDER BY tc.table_schema, tc.table_name, kcu.ordinal_position"
  ))
}

#' List indexes in Postgres
#' @param con A DBI connection.
#' @return A tibble with schema, table, index name, and definition.
#' @export
check_indexes <- function(con) {
  as_tibble(dbGetQuery(
    con,
    "SELECT
      schemaname AS table_schema,
      tablename AS table_name,
      indexname AS index_name,
      indexdef AS index_definition
    FROM pg_indexes
    WHERE schemaname NOT IN ('pg_catalog','information_schema')
    ORDER BY schemaname, tablename, indexname"
  ))
}

#' List temporary tables in Postgres
#' @param con A DBI connection.
#' @return A tibble with schema, table, column, and type.
#' @export
list_temp_tables <- function(con) {
  as_tibble(dbGetQuery(
    con,
    "SELECT 
      t.table_schema,
      t.table_name,
      c.column_name,
      c.data_type
    FROM information_schema.tables t
    JOIN information_schema.columns c 
      ON t.table_schema = c.table_schema 
      AND t.table_name = c.table_name
    WHERE t.table_schema LIKE 'pg_temp%'
    ORDER BY t.table_name, c.ordinal_position"
  ))
}

#' Add a primary key to a table
#' @param con A DBI connection.
#' @param table_name Table name.
#' @param columns Character vector of column names.
#' @export
set_primary_key <- function(con, table_name, columns) {
  sql <- glue_sql(
    "ALTER TABLE {`table_name`} ADD PRIMARY KEY ({glue_collapse(columns, sep = ', ')})",
    .con = con
  )
  invisible(dbExecute(con, sql))
}

#' Add an index to a table
#' @param con A DBI connection.
#' @param pg_schema Schema name.
#' @param tbl Table name.
#' @param col Column name.
#' @param idx_name Index name.
#' @export
set_index <- function(con, pg_schema, tbl, col, idx_name) {
  sql <- glue_sql(
    "CREATE INDEX IF NOT EXISTS {`idx_name`} ON {`pg_schema`}.{`tbl`} ({`col`})",
    .con = con
  )
  invisible(dbExecute(con, sql))
}

#' Remove a table from Postgres
#' @param con A DBI connection.
#' @param pg_schema Schema name.
#' @param table_name Table name.
#' @export
remove_table <- function(con, pg_schema, table_name) {
  sql <- glue_sql(
    "DROP TABLE IF EXISTS {`pg_schema`}.{`table_name`} CASCADE",
    .con = con
  )
  invisible(dbExecute(con, sql))
}

#' Remove an index from Postgres
#' @param con A DBI connection.
#' @param pg_schema Schema name.
#' @param index_name Index name.
#' @export
remove_index <- function(con, pg_schema, index_name) {
  sql <- glue_sql(
    "DROP INDEX IF EXISTS {`pg_schema`}.{`index_name`}",
    .con = con
  )
  invisible(dbExecute(con, sql))
}

#' Check if a primary key exists
#' @param con A DBI connection.
#' @param pg_schema Schema name.
#' @param table_name Table name.
#' @param column_name Column name.
#' @param return_constraints_table Logical, return full table if TRUE.
#' @return Logical or tibble.
#' @export
check_primary_key_exists <- function(
  con,
  pg_schema,
  table_name,
  column_name,
  return_constraints_table = FALSE
) {
  sql <- glue_sql(
    "SELECT tc.constraint_name
     FROM information_schema.table_constraints AS tc
     JOIN information_schema.key_column_usage AS kcu
       ON tc.constraint_name = kcu.constraint_name
      AND tc.table_schema  = kcu.table_schema
      AND tc.table_name    = kcu.table_name
     WHERE tc.constraint_type = 'PRIMARY KEY'
       AND tc.table_schema    = {pg_schema}
       AND tc.table_name      = {table_name}
       AND kcu.column_name    = {column_name}",
    .con = con
  )
  constraints_table <- as_tibble(dbGetQuery(con, sql))
  if (return_constraints_table) {
    return(constraints_table)
  }
  nrow(constraints_table) > 0
}

#' Connect to a DuckDB database
#'
#' Creates a connection to a DuckDB database file and optionally loads
#' the spatial and parquet extensions.
#'
#' @param db_path Path to DuckDB file.
#' @param expose_connection_to_viewer Logical, show in connections pane.
#' @param print_connection_info Logical, print connection info.
#' @param load_spatial Logical, install and load the spatial extension (default: TRUE).
#' @param load_parquet Logical, install and load the parquet extension (default: TRUE).
#' @return A DBI connection object.
#' @export
connect_to_duckdb_db <- function(
  db_path,
  expose_connection_to_viewer = FALSE,
  print_connection_info = FALSE,
  load_spatial = TRUE,
  load_parquet = TRUE
) {
  con <- tryCatch(
    dbConnect(duckdb(), dbdir = db_path),
    error = function(e) stop("Could not connect to DuckDB: ", e$message)
  )
  if (load_spatial) {
    dbExecute(con, "INSTALL spatial; LOAD spatial;")
  }
  if (load_parquet) {
    dbExecute(con, "INSTALL parquet; LOAD parquet;")
  }
  if (expose_connection_to_viewer) {
    connection_view(con)
  }
  if (print_connection_info) {
    print(dbGetInfo(con))
  }
  con
}

#' Attach DuckDB to Postgres for streaming reads
#' @param con A DuckDB connection.
#' @export
attach_duckdb_to_postgres <- function(con) {
  dbExecute(con, "INSTALL postgres_scanner; LOAD postgres_scanner;")
  attach_string <- glue(
    "dbname={Sys.getenv('DB')} host={Sys.getenv('HOST')} user={Sys.getenv('USER')} password={Sys.getenv('PASSWORD')} port={Sys.getenv('PORT')}"
  )
  attach_query <- glue(
    "ATTACH '{attach_string}' AS pg_src (TYPE postgres_scanner, READ_ONLY)"
  )
  invisible(dbExecute(con, attach_query))
}

#' Detach DuckDB from Postgres
#' @param con A DuckDB connection.
#' @export
detach_duckdb_from_postgres <- function(con) {
  invisible(dbExecute(con, "DETACH pg_src"))
}

#' Copy a table from Postgres to DuckDB
#' @param con_duck DuckDB connection.
#' @param con_pg Postgres connection.
#' @param pg_schema Schema name.
#' @param table_name Table name.
#' @param geometry_column Name of geometry column to convert (NULL for non-spatial tables).
#' @export
copy_table_from_postgres_to_duckdb <- function(
  con_duck,
  con_pg,
  pg_schema,
  table_name,
  geometry_column = NULL
) {
  if (!is.null(geometry_column)) {
    # For geometry tables, convert WKB_BLOB to proper GEOMETRY type
    # Get all columns except geometry
    cols_query <- glue_sql(
      "SELECT column_name FROM information_schema.columns 
       WHERE table_schema = {pg_schema} AND table_name = {table_name}
       AND column_name != {geometry_column}",
      .con = con_pg
    )
    other_cols <- dbGetQuery(con_pg, cols_query)$column_name

    # Quote column names using DBI (handles DB-specific quoting)
    quoted_cols <- vapply(
      other_cols,
      function(col) {
        as.character(DBI::dbQuoteIdentifier(con_duck, col))
      },
      character(1)
    )
    quoted_geom <- as.character(DBI::dbQuoteIdentifier(
      con_duck,
      geometry_column
    ))

    # Build SELECT with ST_GeomFromWKB for geometry column
    cols_sql <- paste(
      c(
        quoted_cols,
        paste0("ST_GeomFromWKB(", quoted_geom, ") AS ", quoted_geom)
      ),
      collapse = ", "
    )

    sql <- glue_sql(
      "CREATE TABLE IF NOT EXISTS {`table_name`} AS SELECT {DBI::SQL(cols_sql)} FROM pg_src.{`pg_schema`}.{`table_name`}",
      .con = con_duck
    )
  } else {
    # Regular table copy
    sql <- glue_sql(
      "CREATE TABLE IF NOT EXISTS {`table_name`} AS SELECT * FROM pg_src.{`pg_schema`}.{`table_name`}",
      .con = con_duck
    )
  }
  invisible(dbExecute(con_duck, sql))
}

#' Copy all relevant tables from Postgres to DuckDB
#' @param con_duck DuckDB connection.
#' @param con_pg Postgres connection.
#' @param pg_schema Schema name.
#' @param relevant_tables Character vector of table names.
#' @param geometry_tables Character vector of table names with geometry columns.
#' @param geometry_column Name of the geometry column in geometry tables.
#' @export
copy_all_tables_from_postgres_to_duckdb <- function(
  con_duck,
  con_pg,
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
  geometry_column = "geometry"
) {
  if (length(relevant_tables) == 1 && relevant_tables == "all") {
    relevant_tables <- dbListTables(con_pg, schema = pg_schema)
  }
  for (table_name in relevant_tables) {
    if (!dbExistsTable(con_duck, table_name, schema = pg_schema)) {
      # Determine if this table has geometry
      geom_col <- if (table_name %in% geometry_tables) geometry_column else NULL
      copy_table_from_postgres_to_duckdb(
        con_duck,
        con_pg,
        pg_schema,
        table_name,
        geom_col
      )
    }
  }
}

#' Export a DuckDB table to Parquet
#' @param con DuckDB connection.
#' @param table_name Table name.
#' @param output_path Output file path.
#' @param compression Compression algorithm.
#' @export
export_single_duckdb_table_to_parquet <- function(
  con,
  table_name,
  output_path,
  compression = "SNAPPY"
) {
  sql <- glue_sql(
    "COPY (SELECT * FROM {`table_name`}) TO {`output_path`} (FORMAT 'PARQUET', COMPRESSION {`compression`})",
    .con = con
  )
  invisible(dbExecute(con, sql))
}

#' Export a DuckDB table with geometry to GeoParquet
#'
#' Exports a table containing a GEOMETRY column to GeoParquet format.
#' The DuckDB spatial extension automatically writes proper GeoParquet
#' metadata when exporting tables with GEOMETRY columns.
#'
#' @param con DuckDB connection.
#' @param table_name Table name.
#' @param output_path Output file path.
#' @param compression Compression algorithm (default: "SNAPPY").
#'
#' @return Invisible NULL. Called for side effect of writing file.
#'
#' @details
#' This function assumes the table already has a proper GEOMETRY column
#' (not WKB_BLOB). If your table has WKB_BLOB, use ST_GeomFromWKB() to
#' convert it first, or ensure geometry conversion happened during
#' the copy from PostgreSQL.
#'
#' The DuckDB spatial extension automatically detects GEOMETRY columns
#' and writes proper GeoParquet metadata (CRS, bbox, geometry types) to
#' the Parquet file footer.
#'
#' @examples
#' \dontrun{
#' export_geometry_table_to_geoparquet(
#'   con,
#'   "Gemeindegrenzen_Polygone",
#'   "output/boundaries.parquet"
#' )
#' }
#'
#' @export
export_geometry_table_to_geoparquet <- function(
  con,
  table_name,
  output_path,
  compression = "SNAPPY"
) {
  sql <- glue_sql(
    "COPY (SELECT * FROM {`table_name`}) TO {`output_path`} (FORMAT 'PARQUET', COMPRESSION {`compression`})",
    .con = con
  )
  invisible(dbExecute(con, sql))
}

#' Export a large DuckDB table to Parquet in chunks
#' @param con DuckDB connection.
#' @param table_name Table name.
#' @param output_path Output file path.
#' @param compression Compression algorithm.
#' @param chunk_size Number of rows per chunk.
#' @export
export_large_table_to_parquet <- function(
  con,
  table_name,
  output_path,
  compression = "SNAPPY",
  chunk_size = 1e8
) {
  total_rows <- dbGetQuery(
    con,
    glue_sql("SELECT COUNT(*) FROM {`table_name`}", .con = con)
  )[[1]]
  num_chunks <- ceiling(total_rows / chunk_size)
  for (i in seq_len(num_chunks)) {
    offset <- (i - 1) * chunk_size
    output_path_chunks <- file.path(
      dirname(output_path),
      glue("{table_name}_chunks"),
      basename(output_path)
    )
    ensure_dir_exists(dirname(output_path_chunks))
    chunk_file <- paste0(output_path_chunks, "_chunk_", i, ".parquet")
    sql <- glue_sql(
      "COPY (SELECT * FROM {`table_name`} LIMIT {chunk_size} OFFSET {offset}) 
       TO {`chunk_file`} (FORMAT 'PARQUET', COMPRESSION {`compression`})",
      .con = con
    )
    invisible(dbExecute(con, sql))
  }
}

#' Export all DuckDB tables to Parquet files
#' @param con DuckDB connection.
#' @param output_dir Output directory.
#' @param large_tables_list Vector of large table names.
#' @param geometry_tables_list Vector of table names with geometry columns.
#' @param compression Compression algorithm.
#' @export
export_all_duckdb_tables_to_parquet <- function(
  con,
  output_dir,
  large_tables_list = c("Entfernungsdaten"),
  geometry_tables_list = c(
    "Gemeindegrenzen_Polygone",
    "Kreisgrenzen_Polygone",
    "Landesgrenzen_Polygone",
    "Regierungsbezirksgrenzen_Polygone",
    "Krankenhaus_Standortliste"
  ),
  compression = "SNAPPY"
) {
  ensure_dir_exists(output_dir)
  tables_list <- dbListTables(con)
  for (table_name in tables_list) {
    parquet_path <- file.path(output_dir, paste0(table_name, ".parquet"))
    if (table_name %in% geometry_tables_list) {
      # GeoParquet export for spatial tables
      export_geometry_table_to_geoparquet(
        con = con,
        table_name = table_name,
        output_path = parquet_path,
        compression = compression
      )
    } else if (table_name %in% large_tables_list) {
      # Chunked export for large tables
      export_large_table_to_parquet(
        con = con,
        table_name = table_name,
        output_path = parquet_path,
        compression = compression
      )
    } else {
      # Regular Parquet for everything else
      export_single_duckdb_table_to_parquet(
        con = con,
        table_name = table_name,
        output_path = parquet_path,
        compression = compression
      )
    }
  }
}

#' Import chunked Parquet files into DuckDB
#' @param con DuckDB connection.
#' @param output_dir Directory with Parquet files.
#' @param table_name Table name.
#' @param num_chunks Number of chunks (optional).
#' @export
import_chunked_parquet_to_duckdb <- function(
  con,
  output_dir,
  table_name,
  num_chunks = NULL
) {
  parquet_path <- file.path(output_dir, paste0(table_name, ".parquet"))
  # Look in the {table_name}_chunks subdirectory that export_large_table_to_parquet creates
  chunks_dir <- file.path(output_dir, paste0(table_name, "_chunks"))
  chunk_pattern <- paste0(table_name, ".parquet_chunk_", "[0-9]+", ".parquet")
  chunk_files <- list.files(
    path = chunks_dir,
    pattern = chunk_pattern,
    full.names = TRUE
  )
  if (!length(chunk_files)) {
    stop("No chunk files found.")
  }
  dbExecute(
    con,
    glue_sql(
      "CREATE TABLE {`table_name`} AS SELECT * FROM read_parquet({`chunk_files[1]`})",
      .con = con
    )
  )
  if (length(chunk_files) > 1) {
    for (i in 2:length(chunk_files)) {
      dbExecute(
        con,
        glue_sql(
          "INSERT INTO {`table_name`} SELECT * FROM read_parquet({`chunk_files[i]`})",
          .con = con
        )
      )
    }
  }
}

#' Read a GeoParquet file into an sf object
#'
#' Convenience wrapper for reading GeoParquet files exported by this package.
#' Requires the sf and arrow packages to be installed.
#'
#' @param path Path to the GeoParquet file.
#'
#' @return An sf object with geometry column.
#'
#' @examples
#' \dontrun{
#' boundaries <- read_geoparquet("output/Gemeindegrenzen_Polygone.parquet")
#' plot(boundaries["Gemeindeschluessel"])
#' }
#'
#' @export
read_geoparquet <- function(path) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package 'sf' is required to read GeoParquet files. Install with: install.packages('sf')"
    )
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "Package 'arrow' is required to read GeoParquet files. Install with: install.packages('arrow')"
    )
  }
  require_file_exists(path)

  # Read with arrow, then convert to sf
  df <- arrow::read_parquet(path)
  sf::st_as_sf(df)
}

#' Format columns for label creation
#' @param data A tibble.
#' @param columns Character vector of column names to format.
#' @param digits Number of digits to round numeric columns.
#' @param big.mark Character for thousands separator.
#' @param decimal.mark Character for decimal separator.
#' @return Tibble with new *_formatted columns.
#' @export
format_label_columns <- function(
  data,
  columns,
  digits = 1,
  big.mark = " ",
  decimal.mark = ","
) {
  for (col in columns) {
    formatted_col <- paste0(col, "_formatted")
    if (col %in% names(data)) {
      if (is.numeric(data[[col]])) {
        data[[formatted_col]] <- format(
          round(data[[col]], digits),
          big.mark = big.mark,
          decimal.mark = decimal.mark
        )
      } else {
        data[[formatted_col]] <- format(
          data[[col]],
          big.mark = big.mark,
          decimal.mark = decimal.mark
        )
      }
    }
  }
  data
}

#' Ensure a directory exists, create if not
#' @param dir_path Directory path
#' @export
ensure_dir_exists <- function(dir_path) {
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
}

#' Require a file exists, stop with error if not
#' @param file_path File path
#' @export
require_file_exists <- function(file_path) {
  if (!file.exists(file_path)) stop("File not found: ", file_path)
}
