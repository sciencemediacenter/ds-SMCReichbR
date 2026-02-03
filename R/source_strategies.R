#' Create PostgreSQL source configuration
#'
#' Creates a configuration object for using PostgreSQL as the data source.
#' Settings are applied via SET commands when connecting. If a setting is NULL,
#' it will not be applied (database default will be used).
#'
#' @param work_mem Memory for sort/hash operations (default: "512MB")
#' @param maintenance_work_mem Memory for maintenance ops (default: "512MB")
#' @param effective_cache_size Planner's cache size assumption (default: "4GB")
#' @param random_page_cost Cost estimate for random page fetch (default: 1.1)
#' @param effective_io_concurrency Concurrent I/O operations (default: 200)
#' @param max_parallel_workers_per_gather Parallel workers per query (default: 4)
#' @param enable_hashjoin Enable/disable hash joins (default: NULL = database default)
#' @param enable_mergejoin Enable/disable merge joins (default: NULL = database default)
#' @param enable_nestloop Enable/disable nested loop joins (default: NULL = database default)
#' @param min_parallel_table_scan_size Minimum table size for parallel scan (default: NULL = database default)
#' @param parallel_setup_cost Cost of starting parallel workers (default: NULL = database default)
#' @param parallel_tuple_cost Cost per tuple for parallel workers (default: NULL = database default)
#' @param seq_page_cost Cost of sequential page fetch (default: NULL = database default)
#' @return A pg_source object
#' @export
#' @seealso \code{\link{Geofilter_Funktion}} and \code{\link{Klinikfilter_Funktion}}
#'   apply additional filter-specific optimizations on top of these defaults.
#' @examples
#' \dontrun{
#' # Use defaults
#' src <- postgres_source()
#'
#' # Override specific settings
#' src <- postgres_source(work_mem = "1GB", max_parallel_workers_per_gather = 8)
#'
#' # Configure for nested loop joins (used by Geofilter)
#' src <- postgres_source(
#'   enable_hashjoin = "off",
#'   enable_mergejoin = "off",
#'   enable_nestloop = "on"
#' )
#'
#' # Use database defaults for all settings (no optimizations)
#' src <- postgres_source(
#'   work_mem = NULL,
#'   maintenance_work_mem = NULL,
#'   effective_cache_size = NULL,
#'   random_page_cost = NULL,
#'   effective_io_concurrency = NULL,
#'   max_parallel_workers_per_gather = NULL
#' )
#' }
postgres_source <- function(
  work_mem = "512MB",
  maintenance_work_mem = "512MB",
  effective_cache_size = "4GB",
  random_page_cost = 1.1,
  effective_io_concurrency = 200,
  max_parallel_workers_per_gather = 4,
  enable_hashjoin = NULL,
  enable_mergejoin = NULL,
  enable_nestloop = NULL,
  min_parallel_table_scan_size = NULL,
  parallel_setup_cost = NULL,
  parallel_tuple_cost = NULL,
  seq_page_cost = NULL
) {
  settings <- list(
    work_mem = work_mem,
    maintenance_work_mem = maintenance_work_mem,
    effective_cache_size = effective_cache_size,
    random_page_cost = random_page_cost,
    effective_io_concurrency = effective_io_concurrency,
    max_parallel_workers_per_gather = max_parallel_workers_per_gather,
    enable_hashjoin = enable_hashjoin,
    enable_mergejoin = enable_mergejoin,
    enable_nestloop = enable_nestloop,
    min_parallel_table_scan_size = min_parallel_table_scan_size,
    parallel_setup_cost = parallel_setup_cost,
    parallel_tuple_cost = parallel_tuple_cost,
    seq_page_cost = seq_page_cost
  )

  structure(
    list(
      type = "postgres",
      settings = settings
    ),
    class = "pg_source"
  )
}

#' Create DuckDB source configuration
#'
#' Creates a configuration object for using an existing DuckDB database
#' (e.g., from Komplettexport) as the data source.
#'
#' @param path Path to existing DuckDB database file
#' @param threads Number of threads for DuckDB (default: NULL = auto)
#' @param memory_limit Memory limit for DuckDB (default: NULL = auto)
#' @return A duckdb_source object
#' @export
#' @examples
#' \dontrun{
#' # Use DuckDB Komplettexport as source
#' src <- duckdb_source(path = "data/nobackup/Komplettexport.duckdb")
#'
#' # With custom settings
#' src <- duckdb_source(
#'   path = "data/nobackup/Komplettexport.duckdb",
#'   threads = 8,
#'   memory_limit = "16GB"
#' )
#' }
duckdb_source <- function(
  path,
  threads = NULL,
  memory_limit = NULL
) {
  if (missing(path) || is.null(path)) {
    stop("path is required for duckdb_source")
  }
  if (!file.exists(path)) {
    stop("DuckDB source file does not exist: ", path)
  }

  settings <- list(
    threads = threads,
    memory_limit = memory_limit
  )

  structure(
    list(
      type = "duckdb",
      path = path,
      settings = settings
    ),
    class = "duckdb_source"
  )
}

#' Apply source-specific optimizations
#'
#' Applies database-specific optimization settings via SET commands.
#' Only non-NULL settings are applied.
#'
#' @param source Source configuration (pg_source or duckdb_source)
#' @param con Database connection to apply settings to
#' @keywords internal
apply_source_optimizations <- function(source, con) {
  for (setting_name in names(source$settings)) {
    value <- source$settings[[setting_name]]
    if (!is.null(value)) {
      if (is.character(value)) {
        dbExecute(con, glue("SET {setting_name} = '{value}'"))
      } else {
        dbExecute(con, glue("SET {setting_name} = {value}"))
      }
    }
  }
}

#' Connect to data source
#'
#' Establishes connection to the data source and attaches it to the target DuckDB.
#'
#' @param source Source configuration (pg_source or duckdb_source)
#' @param con_duck Target DuckDB connection for filtered results
#' @param expose_connection_to_viewer Whether to expose to RStudio viewer
#' @param print_connection_info Whether to print connection info
#' @return List with connection(s) and metadata
#' @keywords internal
connect_to_source <- function(
  source,
  con_duck,
  expose_connection_to_viewer = TRUE,
  print_connection_info = FALSE
) {
  if (inherits(source, "pg_source")) {
    con_pg <- connect_to_postgres_db(
      expose_connection_to_viewer = expose_connection_to_viewer,
      print_connection_info = print_connection_info
    )
    if (is.null(con_pg) || !dbIsValid(con_pg)) {
      stop("Failed to connect to PostgreSQL database.")
    }
    attach_duckdb_to_postgres(con = con_duck)
    list(con = con_pg, con_duck = con_duck, type = "postgres")
  } else if (inherits(source, "duckdb_source")) {
    # Attach source DuckDB as "source_db" in read-only mode
    attach_sql <- glue_sql(
      "ATTACH {source$path} AS source_db (READ_ONLY)",
      .con = con_duck
    )
    dbExecute(con_duck, attach_sql)
    list(con = NULL, con_duck = con_duck, type = "duckdb")
  } else {
    stop("Unknown source type. Use postgres_source() or duckdb_source().")
  }
}

#' Get table reference for source
#'
#' Returns the fully qualified table reference for the given source type.
#'
#' @param source Source configuration (pg_source or duckdb_source)
#' @param table_name Table name
#' @param schema Schema name (for PostgreSQL, default: "public")
#' @return SQL table reference string
#' @keywords internal
get_source_table_ref <- function(source, table_name, schema = "public") {
  if (inherits(source, "pg_source")) {
    glue("pg_src.{schema}.{table_name}")
  } else if (inherits(source, "duckdb_source")) {
    glue("source_db.{table_name}")
  } else {
    stop("Unknown source type.")
  }
}

#' Copy supporting tables from source to target DuckDB
#'
#' Copies tables from the data source to the target DuckDB database.
#' For PostgreSQL sources, converts WKB_BLOB to GEOMETRY.
#' For DuckDB sources, geometry is already in correct format.
#'
#' @param source Source configuration (pg_source or duckdb_source)
#' @param con_source Source connection (PostgreSQL connection or NULL for DuckDB)
#' @param con_duck Target DuckDB connection
#' @param pg_schema Schema name (for PostgreSQL)
#' @param tables Vector of table names to copy
#' @param geometry_tables Vector of geometry table names
#' @param geometry_column Name of geometry column
#' @keywords internal
copy_tables_from_source <- function(
  source,
  con_source,
  con_duck,
  pg_schema = "public",
  tables,
  geometry_tables,
  geometry_column
) {
  if (inherits(source, "pg_source")) {
    # Use existing function for PostgreSQL with WKB conversion
    copy_all_tables_from_postgres_to_duckdb(
      con_duck = con_duck,
      con_pg = con_source,
      pg_schema = pg_schema,
      relevant_tables = tables,
      geometry_tables = geometry_tables,
      geometry_column = geometry_column
    )
  } else if (inherits(source, "duckdb_source")) {
    # DuckDB source: geometry already converted, just copy tables
    for (table_name in tables) {
      if (!dbExistsTable(con_duck, table_name)) {
        sql <- glue_sql(
          "CREATE TABLE IF NOT EXISTS {`table_name`} AS
           SELECT * FROM source_db.{`table_name`}",
          .con = con_duck
        )
        dbExecute(con_duck, sql)
      }
    }
  } else {
    stop("Unknown source type.")
  }
}

#' Disconnect from source
#'
#' Cleans up connections to the data source.
#'
#' @param source Source configuration (pg_source or duckdb_source)
#' @param connections List of connections returned by connect_to_source
#' @param shutdown Whether to shutdown connections (default: TRUE)
#' @keywords internal
disconnect_source <- function(source, connections, shutdown = TRUE) {
  if (inherits(source, "pg_source")) {
    detach_duckdb_from_postgres(connections$con_duck)
    if (!is.null(connections$con) && dbIsValid(connections$con)) {
      dbDisconnect(connections$con)
    }
  } else if (inherits(source, "duckdb_source")) {
    dbExecute(connections$con_duck, "DETACH source_db")
  }
}

#' Create source configuration from parameters
#'
#' Helper function to create appropriate source configuration based on
#' source type string and options. Merges user-provided options with defaults.
#'
#' @param source Source type: "postgres" or "duckdb"
#' @param source_duckdb_path Path to DuckDB source (required if source = "duckdb")
#' @param pg_options List of PostgreSQL options to override defaults
#' @param duckdb_options List of DuckDB options to override defaults
#' @return A source configuration object (pg_source or duckdb_source)
#' @keywords internal
create_source_config <- function(
  source = "postgres",
  source_duckdb_path = NULL,
  pg_options = list(),
  duckdb_options = list()
) {
  if (source == "postgres") {
    # Get defaults
    defaults <- postgres_source()

    # Merge user options with defaults for settings
    merged_settings <- utils::modifyList(defaults$settings, pg_options)

    # Create source with merged options
    do.call(postgres_source, merged_settings)

  } else if (source == "duckdb") {
    if (is.null(source_duckdb_path)) {
      stop("source_duckdb_path is required when source = 'duckdb'")
    }

    # Merge user options with path
    duckdb_args <- utils::modifyList(
      list(path = source_duckdb_path),
      duckdb_options
    )

    do.call(duckdb_source, duckdb_args)

  } else {
    stop("source must be 'postgres' or 'duckdb', got: ", source)
  }
}
