#' @keywords internal
#' @importFrom dplyr mutate left_join summarise filter distinct bind_rows rowwise as_tibble select pull inner_join tbl collect tibble all_of
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map list_rbind
#' @importFrom rlang syms
#' @importFrom readr read_csv read_csv2 write_csv cols col_character
#' @importFrom DBI dbIsValid dbExecute dbWriteTable dbGetQuery dbAppendTable dbListTables dbDisconnect dbConnect dbExistsTable dbGetInfo Id
#' @importFrom connections connection_view
#' @importFrom RPostgres Postgres
#' @importFrom duckdb duckdb
#' @importFrom glue glue glue_sql
#' @importFrom htmltools HTML
#' @importFrom stats weighted.mean na.omit
"_PACKAGE"
