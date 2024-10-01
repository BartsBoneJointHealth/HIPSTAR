readData <- function() {
  files <- list.files(here::here("data"), full.names = TRUE)
  filesCsv <- files[tools::file_ext(files) == "csv"]
  data <- readFiles(filesCsv)
  filesZip <- files[tools::file_ext(files) == "zip"]
  tmpdir <- tempdir()
  for (fileZipe in filesZip) {
    unzip(fileZipe, exdir = tmpdir)
    files <- list.files(here::here("data"), full.names = TRUE)
    filesCsv <- files[tools::file_ext(files) == "csv"]
    data <- readFiles(filesCsv)
  }

  data$counts <- data$results |>
    visOmopResults::filterSettings(
      result_type %in% c("summarise_characteristics", "summarised_characteristics")
    ) |>
    dplyr::filter(variable_name %in% c("Number subjects", "Number records"))
    

  data$lsc <- data$results |>
    visOmopResults::filterSettings(
      result_type %in% c("summarise_large_scale_characteristics", "summarised_large_scale_characteristics")
    )

  data$cdm_summary <- data$results |>
    visOmopResults::filterSettings(result_type == "cdm_snapshot")

  data$chars <- data$results |>
    visOmopResults::filterSettings(
      result_type %in% c("summarise_characteristics", "summarised_characteristics")
    ) |>
    dplyr::filter(!grepl("year", strata_name)) |>
    updateResultType("summarised_characteristics")
  
  data$results <- NULL
  
  # data <- list()
  # data$lsc <- readr::read_csv(here::here("data", "lsc.csv"), col_types = c(result_id = "i", .default = "c")) |> 
  #   omopgenerics::newSummarisedResult()
  # data$counts <- readr::read_csv(here::here("data", "cohort_count.csv"), col_types = c(result_id = "i", .default = "c")) |> 
  #   omopgenerics::newSummarisedResult()
  # data$cdm_summary <- readr::read_csv(here::here("data", "cdm_summary.csv"), col_types = c(result_id = "i", .default = "c")) |> 
  #   omopgenerics::newSummarisedResult()
  # data$chars <- readr::read_csv(here::here("data", "chars.csv"), col_types = c(result_id = "i", .default = "c")) |> 
  #   omopgenerics::newSummarisedResult()
  
  return(data)
}
readFiles <- function(files, data = list()) {
  for (file in files) {
    pat <- getPat(file)
    x <-  readr::read_csv(file, col_types = c(result_id = "i", .default = "c")) |>
      dplyr::distinct() |>
      dplyr::filter(.data$variable_level != "#NAME?" | is.na(.data$variable_level)) |>
      dplyr::mutate(estimate_value = dplyr::if_else(
        estimate_type == "numeric", 
        as.character(round(as.numeric(.data$estimate_value), 4)),
        .data$estimate_value
      )) |>
      dplyr::distinct() |>
      omopgenerics::newSummarisedResult()
    data$results <- omopgenerics::bind(data$result, x)
  }
  return(data)
}

getPat <- function(f) {
  fname <- basename(f)
  patterns <- "results"
  if (startsWith(fname, patterns)) {
    id <- patterns 
  } else {
    id <- NULL
    cli::cli_inform(c("!" = "file: {f} is ignored as does not start with results."))
  }
  return(id)
}
addData <- function(dat, id, x) {
  if (id %in% names(dat)) {
    dat[[id]] <- dat[[id]] |> dplyr::union_all(x)
  } else {
    dat[[id]] <- x
  }
  return(dat)
}
selectors <- function(data, prefix, columns, multiple = TRUE, default = list()) {
  def <- function(col) {
    if (col %in% names(default)) {
      x <- default[[col]]
    } else {
      x <- choic(col)
      if (!multiple) {
        x <- dplyr::first(x)
      }
    }
    return(x)
  }
  choic <- function(col) {
    data[[col]] |> unique() |> sort()
  }
  purrr::map(columns, ~ shinyWidgets::pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choic(.),
    selected = def(.),
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE
  ))
}
filterData <- function(data, prefix, input) {
  cols <- colnames(data)
  cols <- cols[paste0(prefix, "_", cols) %in% names(input)]
  for (col in cols) {
    data <- data |>
      dplyr::filter(.data[[col]] %in% .env$input[[paste0(prefix, "_", col)]])
  }
  validate(need(nrow(data) > 0, "No results for selected inputs"))
  return(data)
}
uniteCols <- function(x, cols, nm) {
  if (length(cols) == 0) {
    x <- x |> dplyr::mutate(!!nm := "")
  } else if (length(cols) == 1) {
    x <- x |> dplyr::mutate(!!nm := .data[[cols]])
  } else {
    x <- x |> tidyr::unite(col = !!nm, dplyr::all_of(cols), sep = "; ", remove = FALSE)
  }
  return(x)
}
updateResultType <- function(x, rt) {
  set <- omopgenerics::settings(x) |>
    dplyr::mutate("result_type" = .env$rt)
  omopgenerics::newSummarisedResult(x, set)
}

