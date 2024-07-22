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
  types <- patterns <- c("cdm_summary", "patient_demographics", "large_Scale_characteristics", "results")
  for (ty in types) {
    if (ty %in% names(data)) {
      data[[ty]] <- data[[ty]] |> 
        dplyr::distinct() |>
        omopgenerics::newSummarisedResult()
    }
  }
  newNames <- names(data)
  newNames[newNames == "large_Scale_characteristics"] <- "lsc"
  names(data) <- newNames
  return(data)
}
readFiles <- function(files, data = list()) {
  for (file in files) {
    pat <- getPat(file)
    if (!is.null(pat)) {
      x <- readr::read_csv(file, col_types = c(result_id = "i", .default = "c"))
      if (pat == "results") {
        data <- data |> addData("results", x)
      } else {
        data <- data |> addData(pat, x)
      }
    }
  }
  return(data)
}
getPat <- function(f) {
  id <- NULL
  fname <- basename(f)
  patterns <- c("cdm_summary", "patient_demographics", "large_Scale_characteristics", "results")
  for (pat in patterns) {
    if (startsWith(fname, pat)) {
      id <-pat 
    }
  }
  if (is.null(id)) {
    patterns <- paste0(patterns, collapse = ", ")
    cli::cli_inform(c("!" = "file: {f} is ignored as does not start with either: {patterns}."))
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
