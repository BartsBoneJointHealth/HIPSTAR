library(shiny)
library(shinydashboard)

source(here::here("functions.R"), local = TRUE)

data <- readData()

data$lsc <- data$lsc |>
  visOmopResults::addSettings(columns = "table_name") |>
  dplyr::filter(estimate_name == "percentage") |>
  visOmopResults::splitAll() |>
  dplyr::rename(concept_name = "variable_name", window = "variable_level") |>
  dplyr::select(-"estimate_name", -"estimate_type", -"result_id") |>
  dplyr::mutate(estimate_value = as.numeric(estimate_value), concept_id = as.integer(concept_id)) |>
  dplyr::relocate(dplyr::starts_with("concept")) |>
  dplyr::relocate("table_name", .before = "estimate_value")

data$counts <- data$counts |>
  visOmopResults::splitGroup() |>
  visOmopResults::splitStrata()

data$chars <- data$chars |>
  visOmopResults::splitGroup() |>
  visOmopResults::splitStrata()

# theme -----
mytheme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#0c0e0c" 
  ),
  fresh::adminlte_sidebar(
    # width = "400px",
    dark_bg = "#143695",
    dark_hover_bg = "#143695",
    dark_color = "white"
  ), 
  fresh::adminlte_global(
    content_bg = "#eaebea" 
  ),
  fresh::adminlte_vars(
    border_color = "#143695",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#143695",
    active_link_hover_border_color = "#143695",
    link_hover_border_color = "#143695"
  )
)

# ui -----
ui <- dashboardPage(
  dashboardHeader(title = "HIPSTAR"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
      menuItem(
        text = "Database summary",
        tabName = "db_summary",
        menuSubItem(
          text = "Database details",
          tabName = "db_snapshot"
        )
      ),
      menuItem(
        text = "Study analyses",
        tabName = "study_analyses",
        menuSubItem(
          text = "Cohort counts",
          tabName = "study_counts"
        ),
        menuSubItem(
          text = "Cohort characteristics",
          tabName = "study_characteristics"
        ),
        menuSubItem(
          text = "Cohort large scale characteristics",
          tabName = "study_lsc"
        )
      )
    )
  ),
  
  ## body ----
  dashboardBody(
    fresh::use_theme(mytheme),
    tabItems(
      ### background  ----
      tabItem(
        tabName = "background",
        a(
          img(
            src = "logo.jpeg", 
            align = "right", width = "200px"
          ),
          href = "https://www.bonejointhealth.ac.uk",
          target = "_blank"
        ),
        h3("HIPSTAR"),
        h5("HIPSTAR study ..."),
        h5("To run this project follow this instructions:"),
        a("https://github.com/BartsBoneJointHealth/HIPSTAR", href = "https://github.com/BartsBoneJointHealth/HIPSTAR")
      ),
      ### cdm snapshot ----
      tabItem(
        tabName = "db_snapshot",
        h4("Information about the databases that participated in the study"),
        selectors(data$cdm_summary, "db_snapshot", c("cdm_name")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            gt::gt_output("db_snapshot_tidy") |> shinycssloaders::withSpinner()
          )
        )
      ),
      ### study counts ----
      tabItem(
        tabName = "study_counts",
        h4("Counts for the different cohorts in the study"),
        selectors(data$counts, "counts", c("cdm_name", "cohort_name", "age_group", "sex", "variable_name")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "tidy table",
            DT::DTOutput("counts_tidy") |> shinycssloaders::withSpinner()
          ),
          tabPanel(
            "gt table",
            gt::gt_output("counts_gt") |> shinycssloaders::withSpinner()
          ),
          tabPanel(
            "Plot",
            shinyWidgets::pickerInput("counts_x", label = "x variable", choices = c("cdm_name", "cohort_name", "age_group", "sex", "variable_name"), selected = "sex", multiple = TRUE, inline = TRUE),
            shinyWidgets::pickerInput("counts_facet", label = "Facet by", choices = c("cdm_name", "cohort_name", "age_group", "sex", "variable_name"), selected = character(), multiple = TRUE, inline = TRUE),
            plotly::plotlyOutput("counts_plot") |> shinycssloaders::withSpinner()
          )
        )
      ),
      ### study characteristics ----
      tabItem(
        tabName = "study_characteristics",
        h4("Characteristics of the different cohorts"),
        selectors(data$chars, "chars", c("cdm_name", "cohort_name", "age_group", "sex"), default = list(sex = "overall", age_group = "overall")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "gt table",
            gt::gt_output("chars_gt") |> shinycssloaders::withSpinner()
          )
        )
      ),
      ### study lsc ----
      tabItem(
        tabName = "study_lsc",
        h4("Large Scale Characteristics of the different cohorts"),
        selectors(data$lsc, "lsc", c("cdm_name", "cohort_name", "age_group", "sex", "window", "table_name")),
        shinyWidgets::pickerInput("lsc_compare", "Pivot", c("cdm_name", "cohort_name", "age_group", "sex", "window"), character(), multiple = T),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "data table",
            DT::DTOutput("lsc_data") |> shinycssloaders::withSpinner()
          )
        )
      )
      ### end ----
    )
  )
)


server <- function(input, output, session) {
  # cdm snapshot ----
  getSnapshot <- reactive({
    filterData(data$cdm_summary, "db_snapshot", input)
  })
  output$db_snapshot_tidy <- gt::render_gt({
    getSnapshot() |>
      dplyr::mutate(
        estimate_name = gsub("_", " ", .data$estimate_name),
        estimate_name = stringr::str_to_sentence(.data$estimate_name)) |>
      visOmopResults::visOmopTable(
        formatEstimateName = character(), 
        header = c("cdm_name"), 
        split = c("group", "strata", "additional"), 
        excludeColumns = c("variable_level", "result_id", "estimate_type")
      )
  })
  # study counts ----
  getCounts <- reactive({
    filterData(data$counts, "counts", input)
  })
  output$counts_tidy <- DT::renderDT({
    x <- getCounts() |>
      dplyr::select(-c("variable_level", "estimate_name", "estimate_type", "additional_name", "additional_level", "result_id")) |>
      dplyr::mutate(estimate_value = as.integer(estimate_value)) |>
      dplyr::rename(count = estimate_value)
    DT::datatable(x)
  })
  output$counts_gt <- gt::render_gt({
    getCounts() |>
      visOmopResults::formatEstimateValue() |>
      visOmopResults::formatEstimateName(estimateNameFormat = c(N = "<count>")) |>
      visOmopResults::splitAdditional() |>
      dplyr::select(-c("variable_level", "estimate_name", "estimate_type", "result_id")) |>
      tidyr::pivot_wider(names_from = "cdm_name", values_from = "estimate_value") |>
      gt::gt(groupname_col = "cohort_name")
  })
  output$counts_plot <- plotly::renderPlotly({
    x <- getCounts() |>
      dplyr::mutate(estimate_value = as.integer(estimate_value)) |>
      dplyr::select(-c("result_id", "variable_level", "estimate_name", "estimate_type", "additional_name", "additional_level"))
    xcol <- input$counts_x
    cols <- input$counts_facet
    noCols <- c("cdm_name", "cohort_name", "age_group", "sex", "variable_name")
    noCols <- noCols[!noCols %in% c(cols, xcol)]
    x <- uniteCols(x, noCols, "color")
    x <- uniteCols(x, cols, "facet")
    x <- uniteCols(x, xcol, "x")
    p <- ggplot2::ggplot(data = x, mapping = ggplot2::aes(x = x, y = estimate_value, fill = color)) +
      ggplot2::geom_col() +
      ggplot2::geom_line()
    if (length(cols) > 0) {
      p <- p + ggplot2::facet_wrap(facets = "facet") 
    }
    plotly::ggplotly(p)
  })
  # study characteristics ----
  getChars <- reactive({
    filterData(data$chars, "chars", input)
  })
  output$chars_gt <- gt::render_gt({
    getChars() |>
      visOmopResults::formatEstimateValue() |>
      visOmopResults::formatEstimateName(estimateNameFormat = c(
        `N (%)` = "<count> (<percentage>%)", 
        N = "<count>", 
        `Median [Q25 - Q75]` = "<median> [<q25> - <q75>]", 
        `Mean (SD)` = "<mean> (<sd>)", 
        Range = "<min> to <max>"
      ), keepNotFormatted = FALSE) |>
      dplyr::select(-c("estimate_type", "additional_name", "additional_level", "result_id")) |>
      dplyr::mutate(order = dplyr::case_when(
        stringr::str_starts(tolower(.data$variable_name), "hipstar") ~ 3,
        stringr::str_starts(tolower(.data$variable_name), "death") ~ 2,
        .default = 1 
      )) |>
      dplyr::arrange(order, variable_name, variable_level) |>
      dplyr::select(-"order") |>
      tidyr::pivot_wider(names_from = "cdm_name", values_from = "estimate_value") |>
      visOmopResults::gtTable(
        groupColumn = "cohort_name", 
        colsToMergeRows = c("variable_name", "sex", "age_group"))
  })
  # study lsc ----
  getLsc <- reactive({
    filterData(data$lsc, "lsc", input)
  })
  output$lsc_data <- DT::renderDT({
    x <- getLsc()
    cols <- input$lsc_compare
    if (length(cols) > 0) {
      x <- x |> 
        tidyr::pivot_wider(names_from = dplyr::all_of(cols), values_from = "estimate_value")
    } else {
      x <- x |> dplyr::rename(percentage = estimate_value)
    }
    DT::datatable(data = x, options = list(scrollX = TRUE))
  })
  # end ----
}

shinyApp(ui = ui, server = server)
