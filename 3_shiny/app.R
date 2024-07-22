library(shiny)
library(shinydashboard)

source(here::here("functions.R"), local = TRUE)

data <- readData()

data$lsc <- data$lsc |>
  dplyr::filter(estimate_name == "percentage") |>
  dplyr::select("cdm_name", "variable_name", "window" = "variable_level", "estimate_value", "additional_name", "additional_level") |>
  dplyr::mutate(dplyr::across(
    dplyr::starts_with("additional"), ~ gsub(" and ", " &&& ", .x)
  )) |>
  visOmopResults::splitAdditional() |>
  dplyr::mutate(estimate_value = as.numeric(estimate_value), concept = as.integer(concept)) |>
  dplyr::select("cdm_name", "table_name", "concept_name" = "variable_name", "concept_id" = "concept", "window", "percentage" = "estimate_value")

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
        ),
        menuSubItem(
          text = "Database demographics",
          tabName = "db_demographics"
        ),
        menuSubItem(
          text = "Database codes",
          tabName = "db_codes"
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
      ### db demographics ----
      tabItem(
        tabName = "db_demographics",
        h4("Demographics at the moment they enter to the database."),
        selectors(data$patient_demographics, "db_demographics", c("cdm_name", "variable_name")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            gt::gt_output("db_demographics_tidy") |> shinycssloaders::withSpinner()
          ),
          tabPanel(
            "Plot",
            plotOutput("db_demographics_plot") |> shinycssloaders::withSpinner()
          )
        )
      ),
      ### db codes ----
      tabItem(
        tabName = "db_codes",
        h4("Percentage of the concept ids recorded in the database"),
        h5("Windows origin is at start of subject observation"),
        h5("Only records within the observation period of the subjects are reported"),
        selectors(data$lsc, "db_codes", c("cdm_name", "window", "table_name")),
        shinyWidgets::pickerInput("db_codes_pivot", label = "Pivot", choices = c("cdm_name", "window"), selected = character(), multiple = TRUE),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Data table",
            DT::DTOutput("db_codes_data") |> shinycssloaders::withSpinner()
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
  # db demographics ----
  getDemographics <- reactive({
    filterData(data$patient_demographics, "db_demographics", input)
  })
  output$db_demographics_tidy <- gt::render_gt({
    getDemographics() |>
      dplyr::filter(!estimate_name %in% c("q05", "q95")) |>
      CohortCharacteristics::tableCharacteristics(
        header = c("cdm_name"), 
        excludeColumns = c("cohort_name", "result_id", "estimate_type", "additional_name", "additional_level"))
  })
  output$db_demographics_plot <- renderPlot({
    x <- getDemographics()
    vars <- x$variable_name |> unique()
    validate(need(length(vars) == 1, "Please select only one variable to plot"))
    type <- switch(vars,
                   "Number subjects" = "barplot", 
                   "Number records" = "barplot",
                   "Cohort start date" = "boxplot",
                   "Cohort end date" = "boxplot",
                   "Age" = "boxplot",
                   "Sex" = "barplot%",
                   "Prior observation" = "boxplot",
                   "Future observation" = "boxplot")
    if (type == "barplot") {
      x <- x |>
        dplyr::mutate(estimate_value = as.integer(estimate_value))
      p <- ggplot2::ggplot(x, mapping = ggplot2::aes(fill = cdm_name, color = cdm_name, x = cdm_name, y = estimate_value)) +
        ggplot2::geom_bar(stat = "identity")
    } else if (type == "barplot%") {
      vars <- "Percentage of females"
      x <- x |>
        dplyr::filter(estimate_name == "percentage", variable_level == "Female") |>
        dplyr::mutate(estimate_value = as.integer(estimate_value))
      p <- ggplot2::ggplot(x, mapping = ggplot2::aes(fill = cdm_name, color = cdm_name, x = cdm_name, y = estimate_value)) +
        ggplot2::geom_bar(stat = "identity")
    } else {
      x <- x |>
        dplyr::filter(estimate_name %in% c("min", "q25", "median", "q75", "max")) |>
        dplyr::select("cdm_name", "estimate_name", "estimate_value")
      if (grepl("date", vars)) {
        x <- x |>
          dplyr::mutate(estimate_value = as.Date(estimate_value)) 
      } else {
        x <- x |>
          dplyr::mutate(estimate_value = as.numeric(estimate_value))
      }
      x <- x |>
        tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value")
      p <- ggplot2::ggplot(data = x, mapping = ggplot2::aes(color = cdm_name, x = cdm_name, lower = q25, upper = q75, middle = median, ymin = min, ymax = max)) +
        ggplot2::geom_boxplot(stat = "identity")
    }
    p + ggplot2::ylab(vars)
  })
  # db codes ----
  getCodes <- reactive({
    filterData(data$lsc, "db_codes", input)
  })
  output$db_codes_data <- DT::renderDT({
    x <- getCodes()
    if (length(input$db_codes_pivot) > 0) {
      x <- x |> 
        tidyr::pivot_wider(names_from = input$db_codes_pivot, values_from = "percentage")
    }
    DT::datatable(x)
  })
  # end ----
}

shinyApp(ui = ui, server = server)