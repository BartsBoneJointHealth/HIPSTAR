# import codes -----
cli::cli_inform("Getting codes")
codes <- read_csv("codes.csv", col_types = c("i", "c", "c")) |>
  mutate(cohort_name = omopgenerics::toSnakeCase(cohort_name))
codes <- split(x = codes,
      f = codes$cohort_name)
for(i in seq_along(codes)){
  codes[[i]] <- codes[[i]] |>
    pull("concept_id")
}
codes <- omopgenerics::newCodelist(codes)

# build cohorts ----
cli::cli_inform("Instantiating cohorts")
cdm$hipstar_cohorts_any <- conceptCohort(cdm,
              codes,
              name = "hipstar_cohorts_any")
attr(cdm$hipstar_cohorts_any, "cohort_set") <-
  attr(cdm$hipstar_cohorts_any, "cohort_set") |>
  dplyr::mutate(cohort_name = paste0(cohort_name, "_any"))

cdm$hipstar_cohorts_first <- cdm$hipstar_cohorts_any |>
  requireIsFirstEntry(name = "hipstar_cohorts_first")
attr(cdm$hipstar_cohorts_first, "cohort_set") <- attr(cdm$hipstar_cohorts_first, "cohort_set") |>
  dplyr::mutate(cohort_name = paste0(cohort_name, "_first"))

cli::cli_inform("Combining cohorts")
cdm <- omopgenerics::bind(cdm$hipstar_cohorts_any,
     cdm$hipstar_cohorts_first,
     name = "hipstar_cohorts")

# add observation period cohort -----
cli::cli_inform("Adding observation period cohort")
cdm$obs_cohort <- demographicsCohort(cdm,
                   name = "obs_cohort") |>
  requireIsFirstEntry(name = "obs_cohort")
attr(cdm$obs_cohort, "cohort_set") <- attr(cdm$obs_cohort, "cohort_set") |>
  dplyr::mutate(cohort_name = "first_entry_in_data_source")
cdm <- omopgenerics::bind(cdm$hipstar_cohorts,
                          cdm$obs_cohort,
                          name = "hipstar_cohorts")

# add characteristics -----
cli::cli_inform("Adding characteristics")
cdm$hipstar_cohorts <- cdm$hipstar_cohorts %>%
  mutate(year = clock::get_year(cohort_start_date)) %>%
  addDemographics(ageGroup = list(c(0,17),
                                  c(18, 44),
                                  c(45, 64),
                                  c(65, 79),
                                  c(80, Inf))) %>%
  dplyr::compute(name = "hipstar_cohorts",
                 temporary = FALSE,
                 overwrite = TRUE)

# summarise cohort counts over time -----
cli::cli_inform("Getting summary of cohort counts")
cohort_count <- summariseCohortCount(cdm$hipstar_cohorts,
                                     strata = list(c("year"),
                                                   c("year", "age_group"),
                                                   c("year", "sex"),
                                                   c("year", "age_group", "sex")))

# summarise cohort characteristics ----
cli::cli_inform("Getting summary of cohort characteristics")
ids_main <- settings(cdm$hipstar_cohorts) |>
  filter(str_detect(cohort_name,
                    "hip_fracture_conditions|hip_fracture_surgery|first_entry_in_data_source")) |>
  pull("cohort_definition_id")
cdm$hipstar_cohorts_main <-subsetCohorts(cdm$hipstar_cohorts,
              ids_main,
              name = "hipstar_cohorts_main") %>%
  mutate(year = clock::get_year(cohort_start_date)) %>%
  addDemographics(ageGroup = list(c(0,17),
                                  c(18, 44),
                                  c(45, 64),
                                  c(65, 79),
                                  c(80, Inf))) %>%
  dplyr::compute(name = "hipstar_cohorts_main",
                 temporary = FALSE,
                 overwrite = TRUE)

chars <- cdm$hipstar_cohorts_main %>%
  summariseCharacteristics(strata = list(c("age_group"),
                                         c("sex"),
                                         c("age_group", "sex")),
    tableIntersectFlag = list(
      list(tableName = "death",
           targetStartDate = "death_date",
           targetEndDate =  "death_date",
           window = list(c(0, 30))),
      list(tableName = "death",
           targetStartDate = "death_date",
           targetEndDate =  "death_date",
           window = list(c(0, 90))),
      list(tableName = "death",
           targetStartDate = "death_date",
           targetEndDate =  "death_date",
           window = list(c(0, 365))),
      list(tableName = "death",
           targetStartDate = "death_date",
           targetEndDate =  "death_date",
           window = list(c(0, Inf)))
      ),
    cohortIntersectFlag = list(
      list(targetCohortTable = "hipstar_cohorts",
           window = list(c(-Inf, -1))),
      list(targetCohortTable = "hipstar_cohorts",
           window = list(c(0, 0))),
      list(targetCohortTable = "hipstar_cohorts",
           window = list(c(1,30))),
      list(targetCohortTable = "hipstar_cohorts",
           window = list(c(1,90))),
      list(targetCohortTable = "hipstar_cohorts",
           window = list(c(1,365))),
      list(targetCohortTable = "hipstar_cohorts",
           window = list(c(1,Inf)))
    )
  )
attr(chars, "settings")$result_id <- attr(chars, "settings")$result_id * 10L

# large scale characteristics ------
cli::cli_inform("Getting large scale characteristics")
lsc <- cdm$hipstar_cohorts_main %>%
  summariseLargeScaleCharacteristics(
    strata = list(c("age_group"),
                  c("sex"),
                  c("age_group", "sex")),
    window = list(
      c(-Inf, -1),
      c(0, 0),
      c(1,30),
      c(1,90),
      c(1,365),
      c(1,Inf)),
    eventInWindow = c("condition_occurrence",
                      "visit_occurrence",
                      "observation",
                      "drug_exposure",
                      "procedure_occurrence",
                      "device_exposure",
                      "measurement"
    ),
    minimumFrequency = 0)
attr(lsc, "settings")$result_id <- attr(lsc, "settings")$result_id * 100L

# export results -----
cli::cli_inform("Exporting results")
results <- omopgenerics::bind(cohort_count,
                              chars,
                              lsc)
omopgenerics::exportSummarisedResult(results,
                                     minCellCount = minCellCount,
                                     path = here("results"))
