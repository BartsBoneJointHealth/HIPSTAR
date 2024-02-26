
# create cdm reference -----
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_schema,
  write_schema = c(schema = write_schema,
                   prefix = study_prefix),
  cdm_name = db_name
)

# export cdm snapshot ----
cli::cli_inform("Exporting cdm summary")
write.csv(summary(cdm),
          file = here("Results",
                      paste0("cdm_summary_",
                             cdm_name(cdm),
                             "_",
                             Sys.Date(),
                             ".csv")),
          row.names = FALSE)



# create denominator whole database population -----
cli::cli_inform("Creating a cohort with everyone in the database")
cdm <- generateDenominatorCohortSet(cdm = cdm,
                                    name =  "db_population_cohort")

# patient demographics -----
cli::cli_inform("Getting patient demographics")
chars <- summariseCharacteristics(cdm$db_population_cohort)
write.csv(chars,
          file = here("Results",
                      paste0("patient_demographics_",
                             cdm_name(cdm),
                             "_",
                             Sys.Date(),
                             ".csv")),
          row.names = FALSE)

# large scale characteristics -----
cli::cli_inform("Running large scale characterisation")
lsc <- summariseLargeScaleCharacteristics(cohort = cdm$db_population_cohort,
                                          window = list(c(-Inf, -1),
                                                        c(0, 0),
                                                        c(0, 365),
                                                        c(0, Inf),
                                                        c(-Inf, Inf)),
                                          eventInWindow = c("condition_occurrence",
                                                            "visit_occurrence",
                                                            "observation",
                                                            "drug_exposure",
                                                            "procedure_occurrence",
                                                            "device_exposure",
                                                            "measurement"
                                          ),
                                          minimumFrequency = 0)
write.csv(lsc,
          file = here("Results",
                      paste0("large_Scale_characteristics_",
                             cdm_name(cdm),
                             "_",
                             Sys.Date(),
                             ".csv")),
          row.names = FALSE)



# drop intermediate tables ----
cli::cli_inform("Dropping any intermediate tables")
CDMConnector::dropTable(cdm = cdm,
                        name = tidyselect::contains("population_cohort"))

# disconnect from db ----
cli::cli_inform("Closing database connection")
cdmDisconnect(cdm)
