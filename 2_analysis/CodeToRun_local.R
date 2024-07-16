
# Please restore the renv file first ----
# if you have not installed renv, please first install: install.packages("renv")
renv::activate()
renv::restore()

# Load required packages ----
library(DBI)
library(dplyr)
library(CDMConnector)
library(here)
library(CodelistGenerator)
library(readr)
library(omopgenerics)
library(CohortConstructor)
library(CohortCharacteristics)
library(PatientProfiles)
library(stringr)

db <- DBI::dbConnect(RPostgres::Postgres(),
                     dbname ="cdm_gold_202307",
                     port = Sys.getenv("DB_PORT"),
                     host = "163.1.65.51",
                     user = Sys.getenv("DB_USER"),
                     password =  Sys.getenv("DB_PASSWORD"))

dbName <- "cprd 100k"

# The name of the schema that contains the OMOP CDM with patient-level data
cdmSchema <- "public_100k"

# The name of the schema where results tables will be created
writeSchema <- "results"

writePrefix <- "glp_"

# minimum counts that can be displayed according to data governance
minCellCount <- 5

# create cdm reference -----
cdm <- cdmFromCon(
  con = db,
  cdmSchema = c(schema = cdmSchema),
  writeSchema = c(schema = writeSchema, prefix = writePrefix),
  cdmName = dbName,
  .softValidation = TRUE
)

# Run the study code ----
source(here("RunStudy.R"))
