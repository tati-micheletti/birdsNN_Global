defineModule(sim, list(
  name = "birdsNN_Global",
  description = "",
  keywords = c("PreVal", "validation architecture", "birds", "DDA", "autocorrelation"),
  authors = structure(list(list(given = "Tatiane", family = "Micheletti",
                                 role = c("aut", "cre"),
                                 email = "tatiane.micheletti@ufz.de", comment = NULL)),
                       class = "person"),
  childModules = character(0),
  version = list(birdsNN_Global = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "birdsNN_Global.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 3.1.2.9018)", "ggplot2", "data.table", "MASS", "AER", "sf"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    ## Opt-in: pin a fixed cacheId per event so a pre-seeded Google Drive folder
    ## can short-circuit a deterministic event to a download. To enable,
    ## uncomment the block below (the leading `,` is valid R as a continuation),
    ## edit the cacheId/cloudFolderID, and set `.useCache` above to include the
    ## relevant event name(s), e.g. `c("init")`.
    # ,defineParameter(".useCacheArgs", "list",
    #                  list(init = list(cacheId       = "_v1.0",
    #                                   useCloud      = TRUE,
    #                                   cloudFolderID = "<google-drive-folder-id>")),
    #                  NA, NA,
    #                  paste("Optional named list, keyed by event name, of extra arguments",
    #                        "passed to reproducible::Cache() for that event. Useful for",
    #                        "pinning a fixed cacheId so a pre-seeded cloud folder can",
    #                        "short-circuit a deterministic event."))
  defineParameter("targetSpeciesEURING", "numeric", NA_real_, NA, NA,
                    "EURING code for the species of interest.",
                    "on. NA (default) auto-picks the species with the most rows."),
  defineParameter("probeflaechenShpSubpath", "character",
                    "raw/dda/MhB_Probeflaechen_DE_S2637_epsg25832.shp", NA, NA,
                    "Path to the DDA route-coordinate shapefile -- resolved via",
                    "dataPrep_Monitor's resolvePath(outputPath(sim), ...): relative",
                    "value stays under outputPath(sim) (default), absolute value",
                    "points anywhere else (e.g. a shared data/ folder). This is",
                    "genuinely SHARED raw data (dataPrep_Monitor also reads it), not",
                    "module-private -- do not point this at dataPath(sim)."),
  defineParameter("maxBlockSizeM", "numeric", 200000, NA, NA,
                    "Cap in metres on the empirically-derived spatial CV block size.",
                    "Matches inputs_Monitor's own landscape-scale default (200km).")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "birdTerritoryRaw", objectClass = "data.table",
                 desc = paste("Raw DDA Territory table",
                              "(i.e., BirdStats_Daten2005_2024D_alle_Territory), with at least",
                              "Nummer, Countdate, EURING, Reviere, ROUTENCODE."),
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
        createsOutput(objectName = "birdResidualCheck", objectClass = "list",
                  desc = paste("Output of fitBaselineResiduals(): residualTable,",
                               "euringCode used, fitted model object, and dispersion",
                               "statistic. Feeds the next diagnostic step (variogram).")),
        createsOutput(objectName = "birdBlockSize", objectClass = "list",
                  desc = paste("Output of determineBirdBlockSize(): per-species",
                               "empirical spatial CV block size (metres) and the",
                               "per-route mean-residual sf object it was derived from."))
  )
))

doEvent.birdsNN_Global = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "birdsNN_Global", "cleaningData")
      sim <- scheduleEvent(sim, start(sim), "birdsNN_Global", "residualCheck")
      sim <- scheduleEvent(sim, start(sim), "birdsNN_Global", "spatialBlockCheck")
    },
    cleaningData = {
      # Here, as we are working with abundance, we need to:
      #  delete Reviere = -1, standardize route code (all to lowercase), 
      # make sure year is formatted as a time variable

      sim$birdTerritoryCleaned <- cleanDataset(rawDDAdataset = sim$birdTerritoryRaw)

    },
    residualCheck = {
      # Here we check the residuals to check for spatial-autocorrelation 
      # (GLM residuals per species). This will decide which architecture 
      # to use: if we need or not spatial blocking.
  
  sim$birdResidualCheck <- fitBaselineResiduals(
    territoryData  = sim$birdTerritoryCleaned,
    euringCode = P(sim)$targetSpeciesEURING
  )
      browser()

    },
    spatialBlockCheck = {
      # Empirical spatial CV block size per species, from cv_spatial_autocor()
      # run on per-route MEAN residuals (not raw occurrence, and not raw
      # per-route-year residuals -- see determineBirdBlockSize.R). Reuses
      # inputs_Monitor's determineBlockSize() (loadOrder puts inputs_Monitor
      # before this module) rather than duplicating it -- a deliberate
      # deviation from this codebase's usual duplicate-small-functions
      # convention, since birdsNN_Global already structurally depends on
      # inputs_Monitor being loaded first.

      sim$birdBlockSize <- determineBirdBlockSize(
        birdResidualCheck = sim$birdResidualCheck,
        probeflaechenShpPath = resolvePath(outputPath(sim), P(sim)$probeflaechenShpSubpath),
        maxBlockSizeM = P(sim)$maxBlockSizeM
      )

    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere('birdTerritoryRaw', sim)) {
    stop(paste0("birdTerritoryRaw needs to be supplied.",
    "As this data is not public, there are currently no defaults.",
    "Please contact DDA to ask for the file 'BirdStats_Daten2005-2024D_alle_Territory.csv'")) 
  }

  return(invisible(sim))
}