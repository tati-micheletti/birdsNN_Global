determineBirdBlockSize <- function(birdResidualCheck, probeflaechenShpPath, maxBlockSizeM){

  # Read-only: this shapefile is raw external data, never write to it.
  routeCoords <- sf::st_read(probeflaechenShpPath, quiet = TRUE)

  results <- lapply(names(birdResidualCheck), function(code){

    residualTable <- data.table::copy(birdResidualCheck[[code]]$residualTable)

    # Collapse to one mean residual per route -- a route being consistently
    # above/below average across its own years is a within-route-across-time
    # effect, not evidence of spatial autocorrelation between routes. Only
    # the per-route mean is the right input for the spatial test.
    routeResiduals <- residualTable[, .(meanResidual = mean(residual)), by = ROUTENCODE]

    sfResiduals <- merge(routeCoords, routeResiduals, by.x = "SITE_ID", by.y = "ROUTENCODE")

    blockSizeM <- determineBlockSize(sfResiduals, maxBlockSizeM, column = "meanResidual")

    list(euringCode  = code,
         nRoutes     = nrow(sfResiduals),
         blockSizeM  = blockSizeM,
         sfResiduals = sfResiduals)

  })

  names(results) <- names(birdResidualCheck)

  return(results)
}
