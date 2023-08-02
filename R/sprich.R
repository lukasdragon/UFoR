#' Species Richness Calculation
#'
#' @param x Community data (matrix-like object or a vector).
#' @param region Regional divisions of the community data.
#' @param se Estimate standard errors (TRUE/FALSE).
#'
#' @return A summary list containing species richness for each region and optionally standard errors.
#' @export
#'
#' @examples
#' \dontrun{
#' data(TreeTown_Data)
#' sprich(TreeTown_Data$Column1, TreeTown_Data$Column2, se = TRUE)
#' }
sprich <- function(x, region, se = FALSE) {
  unique_regions <- unique(region)
  richness_list <- list()
  standard_errors <- NULL

  for (reg in unique_regions) {
    indices <- which(region == reg)
    region_data <- x[indices, , drop = FALSE]

    # Calculate species richness
    species_richness <- length(unique(region_data))
    richness_list[[as.character(reg)]] <- species_richness

    # Optionally calculate standard errors
    if (se) {
      se_value <- sqrt(sum((region_data - mean(region_data))^2) / length(region_data))
      standard_errors <- c(standard_errors, se_value)
    }
  }

  summary_list <- list(richness = richness_list)
  if (se) summary_list$standard_errors <- standard_errors

  return(summary_list)
}
