#' Summarizing Violations of Santamour's Rule
#'
#' @param species Species column in community data.
#' @param genus Genus column in community data.
#' @param family Family column in community data.
#' @param region Regional divisions of the community data.
#'
#' @return A summary list containing the count of violations
#' for species, genus, family, and overall.
#' @export
#'
#' @examples
#' \dontrun{
#' data(TreeTown_Data)
#' santamoursummary(TreeTown_Data$Species, TreeTown_Data$Genus,
#' TreeTown_Data$Family, TreeTown_Data$Region)
#' }
santamoursummary <- function(species, genus, family, region) {
			unique_regions     <- unique(region)
			species_violations <- 0
			genus_violations   <- 0
			family_violations  <- 0
			overall_violations <- 0

			for (reg in unique_regions) {
						indices     <- which(region == reg)
						total_count <- length(indices)

						species_perc <- length(unique(species[indices])) / total_count *
							100
						genus_perc   <- length(unique(genus[indices])) / total_count * 100
						family_perc  <- length(unique(family[indices])) / total_count * 100

						if (species_perc > 10) species_violations <- species_violations + 1
						if (genus_perc > 20) genus_violations <- genus_violations + 1
						if (family_perc > 30) family_violations <- family_violations + 1

						if (species_perc > 10 ||
							genus_perc > 20 ||
							family_perc > 30) {
									overall_violations <- overall_violations + 1
						}
			}

			summary_list <- list(
				species_violations = species_violations,
				genus_violations   = genus_violations,
				family_violations  = family_violations,
				overall_violations = overall_violations
			)

			return(summary_list)
}
