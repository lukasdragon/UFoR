#' Santamour's Species Diversity Rule
#'
#' Santamour's Species Diversity Rule, or the 10-20-30 rule, is a guideline
#' suggesting no more than 10% of any one species, 20% of any one genus,
#' and 30% of any one family should occupy a region. This rule is a preventive
#' measure against the risks associated with over-reliance on a few species.
#'
#' @param species Species column in community data.
#' @param genus Genus column in community data.
#' @param family Family column in community data.
#' @param region Regional divisions of the community data.
#' @param output Either "numeric" for the count of violations or "text"
#' for the species names.
#'
#' @return Either a numeric count of violations of the rule,
#' or the species associated with those violations, depending on output.
#' @export
#'
#' @examples
#' \dontrun{
#' data(TreeTown_Data)
#' santamour(TreeTown_Data$Species, TreeTown_Data$Genus, TreeTown_Data$Family,
#' TreeTown_Data$Region, "numeric")
#' }
santamour <- function(species, genus, family, region, output) {
			unique_regions    <- unique(region)
			violation_count   <- 0
			violating_species <- unique(character(0))

			for (reg in unique_regions) {
						indices     <- which(region == reg)
						total_count <- length(indices)

						species_perc <- length(unique(species[indices])) / total_count *
							100
						genus_perc   <- length(unique(genus[indices])) / total_count * 100
						family_perc  <- length(unique(family[indices])) / total_count * 100

						if (species_perc > 10 ||
							genus_perc > 20 ||
							family_perc > 30) {
									violation_count   <- violation_count + 1
									violating_species <- unique(c(violating_species,
									                              species[indices]))
						}
			}

			if (output == "numeric") {
						return(violation_count)
			} else if (output == "text") {
						return(violating_species)
			} else {
						stop("output must be 'numeric' or 'text'")
			}
}
