#' Read Tables of Draw Results and Combine
#' @param url. A url of the grand slam singles event
#' @return a data.frame of the match results for the event
#' @export
read_draw <- function(url){
	
	print(url)
	
	collate_tables <- function(tables){
		
		tables <- do.call("rbind", lapply(1:length(tables), function(x) {	

			draw <- read_round_table(tables[[x]], tablenum = x, url)
			draw$round <- as.character(draw$round)
			draw$match <- draw$match + x * 100 # Make unique match
			draw
			})
			)			
						
	tables # Need unique?
	}

	tables <- read_tables(url)
	
	tables <- collate_tables(tables)	
	
	minmax <- c(min(tables$set, na.rm = T), max(tables$set, na.rm = T))
	
	tables$set <- factor(tables$set, levels = minmax[1]:minmax[2])
	
	tables <- tables %>%
		tidyr::complete(set, tidyr::nesting(round, match, player), 
		fill = list(gameswon = NA)) %>%
		dplyr::arrange(round, match, player, set)
	
	# Create match order
	tables <- tables %>%
		dplyr::arrange(match) %>%
		dplyr::group_by(round) %>%
		dplyr::mutate(
			match = rep(1:dplyr::n_distinct(match), each = dplyr::n()/dplyr::n_distinct(match))
		)

tables
}

