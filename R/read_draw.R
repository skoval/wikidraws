#' Read Tables of Draw Results and Combine
#' @param url. A url of the grand slam singles event
#' @return a data.frame of the match results for the event
#' @export
read_draw <- function(url){
	
	print(url)
	
	collate_tables <- function(tables){
		
		tables <- do.call("rbind", lapply(1:length(tables), function(x) {	
			draw <- read_round_table(tables[[x]], tablenum = x, url)
			draw$match <- draw$match + x * 100 # Make unique match
			draw
			})
			)			
						
	tables # Need unique?
	}

	tables <- read_tables(url)
	
	tables <- collate_tables(tables)
	
	# Create match order
	tables <- tables %>%
		arrange(match) %>%
		group_by(round) %>%
		dplyr::mutate(
			match = rep(1:n_distinct(match), each = n()/n_distinct(match))
		)

tables
}

