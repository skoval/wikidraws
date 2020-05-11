#' Scapre Table of Players with Wiki Pages
#' @param urls. Vector of character of event urls
#' @return Data frame of \code{player_page} URL to uniquely identify a player and \code{player} (which may have multiple variants)
#' @export
collect_players <- function(urls){
	
	warn <- options("warn")$warn
	
	options("warn" = -1)
	
	on.exit(options("warn" = warn))	
	
	collate_tables <- function(url){
		
		print(url)
		
		tables <- read_tables(url)
		
		tables <- do.call("rbind", lapply(1:length(tables), function(x) read_player_links(tables[[x]])))
		
	tables
	}

	x <- do.call("rbind", lapply(urls, collate_tables))
	
x %>% 
	dplyr::filter(!is.na(player_page), !is.na(name), name != "Bye") %>%
	dplyr::rename(player = name) %>%
	unique()  
}

