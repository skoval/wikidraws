#' Scrape Draw Results
#' @param urls. Vector of Grand Slam Wiki event urls
#' @return Data frame with game score, players, round and event for each Grand Slam
#' @details The \code{tiebreak} column are the points won in the tiebreak, if played
#' @export
collect_draws <- function(urls){	
	
	warn <- options("warn")$warn
	
	options("warn" = -1)
	
	on.exit(options("warn" = warn))	
	
	events <- lapply(urls, read_draw)
	
	n <- sapply(events, nrow)

	events <- do.call("rbind", events)

	events$links <- rep(urls, n)

	events$player <- gsub("[[:punct:]]", "", events$player) # Clean names

	events$tiebreak <- ifelse(grepl("^[6-7][0-9]+",events$gameswon), sub("^[6-7]", "", events$gameswon), NA)

	events$gameswon <- ifelse(grepl("^[6-7][0-9]+",events$gameswon), substr(events$gameswon, 1, 1), events$gameswon)
	
	events <- events %>%
		dplyr::mutate_at(
			vars(gameswon, tiebreak),
			list(as.numeric)
		)

events	%>% unique()
}