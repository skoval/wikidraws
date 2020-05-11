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
	
	events$round <- sub("-", "", tolower(events$round))
	
	# Correct rounds
	events$round[events$round == "finals"] <- "final"
	events$round[events$round %in% c("1st round", "round 1")] <- "first round"
	events$round[events$round %in% c("2nd round", "round 2")] <- "second round"
	events$round[events$round %in% c("3rd round", "round 3")] <- "third round"
	events$round[events$round %in% c("4th round", "round four", "round 4")] <- "fourth round"
	events$round[events$round == "quarterfinal"] <- "quarterfinals"

	events <- events %>%		
		filter(!((round == "quarterfinals" & match > 4) | (round == "semifinals" & match > 2))) %>% # Repeat quarters
		dplyr::group_by(links, round) %>%
		dplyr::mutate(
			matchcheck = rep(1:(dplyr::n() /(max(as.numeric(set)) * 2)), each = max(as.numeric(set)) * 2)
		)	%>%
		dplyr::filter(match == matchcheck) %>%
		dplyr::select(-matchcheck)
	
	## Correct tiebreaks
	games_tiebreak <- function(x, y, incomplete, url, match = 60){
		# Return games for x corrected for tiebreak
		isner_mahut = "https://en.wikipedia.org//wiki/2010_Wimbledon_Championships_%E2%80%93_Men%27s_Singles"
		isner_mahut_match = 60
		
	 if(url == isner_mahut & match == isner_mahut_match & ((x == 68 | x == 70) & (y == 68 | y == 70)))
	 	x
	 else if(
	 	(grepl("^6[0-9]?", x) & grepl("^6[0-9]?", y) & !is.na(incomplete) & incomplete) |
	 	(grepl("^6[0-9]?", x) & grepl("^7[0-9]?", y)) | 
	 	(grepl("^7[0-9]?", x) & grepl("^6[0-9]?", y)) |
	 	(grepl("^8[0-9]?", x) & grepl("^9[0-9]?", y)) | 
	 	(grepl("^9[0-9]?", x) & grepl("^8[0-9]?", y)) |
	 	(grepl("^12[0-9]?", x) & grepl("^13[0-9]?", y)) |
	 	(grepl("^13[0-9]?", x) & grepl("^12[0-9]?", y))
	 	){
	 		if(
		(grepl("^6[0-9]?", x) & grepl("^6[0-9]?", y) & !is.na(incomplete) & incomplete) |	 	
	 	(grepl("^6[0-9]?", x) & grepl("^7[0-9]?", y)) | 
	 	(grepl("^7[0-9]?", x) & grepl("^6[0-9]?", y)) |	 	
	 	(grepl("^8[0-9]?", x) & grepl("^9[0-9]?", y)) | 
	 	(grepl("^9[0-9]?", x) & grepl("^8[0-9]?", y)) 
	 	)
	 			as.numeric(substr(x, 1, 1))
	 		else 
	 			as.numeric(substr(x, 1, 2)) # Wimbledon case
	 	}	 	
	 else
	 	x
	}
	
	points_tiebreak <- function(x, y, incomplete, url, match = 60){
		# Return games for x corrected for tiebreak
		isner_mahut = "https://en.wikipedia.org//wiki/2010_Wimbledon_Championships_%E2%80%93_Men%27s_Singles"
		isner_mahut_match = 60
		
	 if(url == isner_mahut & match == isner_mahut_match & ((x == 68 | x == 70) & (y == 68 | y == 70)))
	 	NA
	 else if(
	 	(grepl("^6[0-9]?", x) & grepl("^6[0-9]?", y) & !is.na(incomplete) & incomplete) |
	 	(grepl("^6[0-9]?", x) & grepl("^7[0-9]?", y)) | 
	 	(grepl("^7[0-9]?", x) & grepl("^6[0-9]?", y)) |
	 	(grepl("^8[0-9]?", x) & grepl("^9[0-9]?", y)) | 
	 	(grepl("^9[0-9]?", x) & grepl("^8[0-9]?", y)) |	 	
	 	(grepl("^12[0-9]?", x) & grepl("^13[0-9]?", y)) |
	 	(grepl("^13[0-9]?", x) & grepl("^12[0-9]?", y))
	 	){
	 		if(
		(grepl("^6[0-9]?", x) & grepl("^6[0-9]?", y) & !is.na(incomplete) & incomplete) |	 	
	 	(grepl("^6[0-9]?", x) & grepl("^7[0-9]?", y)) | 
	 	(grepl("^7[0-9]?", x) & grepl("^6[0-9]?", y)) |	 	
	 	(grepl("^8[0-9]?", x) & grepl("^9[0-9]?", y)) | 
	 	(grepl("^9[0-9]?", x) & grepl("^8[0-9]?", y))){
	 		    x <- as.numeric(substr(x, 2, nchar(x)))
	 		    y <- as.numeric(substr(y, 2, nchar(y)))
	 		    }
	 		else {
	 			x <- as.numeric(substr(x, 3, nchar(x))) # Wimbledon case
	 			y <- as.numeric(substr(y, 3, nchar(y)))
	 		}
	 	ifelse(is.na(x), max(c(7, y + 2), na.rm = T), x)
	 	}
	 else
	 	NA
	}
	
	games_tiebreak <- Vectorize(games_tiebreak)
	points_tiebreak <- Vectorize(points_tiebreak)		
	
	events <- events %>% unique()	
	
	events <- events %>%	
		dplyr::group_by(links, round, match, set) %>%
		dplyr::mutate(
			ogameswon = gameswon[2:1],
			tiebreak = points_tiebreak(gameswon, ogameswon, incomplete, links, match),
			gameswon = games_tiebreak(gameswon, ogameswon, incomplete, links, match)
		) %>%
		dplyr::ungroup()

	events$ogameswon <- NULL

events
}