#' @export
regular_round_table <- function(results, rounds, table, url){
	
	if(length(rounds) == 1){
		patterns <- rep(1, each = 2)
	}
	else if(length(rounds) == 2){
		patterns <- rep(c(1, 2, 1), each = 2)
	}
	else if(length(rounds) == 3){
		patterns <- c(rep(c(1, 2, 1), each = 2), 3)
		patterns <- c(patterns, patterns[length(patterns):1])
	}
	else{
		if(url == "https://en.wikipedia.org//wiki/1916_U.S._National_Championships_%E2%80%93_Men%27s_Singles")
			patterns <- c(1,1,2,2,1,1,3,3,1,1,2,2,1,1,4,4,1,1,2,2,1,1,3,3,1,1,2,2,1,1)
		else if(
		(url == "https://en.wikipedia.org//wiki/1886_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table >= 3) |
	(url == "https://en.wikipedia.org//wiki/1941_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table >= 2) |
	(url == "https://en.wikipedia.org//wiki/1942_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
	(url == "https://en.wikipedia.org//wiki/1946_U.S._National_Championships_%E2%80%93_Men%27s_Singles")	|
	(url == "https://en.wikipedia.org//wiki/1947_U.S._National_Championships_%E2%80%93_Men%27s_Singles")
	)
			patterns <- c(1, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1, 4, 1, 4, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1)
		else
			patterns <- c(1,1,2,2,1,1,3,3,1,1,2,2,1,1,4, 1, 4, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1)
	}
	
	print(table)
	print(patterns)
	patterns <- rounds[patterns]
	
	# Remove row of seeds
	if(!any(grepl("[A-Z]", results[1,])))
		results <- results[-1,]
	
	flatten_results <- data.frame(
		player = rep(results[1,], each = nrow(results) - 1),
		set = rep(1:(nrow(results) - 1), ncol(results)),
		gameswon = as.numeric(results[-1,]),
		incomplete = grepl("[a-z]", results[-1,]),
		round = rep(patterns, each = nrow(results) - 1)
	)
	
	results <- flatten_results	%>%
		arrange(round) %>%
		group_by(round) %>%
		dplyr::mutate(
			match = rep(1:(n()/(max(set) * 2)), each = max(set) * 2)
		) %>%
		group_by(round, match) %>%
		dplyr::mutate(
			incomplete = any(incomplete)
		)

results[grepl("[A-Z]", results$player),]
}