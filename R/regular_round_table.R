#' @export
regular_round_table <- function(results, rounds, table, url){
	
	three_round_pattern <- function(x){
		
		names <- sub("(.*)([A-Z][a-z]+$)","\\2", x[1,])
		y <- matrix(grepl("[0-9]", x), nr = nrow(x), nc = ncol(x))
		games <- colSums(y, na.rm = T) # Exclude retirements
		
		match1 <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)
		diff_games1 <- tapply(games, match1, function(x) (x[1] != x[2]) & x[1] > 1)
		names1 <- tapply(names, match1, function(x) x[1] == x[2])

		match2 <- c(1,1,2,3,2,3,4,5,4,5,6,7,6,7)
		diff_games2 <- tapply(games, match2, function(x) (x[1] != x[2]) & x[1] > 1)
		names2 <- tapply(names, match2, function(x) x[1] == x[2])
		
		if(sum(diff_games1) < 1 & sum(names1) < 2)
			patterns <- c(1,1,2,2,1,1,3,3,1,1,2,2,1,1)
		else if(sum(diff_games2) < 1 & sum(names2) < 2)
			patterns <-  c(1,1,2,1,2,1,3,1,3,1,2,1,2,1)
		else
			patterns <-  c(1,2,1,2,1,3,1,3,1,2,1,2,1,1)	
	patterns
	}

	four_round_pattern <- function(x){
		
		names <- sub("(.*)([A-Z][a-z]+$)","\\2", x[1,])
		y <- matrix(grepl("[0-9]", x), nr = nrow(x), nc = ncol(x))
		games <- colSums(y, na.rm = T) # Exclude retirements
		
		match1 <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1)
		
		diff_games1 <- tapply(games, match1, function(x)  (x[1] != x[2]) & x[1] > 1)

		match2 <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8, 9, 8, 9, 10,11,10,11,12,13,12,13,14,15,14,15)

		diff_games2 <- tapply(games, match2, function(x) (x[1] != x[2]) & x[1] > 1)
		
		if(sum(diff_games1) < 2)
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1)
		else if (sum(diff_games2) < 2)			
			patterns <-  c(1,1,2,2,1,1,3,3,1,1,2,2,1,1,4, 1, 4, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1)
		else
			patterns <- c(1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1, 4, 1, 4, 1, 1, 2, 2, 
1, 1, 3, 3, 1, 1, 2, 2, 1, 1)
	patterns
	}
	
	if(!any(grepl("[A-Z]", results[1,])))
		results <- results[-1,]	
	
	if(length(rounds) == 1){
		patterns <- rep(1, each = 2)
	}
	else if(length(rounds) == 2){
		patterns <- rep(c(1, 2, 1), each = 2)
	}
	else if(length(rounds) == 3){
		patterns <- three_round_pattern(results)
	}
	else{
		if(url == "https://en.wikipedia.org//wiki/1916_U.S._National_Championships_%E2%80%93_Men%27s_Singles")
			patterns <- c(1,1,2,2,1,1,3,3,1,1,2,2,1,1,4,4,1,1,2,2,1,1,3,3,1,1,2,2,1,1)
		else if(
		(url == "https://en.wikipedia.org//wiki/1886_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table >= 3) |
	(url == "https://en.wikipedia.org//wiki/1941_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table >= 2) |
	(url == "https://en.wikipedia.org//wiki/1942_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
	(url == "https://en.wikipedia.org//wiki/1946_U.S._National_Championships_%E2%80%93_Men%27s_Singles")	|
	(url == "https://en.wikipedia.org//wiki/1947_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
	(url == "https://en.wikipedia.org//wiki/1985_US_Open_%E2%80%93_Women%27s_Singles" & table == 6)
	)
			patterns <- c(1, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1, 4, 1, 4, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1)	
		else
			patterns <- four_round_pattern(results)
	}
	
	patterns <- rounds[patterns]
	
	# Remove row of seeds	
	flatten_results <- data.frame(
		player = rep(results[1,], each = nrow(results) - 1),
		set = rep(1:(nrow(results) - 1), ncol(results)),
		gameswon = as.numeric(results[-1,]),
		incomplete = grepl("[a-z]", results[-1,]),
		round = rep(patterns, each = nrow(results) - 1)
	)	
	
	results <- flatten_results	%>%
		dplyr::arrange(round) %>%
		dplyr::group_by(round) %>%
		dplyr::mutate(
			match = rep(1:(dplyr::n()/(max(set) * 2)), each = max(set) * 2)
		) %>%
		dplyr::group_by(round, match) %>%
		dplyr::mutate(
			incomplete = any(incomplete)
		)

results[grepl("[A-Z]", results$player),]
}