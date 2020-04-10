#' @export
irregular_round_table <- function(results, rounds, table, url){
	
	# type of irregularity
	if(length(rounds) == 3 & ncol(results) == 12){
		patterns <- c(1,1,2,2,1,1,3,3,2,2,1,1)
	}	
	if(length(rounds) == 4 & ncol(results) == 14){
		patterns <- c(2,2,3,3,2,2,4,4,2,2,3,3,2,2)
	}
	else if(length(rounds) == 4 & ncol(results) == 16){
		if(
			(url == "https://en.wikipedia.org//wiki/1946_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table ==  3)
		)
			patterns <- c(2,2,3,3,2,2,4,4,2,2,3,3,2,1,2,1)
		else if((url == "https://en.wikipedia.org//wiki/1935_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table ==  3))
			patterns <- c(2,2,3,3,2,2,4,4,2,2,3,3,2,2,1,1) # bottom type
		else
			patterns <- c(1,1,2,2,3,3,2,2,4,4,2,2,3,3,2,2) # top type				
	}	
	else if(length(rounds) == 4 & ncol(results) == 18){
		if(
			(url == "https://en.wikipedia.org//wiki/1937_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table ==  3) |
			(url == "https://en.wikipedia.org//wiki/1938_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table ==  3) |
			(url == "https://en.wikipedia.org//wiki/1954_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 2)
		)
			patterns <- c(2,2,3,3,2,2,4,4,2,2,3,1,3,1,2,1,2,1) # bottom type
		else if (
			(url == "https://en.wikipedia.org//wiki/1947_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 3)
			)
			patterns <- c(2,2,3,3,2,2,4,4,2,2,3,1,3,1,2,2,1,1) 
		else
			patterns <- c(1,1,2,1,2,1,3,3,2,2,4,4,2,2,3,3,2,2) # top type
	}
	else if(length(rounds) == 4 & ncol(results) == 20){
		if(
			(url == "https://en.wikipedia.org//wiki/1953_U.S._National_Championships_%E2%80%93_Men%27s_Single" & table ==  2)
		)
			patterns <- c(2,2,3,3,2,2,4,4,2,1,2,1,3,1,3,1,2,1,2,1) # bottom type
		else
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2,4,4,2,2,3,3,2,2) # top type
	}			
	else if(length(rounds) == 4 & ncol(results) == 28){
		if(
			(url == "https://en.wikipedia.org//wiki/1921_U.S._National_Championships_%E2%80%93_Men%27s_Singles"  & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1940_U.S._National_Championships_-_Men%27s_Singles" & table == 6)
		)
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,2) # Top
		else
			patterns <- c(2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1) # bottom		
	}	
	else if(length(rounds) == 4 & ncol(results) == 24){
		if(url == "https://en.wikipedia.org//wiki/1881_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 2)
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,3)
		else if(
			(url == "https://en.wikipedia.org//wiki/1933_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1934_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4)		|
	(url == "https://en.wikipedia.org//wiki/1936_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4)	|
	(url == "https://en.wikipedia.org//wiki/1948_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4)				
		)	
			patterns <- c(2,2,3,3,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1) # bottom
		else if(
			url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table == 2
		)
			patterns <- c(1,1,2,1,2,1,3,3,2,1,2,1,4,4,2,1,2,1,3,1,3,1,2,2) # 22122
		else
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,2,3,3,2,2) # top
		}
	else if(length(rounds) == 4 & ncol(results) == 26){
		if(url == "https://en.wikipedia.org//wiki/1881_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 3)
		patterns <- c(1, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 2, 4, 4, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1) # 33
		else if(
			(url == "https://en.wikipedia.org//wiki/1921_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1948_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1955_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 2)
		)
			patterns <- c(2,2,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1)
		else
		 	patterns <- c(1, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1, 4, 1, 4, 1, 2, 1, 2, 1, 3, 3, 2, 2) # Top pattern
	}
	else if(length(rounds) == 4 & ncol(results) == 22){
		if(
			(url == "https://en.wikipedia.org//wiki/1939_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1951_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1961_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 2)
		)
			patterns <-	c(2,2,3,3,2,2,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1)
		else if(
			(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(3,5) ) |
			(url == "https://en.wikipedia.org//wiki/1978_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(2,6,8) ) |
			(url == "https://en.wikipedia.org//wiki/1979_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(6,9) ) |
			(url == "https://en.wikipedia.org//wiki/1980_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(2,4,6,7) )
		)
			patterns <- c(2,1,2,1,3,3,2,1,2,1,4,1,4,1,2,2,3,3,2,1,2,1) # 1221
		
		else if(
			(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1979_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(2,4))
		)	
			patterns <- c(1,1,2,2,3,1,3,1,2,2,4,4,2,1,2,1,3,1,3,1,2,2)
			# 1122
		else if(
			(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(6,8,9)) |
		(url == "https://en.wikipedia.org//wiki/1978_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(3,4,5,7,9)) |
		(url == "https://en.wikipedia.org//wiki/1979_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(3,5,7,8)) |
		(url == "https://en.wikipedia.org//wiki/1980_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(3,5,9))
		)		
			patterns <- c(1,1,2,2,3,3,2,1,2,1,4,4,2,1,2,1,3,3,2,1,2,1) # 1111
		else if(
			(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table == 7) |
			(url == "https://en.wikipedia.org//wiki/1980_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(8))
		)		
			patterns <- c(2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,2,3,3,2,2,1,1) # 2211
		else
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,4,2,2,3,3,2,2) # Top			  
	}	

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
	
	flatten_results	%>%
		arrange(round) %>%
		group_by(round) %>%
		dplyr::mutate(
			match = rep(1:(n()/(max(set) * 2)), each = max(set) * 2)
		) %>%
		group_by(round, match) %>%
		dplyr::mutate(
			incomplete = any(incomplete)
		)
}