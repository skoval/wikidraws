#' Read Rounds of Draw
#' @param table. A table node from \code{html_nodes}
#' @param tablenum. A numeric number for the index of the table in the page's html tables
#' @param url. A character url
#' @return A data frame of match results by round
#' @export
read_round_table <- function(table, tablenum, url){
	
	seedpattern <- function(x){
		
		numbers <- grep("^[0-9]", x)
		blanks <- which(x == "")
		players <- grep("^[A-z\u2013]", x)
		
		for(i in numbers){			
			if(
				i < min(players) |
				(any(blanks == i-1) & any(blanks == i + 1)) |
				(any(blanks == i-1) & any(players == i + 1))
				)
				x[i] <- sub("[0-9]+","",x[i])
		}
					
		x
	}	
			
	trs <- table %>%
		html_nodes("tr")
		
	tab <- lapply(1:length(trs), function(x){
			trs[x] %>%
				html_nodes("td") %>%
				html_text()
		})
		
	rounds <- sub("\n", "", grep("[A-Z0-9]", tab[[1]], val = T))
	
	tab <- tab[-1]
	tab <- tab[sapply(tab, function(x) any(grepl("[A-Za-z0-9\u2013]", x)))]
	
	results <- lapply(tab, function(x){
			# Replace Byes
			x <- sub("bye", "Bye", x)
			x <- str_remove(x, " ?\\[.*\\]") # Remove any footnotes		
			x <- str_remove(x, "[^A-z]\\.")	
			if(any(grepl("[A-Za-z]", x)))
				x[grepl("[A-Za-z]",x)] <- gsub("^( +)?[[:punct:]]", "", iconv(x[grepl("[A-Za-z]",x)], to="ASCII//TRANSLIT"))
			y <- sub("^ +", "", x)
			y <- seedpattern(y)
			y <- sub("\n", "", grep("[A-z0-9\n]", y, val = T))
			y[y %in% c("Q","WC","LL","SE","PR","Alt","PR","o")] <- ""
			y[grepl("/[WQPL]", y)] <- ""
			# capitalisation
			y[y == "DEF"] <- "d."
			y[y == "w/"] <- "w/o"
			y[y == "W/O"] <- "w/o"
			y[y == "L w/o"] <- "w/o"
			y[y == "W"] <- "w/o"
			y[y == "ABD"] <- "d."
			y[grepl("^r", y)] <- "r."
			y <- sub("([0-9]+)(r)","\\1",y)
			correct <- any(grepl("w/o", y)) & any(grepl("Clapham", y))
			y[grepl("[a-z]", y)] <- sub("(^[^wr])([a-z])", "\\U\\1\\L\\2", y[grepl("[a-z]", y)], perl = T)
			if(all(grep("^[A-Z\u2013]", y) == c(2,7)))	
				y <- c(y[1:6],"","", y[7:12])			
			if(any(grepl("\u2013", y)) & any(grepl("[A-z]", y))){
				hyphen <- grep("\u2013", y)
				player <- grep("[A-Z]", y)
				if(any(hyphen == player - 1))
					y[hyphen] <- ""
			}				
			if(correct) y[7] <- "Unknown"
			if(!grepl("[A-Z]", y[1]) & any(grepl("[A-Z]", y)) & length(y) < 12)		
				y <- y[-1]
			if(!grepl("[A-Z]", y[1]) & any(grepl("[A-Z]", y)) & length(y) > 12)
				y <- y[-(grep("^[A-Z]", y) - 1)]			
			matrix(y, ncol = length(grep("^[A-Z\u2013]", y)))
			})	
			
	maxsets <- max(sapply(results, nrow))
	
	results <- do.call("cbind", lapply(results, function(x){
		if(nrow(x) < maxsets)
			rbind(x, matrix("", nr = maxsets - nrow(x), nc = ncol(x)))
		else
			x
	}))
	
	if(all(results[1,] == ""))
		results <- results[-1,]
		
	# type of irregularity
	if((length(rounds) == 4 & ncol(results) != 30) | (length(rounds) == 3 & ncol(results) != 14) | (length(rounds) == 2 & ncol(results) != 6)){

		irregular_round_table(results, rounds, tablenum, url)
	}
	else{

		regular_round_table(results, rounds, tablenum, url)
	}
}