#' Read Rounds of Draw
#' @param table. A table node from \code{html_nodes}
#' @param tablenum. A numeric number for the index of the table in the page's html tables
#' @param url. A character url
#' @return A data frame of match results by round
#' @export
read_round_table <- function(table, tablenum, url){
	
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
			if(any(grepl("[A-Za-z]", x)))
				x[grepl("[A-Za-z]",x)] <- gsub("^( +)?[[:punct:]]", "", iconv(x[grepl("[A-Za-z]",x)], to="ASCII//TRANSLIT"))
			y <- sub("\n", "", sub("^ +", "", grep("[0-9\n]", x, val = T)))
			y[y %in% c("Q","WC","LL","SE","PR","Alt","PR")] <- ""
			y[grepl("/[WQPL]", y)] <- ""
			# capitalisation
			y[y == "DEF"] <- "d."
			y[y == "W/O"] <- "w/o"
			y[grepl("[a-z]", y)] <- sub("(^[^wr])([a-z])", "\\U\\1\\L\\2", y[grepl("[a-z]", y)], perl = T)
			matrix(y, ncol = length(grep("^[A-Z\u2013]", y)))
			})		
			
	maxsets <- max(sapply(results, nrow))
	
	results <- do.call("cbind", lapply(results, function(x){
		if(nrow(x) < maxsets)
			rbind(x, matrix("", nr = maxsets - nrow(x), nc = ncol(x)))
		else
			x
	}))
	
	# type of irregularity
	if((length(rounds) == 4 & ncol(results) != 30) | (length(rounds) == 3 & ncol(results) != 14) | (length(rounds) == 2 & ncol(results) != 6)){
		irregular_round_table(results, rounds, tablenum, url)
	}
	else{
		regular_round_table(results, rounds, tablenum, url)
	}
}