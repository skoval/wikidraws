#' Capture Tables That Don't Follow Knockout Format
#' @param url. A character URL 
#' @return A data frame of each table structure and any that don't follow expected power of 2 knockout structure
#' @export
read_irregularities <- function(url){
	
	warn <- options("warn")$warn
	
	options("warn" = -1)
	
	on.exit(options("warn" = warn))	
	
	irregularity <- function(table){
		
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
			y[y %in% c("Q","WC","LL","SE","PR","Alt")] <- ""
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
		
	data.frame(rounds = length(rounds), results = ncol(results))
	}
	
	tables <- read_tables(url)
	
do.call("rbind", lapply(1:length(tables), function(x){
	
	result <- irregularity(tables[[x]])
	result$table <- x
	result$url <- url
	result$regular <- sum(2^(1:result$rounds))
	result$irregular <- result$regular != result$results

result
}))
}