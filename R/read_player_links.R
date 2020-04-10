#' @export
read_player_links <- function(table){
	
	trs <- table %>%
		html_nodes("tr")
		
	tab <- lapply(1:length(trs), function(x){
			trs[x] %>%
				html_nodes("td") %>%
				html_text()
		})
		
	player_links <- lapply(1:length(trs), function(x){
			 trs[x] %>%
				 html_nodes("td") %>%
				 html_nodes("a") %>%
				 html_attr("href")
		 })		
		 
	agrep_check <- function(name, link){
		x <- agrep(sub("(.*)([A-Z][a-z]*$)","\\2", name),link,val = T, ignore = T)
		if(length(x) == 0)
			NA
		else
			x[1]
	}
		 
	tab <- tab[-1]
	player_links <- player_links[-1]
	player_links <- player_links[sapply(tab, function(x) any(grepl("[A-Za-z0-9\u2013]", x)))]
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
			grep("^[A-Z\u2013]", y, val = T) # Return player info only
			})		
			
	match_link <- function(name, link){
		if(length(name) == 1 & length(link) == 2)
			data.frame(
				player_page = link[2],
				name = gsub("[[:punct:]]", "", name),
				stringsAsFactors = F
			)
		else if(length(name) == 2 & length(link) == 4)
			data.frame(
				player_page = link[c(2,4)],
				name = gsub("[[:punct:]]", "", name),
				stringsAsFactors = F
			)	
		else if (length(name) == 2){
			data.frame(
				player_page = c(
					agrep_check(name[1],link),
					agrep_check(name[2],link)
					),
				name = gsub("[[:punct:]]", "", name),
				stringsAsFactors = F
			)	
		}
		else{
			NULL
		}
	}			
	
	
	do.call("rbind", mapply(
		match_link,
		name = results,
		link = player_links,
		SIMPLIFY = F
	))		%>%
	unique()
}