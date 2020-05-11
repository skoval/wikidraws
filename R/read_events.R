#' Scrape Wiki URLS For Events
#' @param url. A character of the wiki page with Grand Slam event URLS
#' @return Data frame of \code{links} that uniquely identify and event descriptors
#' @export
read_events <- function(url = "https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Tennis/Grand_Slam_Project"){
	
	page <- read_html(url) 
	
	# Columns
	content <- page %>% 
		html_nodes("table") %>% 
		html_nodes("td") %>%
		html_nodes("a")
	
	links <- content %>%
		html_attr("href")
	
	title <- content %>%
		html_attr("title")
		
	draws <- data.frame(
		links = grep("Singles", links, val = T),
		title = title[grepl("Singles", links)],
		stringsAsFactors = F
	)
	
	draws$links <- sub("-", "%E2%80%93", draws$links)
	
	draws$tournament <- 
	ifelse(grepl("Wimbledon", draws$title), "Wimbledon",
			ifelse(grepl("Austral", draws$title), "Australian Open", 
				ifelse(grepl("French", draws$title), "French Open", "US Open")))
	
	draws$year <- sub("([0-9][0-9][0-9][0-9])(.*)", "\\1", draws$title)
	draws$event <- ifelse(grepl("Men", draws$title), "Men", "Women")
	
	draws$links[draws$links == "/wiki/1946_French_Championships_%E2%80%93_Men%27s_Singles"] <- NA
	
draws
}