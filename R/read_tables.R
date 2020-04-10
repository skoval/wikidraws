#' @export
read_tables <- function(url){
	
	page <- read_html(url)
	
	page %>%
		html_nodes(xpath = "//table[@cellpadding=0]")
}