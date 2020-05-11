#' @export
irregular_round_table <- function(results, rounds, table, url){
	
	# type of irregularity
	if(url == "https://en.wikipedia.org//wiki/1877_Wimbledon_Championship_%E2%80%93_Singles" & table == 1){
		rounds <- c("Semifinals", "Final", "Second Place")
		results <- results[,-7] 
		patterns <- c(1,1,2,2,1,1,3,3)
	}
	else if(length(rounds) == 4 & ncol(results) == 10)
		patterns <- c(2,2,3,3,2,2,4,4,3,3)
	else if(
	(url == "https://en.wikipedia.org//wiki/1879_Wimbledon_Championship_%E2%80%93_Singles" & table == 1) |
	(url == "https://en.wikipedia.org//wiki/1881_Wimbledon_Championship_%E2%80%93_Singles" & table == 2)
	){
		rounds <- c("Quarterfinals", "Semifinals", "Final", "Second Place")
		results <- results[,-13] 
		patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2,4,4)
	}	
	else if(length(rounds) == 3 & ncol(results) == 8){
		patterns <- c(2,1,2,1,3,3,2,2)
	}
	else if(length(rounds) == 3 & ncol(results) == 10){
		patterns <- c(2,1,2,1,3,1,3,1,2,2)
	}
	else if(length(rounds) == 3 & ncol(results) == 12){
		if (url == "https://en.wikipedia.org//wiki/1877_Wimbledon_Championship_%E2%80%93_Singles" & table == 4)
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2)
		else if (
		(url == "https://en.wikipedia.org//wiki/1897_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles")
		)
			patterns <- c(1,1,2,1,2,1,3,3,1,1,2,2)
		else if (
		(url == "https://en.wikipedia.org//wiki/1878_Wimbledon_Championship_%E2%80%93_Singles" & table == 2)
		)	
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2)
		else
			patterns <- c(1,1,2,2,1,1,3,3,2,2,1,1)
	}	
	else if(length(rounds) == 4 & ncol(results) == 12){
		if(url == "https://en.wikipedia.org//wiki/1972_French_Open_%E2%80%93_Women%27s_Singles" & table == 2)
			patterns <- c(2,2,4,1,4,1,2,1,2,1,3,3)
		else if(url == "https://en.wikipedia.org//wiki/1972_French_Open_%E2%80%93_Women%27s_Singles")
			patterns <- c(3,1,3,1,2,1,2,1,4,4,3,3)
		else
			patterns <- c(1,1,2,2,1,1,3,3,1,1,2,2) + 1
	}
	else if(length(rounds) == 4 & ncol(results) == 14){
		if(
		(url == "https://en.wikipedia.org//wiki/1898_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3,6)) |
		(url == "https://en.wikipedia.org//wiki/1899_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3, 6)) |
		(url == "https://en.wikipedia.org//wiki/1900_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3, 6)) |
		(url == "https://en.wikipedia.org//wiki/1901_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3, 6)) |
		(url == "https://en.wikipedia.org//wiki/1903_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3, 6)) |
		(url == "https://en.wikipedia.org//wiki/1905_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
		(url == "https://en.wikipedia.org//wiki/1905_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3:5,8:10)) |
		(url == "https://en.wikipedia.org//wiki/1906_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3:5,8:10)) |
		(url == "https://en.wikipedia.org//wiki/1907_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2,3,8,9)) |
		(url == "https://en.wikipedia.org//wiki/1908_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:4,7:9)) |
		(url == "https://en.wikipedia.org//wiki/1909_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3,4,9,10)) |
		(url == "https://en.wikipedia.org//wiki/1910_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3,4,9,10)) |
		(url == "https://en.wikipedia.org//wiki/1911_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3, 10)) |
		(url == "https://en.wikipedia.org//wiki/1912_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3,4,8:10)) |
		(url == "https://en.wikipedia.org//wiki/1914_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3, 10)) |
		(url == "https://en.wikipedia.org//wiki/1919_Australasian_Championships_%E2%80%93_Singles" & table %in% c(3, 5)) |
		(url == "https://en.wikipedia.org//wiki/1921_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2,6)) |
		(url == "https://en.wikipedia.org//wiki/1922_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
		(url == "https://en.wikipedia.org//wiki/1924_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1926_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1926_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:4, 7:9)) |
		(url == "https://en.wikipedia.org//wiki/1927_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1927_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:4, 7:9)) |
		(url == "https://en.wikipedia.org//wiki/1928_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3, 5)) |
		(url == "https://en.wikipedia.org//wiki/1928_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:4, 7:9)) |
		(url == "https://en.wikipedia.org//wiki/1929_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3, 8:9)) |
		(url == "https://en.wikipedia.org//wiki/1929_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
		(url == "https://en.wikipedia.org//wiki/1930_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:4, 8:9)) |
		(url == "https://en.wikipedia.org//wiki/1931_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3, 8:9)) |
		(url == "https://en.wikipedia.org//wiki/1932_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2,9)) |
		(url == "https://en.wikipedia.org//wiki/1933_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3, 8:9)) |
		(url == "https://en.wikipedia.org//wiki/1934_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1935_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3, 8:9)) |
		(url == "https://en.wikipedia.org//wiki/1936_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:4, 7:9)) |
		(url == "https://en.wikipedia.org//wiki/1937_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1938_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2,3,5)) |
		(url == "https://en.wikipedia.org//wiki/1938_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3, 8:9)) |
		(url == "https://en.wikipedia.org//wiki/1946_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3,4)) |
		(url == "https://en.wikipedia.org//wiki/1947_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3, 8:9)) |
		(url == "https://en.wikipedia.org//wiki/1948_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3,4)) |
		(url == "https://en.wikipedia.org//wiki/1948_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2,3,8,9)) |
		(url == "https://en.wikipedia.org//wiki/1949_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1950_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
		(url == "https://en.wikipedia.org//wiki/1950_French_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
		(url == "https://en.wikipedia.org//wiki/1953_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2, 9)) |
		(url == "https://en.wikipedia.org//wiki/1955_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
		(url == "https://en.wikipedia.org//wiki/1957_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:4, 7:9)) |
		(url == "https://en.wikipedia.org//wiki/1958_French_Championships_%E2%80%93_Men%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1959_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1960_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1961_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1961_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:4,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1963_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2, 9)) |
		(url == "https://en.wikipedia.org//wiki/1964_French_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2, 9)) |
		(url == "https://en.wikipedia.org//wiki/1969_Australian_Open_%E2%80%93_Men%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1899_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles") |
		(url == "https://en.wikipedia.org//wiki/1904_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(3, 6)) |
		(url == "https://en.wikipedia.org//wiki/1905_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(3, 6)) |
		(url == "https://en.wikipedia.org//wiki/1906_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1907_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(3, 6)) |
		(url == "https://en.wikipedia.org//wiki/1909_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1910_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(3,5:6)) |
		(url == "https://en.wikipedia.org//wiki/1911_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(3, 6)) |
		(url == "https://en.wikipedia.org//wiki/1912_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1913_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1919_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(3, 6)) |
		(url == "https://en.wikipedia.org//wiki/1923_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2:4, 7:9)) |
		(url == "https://en.wikipedia.org//wiki/1925_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1927_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1927_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1928_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1928_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1929_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
		(url == "https://en.wikipedia.org//wiki/1929_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1930_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1930_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1931_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1932_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
		(url == "https://en.wikipedia.org//wiki/1932_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1933_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
		(url == "https://en.wikipedia.org//wiki/1933_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1935_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1935_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1936_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1936_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1937_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1937_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1938_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1938_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1939_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1946_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1946_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1947_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1948_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1949_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1950_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1951_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1952_French_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1952_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1953_French_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1953_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1954_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1955_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1956_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1957_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1958_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1959_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1960_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1961_Australian_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1961_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1962_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2:3,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1962_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1963_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2:4,7:9)) |
		(url == "https://en.wikipedia.org//wiki/1963_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1964_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2:3,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1964_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1965_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2:3,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1965_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2:3,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1966_Australian_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2,5)) |
		(url == "https://en.wikipedia.org//wiki/1966_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2:3,8:9)) |
		(url == "https://en.wikipedia.org//wiki/1966_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1967_Australian_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2, 5)) |
		(url == "https://en.wikipedia.org//wiki/1967_French_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1967_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1968_French_Open_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1968_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1969_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1970_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1971_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1972_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1973_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1974_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1976_US_Open_%E2%80%93_Women%27s_Singles") | (url == "https://en.wikipedia.org//wiki/1947_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1961_French_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2:4,7:9)) |
			(url == "https://en.wikipedia.org//wiki/1934_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1903_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1933_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1936_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1940_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1948_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1937_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1939_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1902_Wimbledon_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1930_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1938_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1940_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1946_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1947_U.S._National_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1951_U.S._National_Championships_%E2%80%93_Men%27s_Singles")
		)
			patterns <- c(2,2,3,3,2,2,4,4,2,2,3,3,2,2)
		else
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2,4,4)
	}
	else if(length(rounds) == 4 & ncol(results) == 16){
		if(
			(url == "https://en.wikipedia.org//wiki/1946_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table ==  3) |
			(url == "https://en.wikipedia.org//wiki/1937_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |			
			(url == "https://en.wikipedia.org//wiki/1948_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |			
			(url == "https://en.wikipedia.org//wiki/1952_French_Championships_%E2%80%93_Men%27s_Singles" & table == 2) | 
			(url == "https://en.wikipedia.org//wiki/1962_French_Championships_%E2%80%93_Men%27s_Singles" & 
            table == 2) |
            (url == "https://en.wikipedia.org//wiki/1895_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |            
			(url == "https://en.wikipedia.org//wiki/1898_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1899_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles" & table == 3) | 
			(url == "https://en.wikipedia.org//wiki/1911_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |			
			(url == "https://en.wikipedia.org//wiki/1912_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) | 			
			(url == "https://en.wikipedia.org//wiki/1920_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1945_U.S._National_Championships_%E2%80%93_Men%27s_Singles")) 
            	patterns <- c(2, 2, 3, 3, 2, 2, 4, 4, 2, 2, 3, 3,  2, 1, 2, 1)
			else if(
				url == "https://en.wikipedia.org//wiki/1895_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles"
			)
				patterns <- c(2,2,3,3,2,1,2,1,4,4,2,2,3,3,2,2)    
			else if (				
		(url == "https://en.wikipedia.org//wiki/1946_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 5)
			)            
				patterns <- c(2, 2, 3, 3, 2, 2, 4, 4, 2, 2, 3, 1, 3, 1, 2, 2)
			else if (
				(url == "https://en.wikipedia.org//wiki/1963_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
				(url == "https://en.wikipedia.org//wiki/1959_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% 2:4)
			)
				patterns <- c(2,1,2,1,3,3,2,2,4,4,2,2,3,3,2,2)
		else if (
			url == "https://en.wikipedia.org//wiki/1952_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3)
		)
			patterns <- c(2, 2, 3, 3, 2, 2, 4, 4, 2, 1, 2, 1, 3, 3, 2, 2)
		else if (
			url == "https://en.wikipedia.org//wiki/1952_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(4)
		)
			patterns <- c(2, 2, 3, 3, 2, 2, 4, 1, 4, 1, 2, 2, 3, 3, 2, 2)			
		else if(
			(url == "https://en.wikipedia.org//wiki/1911_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 2) 
		)
			patterns <- c(2,2,3,1,3,1,2,2,4,4,2,2,3,3,2,2)
		else if(
		(url == "https://en.wikipedia.org//wiki/1935_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table ==  3) |
		(url == "https://en.wikipedia.org//wiki/1900_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
		(url == "https://en.wikipedia.org//wiki/1912_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 5)	| 
		(url == "https://en.wikipedia.org//wiki/1929_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
		(url == "https://en.wikipedia.org//wiki/1938_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
		(url == "https://en.wikipedia.org//wiki/1950_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2)) |
			(url == "https://en.wikipedia.org//wiki/1952_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1956_French_Championships_%E2%80%93_Men%27s_Singles" & table == 9) |
			(url == "https://en.wikipedia.org//wiki/1910_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1925_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1926_Australasian_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1929_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1932_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2)
		)
			patterns <- c(2,2,3,3,2,2,4,4,2,2,3,3,2,1,2,1) # bottom type
		else
			patterns <- c(1,1,2,2,3,3,2,2,4,4,2,2,3,3,2,2) # top type				
	}	
	else if(length(rounds) == 4 & ncol(results) == 18){
		if(
			(url == "https://en.wikipedia.org//wiki/1937_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table ==  3) |
			(url == "https://en.wikipedia.org//wiki/1938_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table ==  3) |
			(url == "https://en.wikipedia.org//wiki/1954_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1901_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1907_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1913_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1913_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1932_French_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1947_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1955_French_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1964_French_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1914_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1926_Australasian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1928_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1930_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1932_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1938_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1949_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1964_French_Championships_%E2%80%93_Women%27s_Singles" & table == 4)
		)
			patterns <- c(2,2,3,3,2,2,4,4,2,2,3,1,3,1,2,1,2,1) # bottom type
		else if (
			(url == "https://en.wikipedia.org//wiki/1947_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 3)
			)
			patterns <- c(2,2,3,3,2,2,4,4,2,2,3,1,3,1,2,2,1,1) 
		else if (
			(url == "https://en.wikipedia.org//wiki/1935_Australian_Championships_%E2%80%93_Men%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1946_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1949_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2,3)) |
			(url == "https://en.wikipedia.org//wiki/1952_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1959_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 5)  |
			(url == "https://en.wikipedia.org//wiki/1963_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1970_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 3)
		)
			patterns <- c(2, 1, 2, 1, 3, 3, 2, 2, 4, 4, 2, 1, 2, 1, 3, 3, 2, 2)			
			else if (
				url == "https://en.wikipedia.org//wiki/1948_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 2
			)
				patterns <- c(2,2,3,3,2,1,2,1,4,4,2,2,3,3,2,1,2,1)
			else if (
			(url == "https://en.wikipedia.org//wiki/1923_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 2) 
		)
			patterns <- c(2, 2, 3, 3, 2, 2, 4, 1, 4, 1, 2, 1, 2, 1, 3, 3, 2, 2)
			else if (
				url == "https://en.wikipedia.org//wiki/1940_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3
			)
				patterns <- c(2,2,3,3,2,2,4,4,2,1,2,1,3,1,3,1,2,2)
			else if (				
			(url == "https://en.wikipedia.org//wiki/1949_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(4,5)) |
			(url == "https://en.wikipedia.org//wiki/1963_Australian_Championships_%E2%80%93_Women%27s_Singles" & table %in% 4:5)
			)
				patterns <- c(2,2,3,1,3,1,2,2,4,4,2,2,3,1,3,1,2,2)
			else if (
				url == "https://en.wikipedia.org//wiki/1972_French_Open_%E2%80%93_Men%27s_Singles"
			)
				patterns <- c(2, 2, 3, 1, 3, 1, 2, 2, 4, 4, 2, 2, 3, 1, 3, 1, 2, 2)
			else if (
				(url == "https://en.wikipedia.org//wiki/1955_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(3, 5))
			)
			patterns <- c(2, 1, 2, 1, 3, 3, 2, 2, 4, 4, 2, 2, 3, 1, 3, 1, 2, 2)
		else if (
			(url == "https://en.wikipedia.org//wiki/1907_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1921_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 3)
		)		
			patterns <- c(2,2,3,1,3,1,2,1,2,1,4,4,2,2,3,3,2,2) # top center
		else if(
			(url == "https://en.wikipedia.org//wiki/1885_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles") |
			(url == "https://en.wikipedia.org//wiki/1948_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 5)
		)
			patterns <- c(1,1,2,2,3,1,3,1,2,2,4,4,2,2,3,3,2,2) # 1100
		else
			patterns <- c(1,1,2,1,2,1,3,3,2,2,4,4,2,2,3,3,2,2) # top type
	}
	else if(length(rounds) == 4 & ncol(results) == 20){
		if(
			(url == "https://en.wikipedia.org//wiki/1953_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table ==  2) |
			(url == "https://en.wikipedia.org//wiki/1891_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1898_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1899_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1906_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 6) |
			(url == "https://en.wikipedia.org//wiki/1907_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1908_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1909_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1914_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1928_French_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1959_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1960_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1963_French_Championships_%E2%80%93_Men%27s_Singles" & table == 3) | 
			(url == "https://en.wikipedia.org//wiki/1902_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1909_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1923_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1924_Australasian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1927_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1934_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1936_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1939_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1940_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1946_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1946_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1947_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1950_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1954_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1955_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1956_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1919_Australasian_Championships_%E2%80%93_Singles" & table == 4)
		)
			patterns <- c(2,2,3,3,2,2,4,4,2,1,2,1,3,1,3,1,2,1,2,1) # bottom type
		else if(
			(url == "https://en.wikipedia.org//wiki/1885_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4)
			)
			patterns <- c(1,1,2,2,3,3,2,2,1,1,4,4,2,1,2,1,3,3,2,2)
		else if (
			url == "https://en.wikipedia.org//wiki/1919_Australasian_Championships_%E2%80%93_Singles" & table == 2
		)
			patterns <- c(2,2,3,3,2,2,4,1,4,1,2,1,2,1,3,1,3,1,2,2)
		else if(
			(url == "https://en.wikipedia.org//wiki/1939_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4)
		)
			patterns <- c(1,1,2,2,3,3,2,1,2,1,4,4,2,1,2,1,3,3,2,2)
		else if(
			(url == "https://en.wikipedia.org//wiki/1909_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 1)
		)
			patterns <- c(1,1,2,1,2,1,3,3,2,2,4,4,2,1,2,1,3,3,2,2)
		else if(url == "https://en.wikipedia.org//wiki/1894_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles")
			patterns <- c(2,2,3,1,3,1,2,1,2,1,4,4,2,1,2,1,3,3,2,2)
		else if(
			(url == "https://en.wikipedia.org//wiki/1970_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 2)
		)
		patterns <- c(2,1,2,1,3,3,2,2,4,4,2,1,2,1,3,3,2,1,2,1) # 1011
		else if(
			(url == "https://en.wikipedia.org//wiki/1963_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% 2:3)
		)		
		patterns <- c(2,1,2,1,3,3,2,1,2,1,4,4,2,2,3,3,2,1,2,1)
		else if (
			(url == "https://en.wikipedia.org//wiki/1970_Australian_Open_%E2%80%93_Women%27s_Singles" & table %in% 4:5) 
		)
			patterns <- c(1,1,2,2,3,1,3,1,2,2,4,4,2,2,3,1,3,1,2,2) 
		else if (			
			(url == "https://en.wikipedia.org//wiki/1963_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% 4:5)
		)
			patterns <- c(1,1,2,2,3,3,2,2,4,1,4,1,2,2,3,1,3,1,2,2)
		else
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2,4,4,2,2,3,3,2,2) # top type
	}			
	else if(length(rounds) == 4 & ncol(results) == 28){
		if(
			(url == "https://en.wikipedia.org//wiki/1921_U.S._National_Championships_%E2%80%93_Men%27s_Singles"  & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1940_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 6) |
			(url == "https://en.wikipedia.org//wiki/1890_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1896_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
		(url == "https://en.wikipedia.org//wiki/1897_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4)		|
		(url == "https://en.wikipedia.org//wiki/1904_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 6)		|
		(url == "https://en.wikipedia.org//wiki/1908_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 1) |
		(url == "https://en.wikipedia.org//wiki/1921_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
		(url == "https://en.wikipedia.org//wiki/1925_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
		(url == "https://en.wikipedia.org//wiki/1953_French_Championships_%E2%80%93_Men%27s_Singles" & table == 8) | (url == "https://en.wikipedia.org//wiki/1968_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
		(url == "https://en.wikipedia.org//wiki/1901_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles" & table == 4) |
		(url == "https://en.wikipedia.org//wiki/1908_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
		(url == "https://en.wikipedia.org//wiki/1922_Australasian_Championships_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1935_French_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |
		(url == "https://en.wikipedia.org//wiki/1937_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
		(url == "https://en.wikipedia.org//wiki/1937_French_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |
		(url == "https://en.wikipedia.org//wiki/1949_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
		(url == "https://en.wikipedia.org//wiki/1961_French_Championships_%E2%80%93_Women%27s_Singles" & table == 6) |
		(url == "https://en.wikipedia.org//wiki/1962_French_Championships_%E2%80%93_Women%27s_Singles" & table == 7) |
		(url == "https://en.wikipedia.org//wiki/1968_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 5) |
		(url == "https://en.wikipedia.org//wiki/1971_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 3) |
		(url == "https://en.wikipedia.org//wiki/1951_Australian_Championships_%E2%80%93_Women%27s_Singles")
		)
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,2) # Top
		else if (
			(url == "https://en.wikipedia.org//wiki/1925_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 2)
		)	
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1)
		else if (
			url == "https://en.wikipedia.org//wiki/1925_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 4
		)
			patterns <- c(1, 1, 2, 1, 2, 1, 3, 3, 2, 1, 2, 1, 4, 1, 4, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1)
		else
			patterns <- c(2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1) # bottom		
	}	
	else if(length(rounds) == 4 & ncol(results) == 24){
		if((url == "https://en.wikipedia.org//wiki/1881_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
		(url == "https://en.wikipedia.org//wiki/1878_Wimbledon_Championship_%E2%80%93_Singles" & table == 5) |
		(url == "https://en.wikipedia.org//wiki/1880_Wimbledon_Championship_%E2%80%93_Singles" & table == 6) |
		(url == "https://en.wikipedia.org//wiki/1882_Wimbledon_Championship_%E2%80%93_Singles" & table == 4) |
		(url == "https://en.wikipedia.org//wiki/1884_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4)
		)
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,3)
		else if (
			(url == "https://en.wikipedia.org//wiki/1964_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1965_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 4)
		)
			patterns <- c(1,1,2,2,3,1,3,1,2,2,4,1,4,1,2,1,2,1,3,1,3,1,2,2)			
		else if (
			url == "https://en.wikipedia.org//wiki/1965_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2
		)
			patterns <- c(2,1,2,1,3,1,3,1,2,1,2,1,4,4,2,1,2,1,3,3,2,1,2,1)
		else if (
			url == "https://en.wikipedia.org//wiki/1965_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3
		)
			patterns <- c(2,1,2,1,3,3,2,1,2,1,4,4,2,1,2,1,3,1,3,1,2,1,2,1)	
		else if (
			url == "https://en.wikipedia.org//wiki/1965_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 5
		)
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,2,3,1,3,1,2,2)					
		else if (			
		(url == "https://en.wikipedia.org//wiki/1925_Australasian_Championships_%E2%80%93_Women%27s_Singles" & table == 2)
		)
			patterns <- c(2,2,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,2)		
		else if (			
		(url == "https://en.wikipedia.org//wiki/1925_Australasian_Championships_%E2%80%93_Women%27s_Singles" & table == 3)
		)
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,2,3,3,2,2)					
		else if (
		(url == "https://en.wikipedia.org//wiki/1920_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 2)
		)
			patterns <- c(2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,1,2,1,3,1,3,1,2,2)
		else if ((url == "https://en.wikipedia.org//wiki/1923_Australasian_Championships_%E2%80%93_Women%27s_Singles"))
			patterns <- c(2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,3,2,2)
		else if(
			(url == "https://en.wikipedia.org//wiki/1933_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1934_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4)		|
	(url == "https://en.wikipedia.org//wiki/1936_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4)	|
	(url == "https://en.wikipedia.org//wiki/1948_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4)	|
	(url == "https://en.wikipedia.org//wiki/1903_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1926_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
	(url == "https://en.wikipedia.org//wiki/1926_French_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
	(url == "https://en.wikipedia.org//wiki/1927_French_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
	(url == "https://en.wikipedia.org//wiki/1933_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1936_French_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
	(url == "https://en.wikipedia.org//wiki/1937_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1938_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1949_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1904_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1907_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1913_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
	(url == "https://en.wikipedia.org//wiki/1929_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
	(url == "https://en.wikipedia.org//wiki/1930_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
	(url == "https://en.wikipedia.org//wiki/1931_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
	(url == "https://en.wikipedia.org//wiki/1953_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
	(url == "https://en.wikipedia.org//wiki/1965_French_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1966_French_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |
	(url == "https://en.wikipedia.org//wiki/1902_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4)
		)	
			patterns <- c(2,2,3,3,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1) # bottom
		else if(
			(url == "https://en.wikipedia.org//wiki/1920_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1920_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
	(url == "https://en.wikipedia.org//wiki/1985_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 8) 
		)
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,1,2,1,3,3,2,2)
		else if(
			(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1939_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table == 2)
		)		
			patterns <- c(1,1,2,1,2,1,3,3,2,1,2,1,4,4,2,1,2,1,3,1,3,1,2,2) # 22122
		else
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,2,3,3,2,2) # top
		}
	else if(length(rounds) == 4 & ncol(results) == 26){
		if(
		(url == "https://en.wikipedia.org//wiki/1910_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 1) |
		(url == "https://en.wikipedia.org//wiki/1965_Australian_Championships_%E2%80%93_Men%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1975_Australian_Open_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1980_Australian_Open_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1980_Australian_Open_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1981_Australian_Open_%E2%80%93_Women%27s_Singles") |
		(url == "https://en.wikipedia.org//wiki/1982_Australian_Open_%E2%80%93_Women%27s_Singles" & table %in% c(2,5))
		)
			patterns <- c(2,1,2,1,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,2)
		else if(
		(url == "https://en.wikipedia.org//wiki/1879_Wimbledon_Championship_%E2%80%93_Singles" & table == 4) |
		(url == "https://en.wikipedia.org//wiki/1884_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles")
		)
			patterns <- c(1, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1, 4,1,4,1,2,2,3,1,3,1,2,2)
		else if(
			url == "https://en.wikipedia.org//wiki/1925_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 3
		)
			patterns <- c(1,1,2,1,2,1,3,3,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,2)
		else if(
			(url == "https://en.wikipedia.org//wiki/1881_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 3)
			)
		patterns <- c(1, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 2, 4, 4, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1) # 33
		else if (
			(url == "https://en.wikipedia.org//wiki/1982_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 4)
		)			
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,1,2,1,3,1,3,1,2,2)
		else if(
			url == "https://en.wikipedia.org//wiki/1939_French_Championships_%E2%80%93_Women%27s_Singles" & table == 2
		)
			patterns <- c(1,1,2,2,3,3,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,2,1,1)
		else if (
			(url == "https://en.wikipedia.org//wiki/1973_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(2,3)) |
			(url == "https://en.wikipedia.org//wiki/1972_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 4) 	|
			(url == "https://en.wikipedia.org//wiki/1964_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1982_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 3)
		)	
			patterns <- c(2,1,2,1,3,1,3,1,2,1,2,1,4,4,2,1,2,1,3,1,3,1,2,1,2,1)
		else if (
			url == "https://en.wikipedia.org//wiki/1973_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(4,5)
		)				
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,1,2,1,3,1,3,1,2,2)
		else if(
			(url == "https://en.wikipedia.org//wiki/1921_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1948_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1955_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org/wiki/1892_Wimbledon_Championships_%E2%80%93_Gentlemen%27s_Singles" & table == 3) | 
			(url == "https://en.wikipedia.org//wiki/1893_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1910_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1914_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1921_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1961_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1967_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1967_French_Championships_%E2%80%93_Men%27s_Singles" & table == 2) | (url == "https://en.wikipedia.org//wiki/1903_Wimbledon_Championships_%E2%80%93_Ladies%27_Singles" & table == 2) | (url == "https://en.wikipedia.org//wiki/1919_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1928_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) | 
			(url == "https://en.wikipedia.org//wiki/1935_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1937_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1961_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1962_French_Championships_%E2%80%93_Women%27s_Singles" & table == 4) 
		)
			patterns <- c(2,2,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1)
		else
		 	patterns <- c(1, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1, 4, 1, 4, 1, 2, 1, 2, 1, 3, 3, 2, 2) # Top pattern
	}
	else if(length(rounds) == 4 & ncol(results) == 22){
		if(
			(url == "https://en.wikipedia.org//wiki/1939_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1951_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1961_U.S._National_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1886_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1888_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1889_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1894_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1905_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 6) |
			(url == "https://en.wikipedia.org//wiki/1911_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1921_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1924_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1927_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1928_Australian_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1931_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4) | (url == "https://en.wikipedia.org//wiki/1934_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4)	|
			(url == "https://en.wikipedia.org//wiki/1935_French_Championships_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1965_French_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1966_French_Championships_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1921_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1927_French_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1938_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1967_Australian_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1974_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 2)
		)
			patterns <-	c(2,2,3,3,2,2,4,1,4,1,2,1,2,1,3,1,3,1,2,1,2,1) # Bottom type
		else if(
			(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(3) ) |
			(url == "https://en.wikipedia.org//wiki/1977_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 6) |
			(url == "https://en.wikipedia.org//wiki/1978_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1979_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 7) |
			(url == "https://en.wikipedia.org//wiki/1980_US_Open_%E2%80%93_Women%27s_Singles" & table == 6)
			)
			patterns <- c(2,1,2,1,3,3,2,1,2,1,4,1,4,1,2,2,3,3,2,1,2,1) # 1221
		else if(
			(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table == 4) 
		)	
			patterns <- c(1,1,2,2,3,1,3,1,2,2,4,4,2,1,2,1,3,1,3,1,2,2)			
			# 1122
		else if(
			(url == "https://en.wikipedia.org//wiki/1934_Wimbledon_Championships_%E2%80%93_Women%27s_Singles") |
			(url == "https://en.wikipedia.org//wiki/1962_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(4,5)) |
		(url == "https://en.wikipedia.org//wiki/1962_Australian_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(4:5)) |
		(url == "https://en.wikipedia.org//wiki/1964_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(4:5))
		)
			patterns <- c(1,1,2,2,3,1,3,1,2,2,4,1,4,1,2,2,3,1,3,1,2,2) # 1111 all up
		else if (
			(url == "https://en.wikipedia.org//wiki/1982_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 9) |
			(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 4)
		)
			patterns <- c(2,1,2,1,3,3,2,1,2,1,4,1,4,1,2,1,2,1,3,3,2,2)
		else if (
			(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 7)
		)		
			patterns <- c(2,1,2,1,3,3,2,1,2,1,4,4,2,1,2,1,3,1,3,1,2,2)
		else if (
			(url == "https://en.wikipedia.org//wiki/1981_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2,6)) |
			(url == "https://en.wikipedia.org//wiki/1978_US_Open_%E2%80%93_Women%27s_Singles" & table == 9) |
			(url == "https://en.wikipedia.org//wiki/1979_US_Open_%E2%80%93_Women%27s_Singles" & table == 8)
		)
			patterns <- c(1,1,2,2,3,1,3,1,2,2,4,1,4,1,2,2,3,1,3,1,2,2)
		else if(
			(url == "https://en.wikipedia.org//wiki/1982_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(4,6)) |
			(url == "https://en.wikipedia.org//wiki/1978_US_Open_%E2%80%93_Women%27s_Singles" & table == 5) |
				(url == "https://en.wikipedia.org//wiki/1980_US_Open_%E2%80%93_Women%27s_Singles" & table == 3)
		)			
			patterns <- c(1,1,2,2,3,1,3,1,2,2,4,4,2,1,2,1,3,3,2,1,2,1)		
		else if(
			(url == "https://en.wikipedia.org//wiki/1982_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(7)) |
			(url == "https://en.wikipedia.org//wiki/1975_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1978_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(3)) |
			(url == "https://en.wikipedia.org//wiki/1980_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |			
				(url == "https://en.wikipedia.org//wiki/1981_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(3,8)) |			
				(url == "https://en.wikipedia.org//wiki/1976_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(3)) |
				(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table == 5) |				
				(url == "https://en.wikipedia.org//wiki/1978_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(6,8)) |
				(url == "https://en.wikipedia.org//wiki/1980_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(2,4,7)) |				
				(url == "https://en.wikipedia.org//wiki/1981_French_Open_%E2%80%93_Women%27s_Singles" & table == 4) 
		)			
			patterns <-  c(1,1,2,2,3,3,2,1,2,1,4,1,4,1,2,2,3,3,2,1,2,1)	
		else if(
			(url == "https://en.wikipedia.org//wiki/1982_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(8)) |
			(url == "https://en.wikipedia.org//wiki/1977_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1976_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(4,5)) |
			(url == "https://en.wikipedia.org//wiki/1978_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(4,9)) |
			(url == "https://en.wikipedia.org//wiki/1979_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1978_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(3, 7)) |
			(url == "https://en.wikipedia.org//wiki/1979_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(5,7))
		)			
			patterns <-  c(1,1,2,2,3,1,3,1,2,2,4,1,4,1,2,2,3,3,2,1,2,1)						
		else if(
			(url == "https://en.wikipedia.org//wiki/1983_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1984_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 5) |			
			(url == "https://en.wikipedia.org//wiki/1985_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1981_French_Open_%E2%80%93_Women%27s_Singles" & table == 9)
		)
			patterns <- c(2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,2,3,1,3,1,2,2)	
		else if (
			(url == "https://en.wikipedia.org//wiki/1976_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 6) |
			(url == "https://en.wikipedia.org//wiki/1980_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 9) |
			(url == "https://en.wikipedia.org//wiki/1979_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1980_US_Open_%E2%80%93_Women%27s_Singles" & table == 8) |
			(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table == 7)
		)
			patterns <- c(2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,2,3,3,2,1,2,1)
		else if (
			(url == "https://en.wikipedia.org//wiki/1976_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 8) |
			(url == "https://en.wikipedia.org//wiki/1980_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(4, 7)) |
			(url == "https://en.wikipedia.org//wiki/1981_French_Open_%E2%80%93_Women%27s_Singles" & table == 7) |
			(url == "https://en.wikipedia.org//wiki/1978_US_Open_%E2%80%93_Women%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1962_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% 2:3) | 			
		(url == "https://en.wikipedia.org//wiki/1962_Australian_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2:3)) |
		(url == "https://en.wikipedia.org//wiki/1964_Australian_Championships_%E2%80%93_Men%27s_Singles" & table %in% c(2:3))
		)
			patterns <- c(2,1,2,1,3,3,2,1,2,1,4,4,2,1,2,1,3,3,2,1,2,1)		# 1111 all down
		else if(
			(url == "https://en.wikipedia.org//wiki/1983_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(4,7)) |
			(url == "https://en.wikipedia.org//wiki/1985_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% 6) |
			(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Women%27s_Singles" & table %in% c(2,9))
		)
			patterns <- c(2,2,3,1,3,1,2,1,2,1,4,4,2,1,2,1,3,1,3,1,2,2)	
		else if(
			(url == "https://en.wikipedia.org//wiki/1983_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% 2:3)
		)
			patterns <- c(2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,1,2,1,3,3,2,2)	
		else if(
			(url == "https://en.wikipedia.org//wiki/1983_Australian_Open_%E2%80%93_Men%27s_Singles" & table  %in% c(6,9)) |
			(url == "https://en.wikipedia.org//wiki/1985_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% 5) 
		)
			patterns <-  c(2,2,3,1,3,1,2,1,2,1,4,1,4,1,2,2,3,1,3,1,2,2)
		else if(
			url == "https://en.wikipedia.org//wiki/1983_Australian_Open_%E2%80%93_Men%27s_Singles" & table  %in% c(7,8)
		)
			patterns <-  c(2,1,2,1,3,3,2,1,2,1,4,1,4,1,2,1,2,1,3,3,2,2)
		else if (
			(url == "https://en.wikipedia.org//wiki/1977_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 7) |
			(url == "https://en.wikipedia.org//wiki/1981_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% 4) |
			(url == "https://en.wikipedia.org//wiki/1978_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(5,8)) |
			(url == "https://en.wikipedia.org//wiki/1981_French_Open_%E2%80%93_Women%27s_Singles" & table %in% c(3, 6)) |
			(url == "https://en.wikipedia.org//wiki/1979_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(2,4))
		)	
			patterns <- c(1,1,2,2,3,3,2,1,2,1,4,4,2,1,2,1,3,1,3,1,2,2)
		else if(
			(url == "https://en.wikipedia.org//wiki/1984_Australian_Open_%E2%80%93_Men%27s_Singles" & table  %in% c(4,8)) |
			(url == "https://en.wikipedia.org//wiki/1977_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1980_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 6) |
			(url == "https://en.wikipedia.org//wiki/1979_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |
			(url == "https://en.wikipedia.org//wiki/1982_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1981_French_Open_%E2%80%93_Women%27s_Singles" & table == 5) 
		)			
			patterns <- c(2,1,2,1,3,3,2,1,2,1,4,4,2,1,2,1,3,1,3,1,2,2)	
		else if(
			(url == "https://en.wikipedia.org//wiki/1984_Australian_Open_%E2%80%93_Men%27s_Singles" & table  %in% c(6)) |			
			(url == "https://en.wikipedia.org//wiki/1982_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2,8)) 
		)			
			patterns <- c(2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,2,3,1,3,1,2,2)	
		else if(
			(url == "https://en.wikipedia.org//wiki/1984_Australian_Open_%E2%80%93_Men%27s_Singles" & table  %in% c(9)) |
			(url == "https://en.wikipedia.org//wiki/1975_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 6) |
			(url == "https://en.wikipedia.org//wiki/1980_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3) |			
			(url == "https://en.wikipedia.org//wiki/1982_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 5)  |
			(url == "https://en.wikipedia.org//wiki/1978_US_Open_%E2%80%93_Women%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1979_US_Open_%E2%80%93_Women%27s_Singles" & table == 9) |
			(url == "https://en.wikipedia.org//wiki/1981_French_Open_%E2%80%93_Women%27s_Singles" & table == 8) |
			(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 3) |
			(url == "https://en.wikipedia.org//wiki/1978_US_Open_%E2%80%93_Women%27s_Singles" & table == 2) 
		)			
			patterns <- c(2,1,2,1,3,3,2,1,2,1,4,1,4,1,2,2,3,1,3,1,2,2)
		else if (
			(url == "https://en.wikipedia.org//wiki/1977_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(2,8)) |
			(url == "https://en.wikipedia.org//wiki/1975_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(4, 9)) |
			(url == "https://en.wikipedia.org//wiki/1979_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 9) |
			(url == "https://en.wikipedia.org//wiki/1976_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(7, 9)) |
			(url == "https://en.wikipedia.org//wiki/1980_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 8)  |			
			(url == "https://en.wikipedia.org//wiki/1981_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(5,9))
		)
			patterns <- c(1,1,2,2,3,1,3,1,2,2,4,4,2,1,2,1,3,3,2,1,2,1)
		else if (
			(url == "https://en.wikipedia.org//wiki/1975_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 8) |
			(url == "https://en.wikipedia.org//wiki/1979_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 8)
		)
			patterns <- c(2,1,2,1,3,3,2,1,2,1,4,4,2,1,2,1,3,3,2,1,2,1)
		else if(
			(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(6,8,9)) |
			(url == "https://en.wikipedia.org//wiki/1982_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 9) |
			(url == "https://en.wikipedia.org//wiki/1977_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 9) |
			(url == "https://en.wikipedia.org//wiki/1980_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 5) |
			(url == "https://en.wikipedia.org//wiki/1980_US_Open_%E2%80%93_Women%27s_Singles" & table %in% c(5,9)) |
			(url == "https://en.wikipedia.org//wiki/1979_US_Open_%E2%80%93_Women%27s_Singles" & table == 3)
		)		
			patterns <- c(1,1,2,2,3,3,2,1,2,1,4,4,2,1,2,1,3,3,2,1,2,1) # 1111
			else if (
			(url == "https://en.wikipedia.org//wiki/1982_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(3)) 
			)
			patterns <- c(1,1,2,2,3,3,2,2,1,1,4,4,2,1,2,1,3,3,2,1,2,1) # 1111 down
			else if (
				(url == "https://en.wikipedia.org//wiki/1977_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 4) |
				(url == "https://en.wikipedia.org//wiki/1975_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 3)  |
				(url == "https://en.wikipedia.org//wiki/1979_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 6) | 
				(url == "https://en.wikipedia.org//wiki/1981_French_Open_%E2%80%93_Women%27s_Singles" & table == 2) |
				(url == "https://en.wikipedia.org//wiki/1979_US_Open_%E2%80%93_Women%27s_Singles" & table == 6) 
			)
			patterns <- c(1,1,2,2,3,3,2,1,2,1,4,1,4,1,2,2,3,1,3,1,2,2)
		else if (
		(url == "https://en.wikipedia.org//wiki/1885_Wimbledon_Championships_%E2%80%93_Men%27s_Singles" & table == 3) |
		(url == "https://en.wikipedia.org//wiki/1982_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 5)
		)
			patterns <- c(2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,2,3,1,3,1,2,2)
		else if ((url == "https://en.wikipedia.org//wiki/1982_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% 2) |
		(url == "https://en.wikipedia.org//wiki/1981_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 7) |
		(url == "https://en.wikipedia.org//wiki/1978_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 6) |
		(url == "https://en.wikipedia.org//wiki/1975_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 5)
		)
			patterns <-  c(2,1,2,1,3,1,3,1,2,2,4,4,2,1,2,1,3,3,2,1,2,1)
		else if (url == "https://en.wikipedia.org//wiki/1912_Australasian_Championships_%E2%80%93_Men%27s_Singles" & table == 1)
			patterns <- c(1,1,2,1,2,1,3,3,2,2,4,4,2,2,3,1,3,1,2,1,2,1)
		else if (
		(url == "https://en.wikipedia.org//wiki/1972_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 3)  
		)
			patterns <- c(2, 1, 2, 1, 3, 3, 2, 2, 4, 1, 4, 1, 2,1,2,1,3,1,3,1,2,2) # 1-3
		else if(
	(url == "https://en.wikipedia.org//wiki/1977_US_Open_%E2%80%93_Women%27s_Singles" & table == 7)
		)		
			patterns <- c(2,1,2,1,3,1,3,1,2,2,4,1,4,1,2,2,3,3,2,2,1,1) # 2211
		else if (
			url == "https://en.wikipedia.org//wiki/1970_Australian_Open_%E2%80%93_Men%27s_Singles" |
			url == "https://en.wikipedia.org//wiki/1971_Australian_Open_%E2%80%93_Men%27s_Singles" |
			(url == "https://en.wikipedia.org//wiki/1972_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(2,5)) |
			(url == "https://en.wikipedia.org//wiki/1973_Australian_Open_%E2%80%93_Women%27s_Singles") |		
		(url == "https://en.wikipedia.org//wiki/1982_French_Open_%E2%80%93_Women%27s_Singles")  |
		(url == "https://en.wikipedia.org//wiki/1984_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(3,5,7)) |
		(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(8:9)) |
		(url == "https://en.wikipedia.org//wiki/1975_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 7) |
		(url == "https://en.wikipedia.org//wiki/1982_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table %in% c(4,6,7))
		)
			patterns <- c(2,1,2,1,3,1,3,1,2,2,4,4,2,1,2,1,3,1,3,1,2,2) # 2-2
			else if (
				(url == "https://en.wikipedia.org//wiki/1976_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 2) |
				(url == "https://en.wikipedia.org//wiki/1978_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 7) |
				(url == "https://en.wikipedia.org//wiki/1979_Wimbledon_Championships_%E2%80%93_Women%27s_Singles" & table == 2)
			)
			patterns <- c(1,1,2,2,3,1,3,1,2,2,4,4,2,1,2,1,3,1,3,1,2,2)
		else if(
			(url == "https://en.wikipedia.org//wiki/1985_Australian_Open_%E2%80%93_Men%27s_Singles" & table %in% c(2:3,7)) |
			(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 8) |
			(url == "https://en.wikipedia.org//wiki/1983_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 2) |
			(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 6) |
	(url == "https://en.wikipedia.org//wiki/1985_Australian_Open_%E2%80%93_Men%27s_Singles" & table == 8) |
	(url == "https://en.wikipedia.org//wiki/1987_Australian_Open_%E2%80%93_Women%27s_Singles" & table == 6)
			)
			patterns <- c(2,2,3,1,3,1,2,1,2,1,4,1,4,1,2,1,2,1,3,3,2,2)
		else
			patterns <- c(1,1,2,1,2,1,3,1,3,1,2,1,2,1,4,4,2,2,3,3,2,2) # Top			  
	}
		
#	print(patterns)
	
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
	) %>%
	dplyr::arrange(round)	
	
	flatten_results	%>%
		dplyr::group_by(round) %>%
		dplyr::mutate(
			match = rep(1:(dplyr::n()/(max(set) * 2)), each = max(set) * 2)
		) %>%
		dplyr::group_by(round, match) %>%
		dplyr::mutate(
			incomplete = any(incomplete)
		)
}