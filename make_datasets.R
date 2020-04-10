library(wikidraws)

savedir <- "" # Set location for storing datasets
csv_savedir <- "" # Set location for storing datasets
baseurl <- "https://en.wikipedia.org"


data(slams)
#slams <- read_events() # Men's and women's singles slam event pages

usopen <- slams %>% filter(tournament == "US Open") # Limit to usopen

urls <- file.path(baseurl, usopen$links)

## Irregular draws
irregularities <- do.call("rbind", lapply(urls, read_irregularities))	 

irregularities <- irregularities %>%
	filter(irregular)

## Players
usopen_players <- collect_players(urls)

## Draws
usopen <- collect_draws(urls)


## Save results
save(slams, file = file.path(savedir, "slams.RData"))
save(irregularities, file = file.path(savedir, "irregularities.RData"))
save(usopen_players, file = file.path(savedir, "usopen_players.RData"))
save(usopen, file = file.path(savedir, "usopen.RData"))


write.csv(slams, file = file.path(csv_savedir, "slams.csv"), row.names = F)
write.csv(usopen_players, file = file.path(csv_savedir, "usopen_players.csv"), row.names = F)
write.csv(usopen, file = file.path(csv_savedir, "usopen.csv"), row.names = F)