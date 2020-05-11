library(wikidraws)

savedir <- "~/Software/wikidraws/data/" # Set location for storing datasets
csv_savedir <- "~/Software/wikidraws/csv/" # Set location for storing datasets
baseurl <- "https://en.wikipedia.org"

slams <- read_events() # Men's and women's singles slam event pages

slams <- slams %>%
	dplyr::filter(!is.na(links))
	
urls <- file.path(baseurl, slams$links)

#irregularities <- do.call("rbind", lapply(urls, read_irregularities))	 

## WIMBLEDON
wimbledon_urls <- file.path(baseurl, slams$links[slams$tournament == "Wimbledon"])
wimbledon_players <- collect_players(wimbledon_urls)
wimbledon <- collect_draws(wimbledon_urls)

## USOPEN
usopen_urls <- file.path(baseurl, slams$links[slams$tournament == "US Open"])
usopen_players <- collect_players(usopen_urls)
usopen <- collect_draws(usopen_urls)


## FRENCH OPEN
frenchopen_urls <- file.path(baseurl, slams$links[slams$tournament == "French Open"])
frenchopen_players <- collect_players(frenchopen_urls)
frenchopen <- collect_draws(frenchopen_urls)


## AUSSIE OPEN
ausopen_urls <- file.path(baseurl, slams$links[slams$tournament == "Australian Open"])
ausopen_players <- collect_players(ausopen_urls)
ausopen <- collect_draws(ausopen_urls)


## Save results
save(slams, file = file.path(savedir, "slams.RData"))
save(irregularities, file = file.path(savedir, "irregularities.RData"))

save(usopen_players, file = file.path(savedir, "usopen_players.RData"))
save(usopen, file = file.path(savedir, "usopen.RData"))

save(wimbledon_players, file = file.path(savedir, "wimbledon_players.RData"))
save(wimbledon, file = file.path(savedir, "wimbledon.RData"))

save(frenchopen_players, file = file.path(savedir, "frenchopen_players.RData"))
save(frenchopen, file = file.path(savedir, "frenchopen.RData"))

save(ausopen_players, file = file.path(savedir, "ausopen_players.RData"))
save(ausopen, file = file.path(savedir, "ausopen.RData"))

write.csv(slams, file = file.path(csv_savedir, "slams.csv"), row.names = F)
write.csv(usopen_players, file = file.path(csv_savedir, "usopen_players.csv"), row.names = F)
write.csv(usopen, file = file.path(csv_savedir, "usopen.csv"), row.names = F)
write.csv(wimbledon_players, file = file.path(csv_savedir, "wimbledon_players.csv"), row.names = F)
write.csv(wimbledon, file = file.path(csv_savedir, "wimbledon.csv"), row.names = F)
write.csv(frenchopen_players, file = file.path(csv_savedir, "frenchopen_players.csv"), row.names = F)
write.csv(frenchopen, file = file.path(csv_savedir, "frenchopen.csv"), row.names = F)
write.csv(ausopen_players, file = file.path(csv_savedir, "ausopen_players.csv"), row.names = F)
write.csv(ausopen, file = file.path(csv_savedir, "ausopen.csv"), row.names = F)

