As part of the [Wikipedia's  Grand Slam Project](https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Tennis/Grand_Slam_Project), Wiki editors have collected the draws from all Grand Slam events from the pre-Open Era up to the present. 

The `wikidraws` repository is an R package for scraping the box scores from the Grand Slam draws documented by Wikipedia. The goal is to make it easy to collect the match results and look up records, do interesting analyses, etc.

In addition to the tools for scraping and tidying, the repo has a CSV folder with CSV files of the data that I have collected and reviewed so far. You can also access the same info as data objects in the package. Each Grand Slam is a separate dataset. For example, to see the men's and women's singles draws for all historical Wimbledon events thru 2019, you would do the following:

`
library(wikidraws)

data(wimbledon)
`

If you want to do any scraping, the `collect_draws` would be the function to get started with.

If you want some guidance on collecting batches of event data, have a look at the `make_datasets` file. This could be used to collect the datasets yourself, or update them as new events occur.

Some cautionary notes:

- If you use the stored draw data, note that it includes men's and women's events all together.

- Match numbers can be used to find the two players who played a given match, and these numbers are unique within round (not within event)

- The Wiki pages don't have unique identifiers for players. For players who have a wiki page, I have made a lookup table for each event (e.g. `frenchopen_players`) but for players without a page there may be multiple names that are used for them across events.

- The results of `collect_draws` has a `links` variable that identifys the event. You can merge this with the `draws` dataset to bring in tournament-level descriptors.