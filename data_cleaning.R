# 0 Load dependencies -----------------------------------------------------

library(glue) # for reading in bunch of similarly-named files
library(readxl) # for reading Excel files
library(here) # for referencing current working directory
library(dplyr) # for data manipulation
library(magrittr) # for assignment pipe
library(tidyr)

# 1 Load data -------------------------------------------------------------

nhl <- read_xlsx(here("data/Summary (0).xlsx"))

for (i in 1:21) {
  temp <- read_xlsx(glue(here("data/Summary ({i}).xlsx")))
  nhl <- rbind(nhl, temp)
}

# 2 Clean data ------------------------------------------------------------

nhl %<>%
  dplyr::rename(Date = `Game Date`, # rename variables to make them easier to work with, syntax newvar = oldvar
         PP_perc_game = `PP%`, # game-level, because we'll be creating running sums/averages in the PP_perc, GF, GA, etc.
         PK_perc_game = `PK%`,
         ShotsFor = `Shots/GP`,
         ShotsAgainst = `SA/GP`,
         GF_game = GF,
         GA_game = GA) 

# recode -- as NA
nhl$PP_perc_game[nhl$PP_perc_game=="--"] <- NA
nhl$PK_perc_game[nhl$PK_perc_game=="--"] <- NA

#cumulative mean, ignoring the NAs
nhl %<>%
  group_by(Team) %>%
  mutate(PP_perc = replace(PP_perc_game, !is.na(PP_perc_game), cummean(PP_perc_game[!is.na(PP_perc_game)])),
         PK_perc = replace(PK_perc_game, !is.na(PK_perc_game), cummean(PK_perc_game[!is.na(PK_perc_game)]))
         ) %>%
  fill(PP_perc,PK_perc)

nhl %<>%
  mutate(GlDiff_game = GF_game - GA_game, # add variables for goal differential
         Sh_perc_game = (GF_game/ShotsFor), # shooting percentage (goals on shots)
         Saves = ShotsAgainst - GA_game, # saves
         Sv_perc_game = (Saves/ShotsAgainst), # save percentage
         Final = ifelse(W == 1, 1, 0)) %>%  # final result, win = 1
  group_by(Team) %>% 
  mutate(GF = cumsum(GF_game), # cumulative sums for GF, GA, GlDiff
         GA = cumsum(GA_game),
         GlDiff = GF - GA,
         Sh_perc = cummean(Sh_perc_game),
         Sv_perc = cummean(Sv_perc_game)) %>%
  select(Date, # select needed columns
         Team, 
         GF, 
         GA, 
         GlDiff, 
         PP_perc, 
         PK_perc,
         Sh_perc,
         Sv_perc,
         Final)

# 3 Export data as csv ----------------------------------------------------

write.csv(nhl, "nhl2022.csv", row.names = F) # export to csv

