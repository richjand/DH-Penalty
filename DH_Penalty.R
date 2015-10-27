library(readr)
library(dplyr)
library(stringr)
library(lfe)
library(sqldf)
library(ggplot2)
library(lubridate)


my_db <- src_sqlite("BRef/boxscores.sqlite")

wobas <- tbl(my_db, sql('SELECT year, h1b, h2b, h3b, hr, ubb, hbp, PA, ibb, position, teamID, game_id,
                        playerID, home_team, team_league FROM boxscores WHERE PA>=4')) %>%
  left_join(tbl(my_db,sql('SELECT year, w1B, w2B, w3B, wHR, wBB, wHBP FROM woba_weights')), by = 'year') %>%
  collect() %>%
  group_by(year) %>%
  mutate(woba = (h1b*w1B + h2b*w2B + h3b*w3B + hr*wHR + ubb*wBB + hbp*wHBP)/(PA - ibb),
         date = ymd(str_sub(game_id,4,11))) %>%
  ungroup() %>%
  group_by(playerID, year) %>%
  arrange(date) %>%
  mutate(sum1b = cumsum(h1b),
         sum2b = cumsum(h2b),
         sum3b = cumsum(h3b),
         sumhr = cumsum(hr),
         sumbb = cumsum(ubb),
         sumhbp = cumsum(hbp),
         sumpa = cumsum(PA),
         sumibb = cumsum(ibb),
         season_woba = (sum1b*w1B + sum2b*w2B + sum3b*w3B + sumhr*wHR + sumbb*wBB + sumhbp*wHBP)/(sumpa - sumibb),
         full_year_woba = (sum(h1b)*w1B + sum(h2b)*w2B + sum(h3b)*w3B + sum(hr)*wHR + sum(ubb)*wBB + sum(hbp)*wHBP)/(sum(PA)+sum(ibb))) %>%
  select(teamID, game_id, playerID, home_team, team_league, woba, year, position, season_woba, full_year_woba, date) %>%
  group_by(game_id) %>%
  mutate(interleague = ifelse('AL' %in% team_league & 'NL' %in% team_league,1,0)) %>%
  arrange(teamID, year, date) %>%
  ungroup()

  
##create lagged game variable##


wobas <- select(wobas, teamID, game_id, interleague, year) %>%
  distinct() %>%
  group_by(year, teamID) %>%
  mutate(last_team_game = lag(game_id, order_by=str_sub(game_id,4)), 
         last_team_game_interleague = lag(interleague, order_by=str_sub(game_id,4)),
         next_team_game = lead(game_id, order_by=str_sub(game_id,4)),
         next_team_game_interleague = lead(interleague, order_by=str_sub(game_id,4))) %>%
  ungroup() %>%
  select(teamID, game_id, last_team_game, last_team_game_interleague, next_team_game, next_team_game_interleague) %>%
  right_join(wobas, by = c('teamID', 'game_id'))

##Create home team interleague variable##
wobas <- wobas %>%
  select(game_id, home_team, team_league, interleague) %>%
  filter(home_team==1) %>%
  group_by(game_id) %>%
  distinct() %>% 
  mutate(home_team_league = team_league,
         interleague_al_home = ifelse(home_team_league == 'AL' & interleague == 1,1,0)) %>%
  ungroup() %>%
  select(game_id, home_team_league, interleague_al_home) %>%
  right_join(wobas, by = c('game_id'))

##Data set for difference in differences estimations##
fe_design <- wobas
fe_design$playeryear <- factor(str_c(fe_design$playerID, fe_design$year, sep = '-'))
fe_design$year <- factor(fe_design$year)
fe_design$playerID <- factor(fe_design$playerID)
fe_design$dh <- ifelse(fe_design$position=='DH',1,0)

fe1 <- summary(lm(woba ~ dh, data = fe_design)) ##NO FE##
fe2 <- summary(felm(woba ~ dh|playerID, data = fe_design)) ##Player FE##
fe3 <- summary(felm(woba ~ dh|playerID + year, data = fe_design)) ##add FE for year##
fe4 <- summary(felm(woba ~ dh|playerID + year + playeryear, data = fe_design, exactDOF = T)) ##add FE for player/year##
fe5 <- summary(felm(woba ~ dh|playerID + year + playeryear + game_id, data = fe_design, exactDOF = T)) ##add FE for game##

feplotdata <- data.frame(rbind(fe1$coef[2,1:2],
      fe2$coef[1:2],
      fe3$coef[1:2],
      fe4$coef[1:2],
      fe5$coef[1:2]), 
      fixed_effects = c('None','Player','Player, Year','Player, Year, PlayerYear','Player, Year, PlayerYear ,Game'))

feplotdata <- mutate(feplotdata,
       ci_low = Estimate - 1.96 * Std..Error,
       ci_high = Estimate + 1.96 * Std..Error)

feplotdata$fixed_effects <-factor(feplotdata$fixed_effects, levels=feplotdata[order(feplotdata$), "car"])

ggplot(feplotdata, aes(y = fixed_effects, x = Estimate)) + 
  geom_point(cex=2) + 
  geom_errorbarh(aes(xmin=ci_low, xmax = ci_high), height = 0) +
  geom_errorbarh(aes(xmin=Estimate-Std..Error, xmax = Estimate + Std..Error), height = 0, cex = 1.2) +
  geom_vline(xintercept = 0, linetype = 2) +
  xlab('Estimate of DH Effect') +
  ylab('Fixed Effects Included')
ggsave("baseball/panelestimates.pdf")
##Differences in differences, Comparison of days immediately before and after NL players play interleague games in AL parks, 
##some of whom end up playing DH##
##Create and merge pre and post treatment##
pretreat_nl <- filter(wobas, interleague == 0 & team_league == 'NL' & next_team_game_interleague == 1) %>%
  mutate(mergegame = next_team_game) %>%
  select(playerID, woba, mergegame) %>%
  rename(wobapre = woba)

posttreat_nl <- filter(wobas, interleague_al_home == 1 & team_league == 'NL' & last_team_game_interleague == 0) %>%
  mutate(mergegame = game_id,
         dh = ifelse(position=='DH',1,0)) %>%
  select(game_id, last_team_game, teamID, playerID, woba, position, mergegame,dh, year, season_woba, full_year_woba) %>%
  rename(wobapost = woba)

##Convert pre and post performance into differences##
diffs_nl <- inner_join(posttreat_nl, pretreat_nl, by = c('mergegame', 'playerID')) %>%
  mutate(d_woba = wobapost - wobapre) %>%
  select(game_id,last_team_game, teamID, playerID, d_woba, position, dh, year, season_woba, full_year_woba)

##AL PLAYERS##
pretreat_al <- filter(wobas, interleague == 1 & interleague_al_home == 0 & team_league == 'AL' & next_team_game_interleague == 0) %>%
  mutate(mergegame = next_team_game) %>%
  select(playerID, woba, mergegame) %>%
  rename(wobapre = woba)

posttreat_al <- filter(wobas, (interleague == 0|interleague==1 & interleague_al_home==1) & team_league == 'AL' & last_team_game_interleague == 1) %>%
  mutate(mergegame = game_id,
         dh = ifelse(position=='DH',1,0)) %>%
  select(game_id, last_team_game, teamID, playerID, woba, position, mergegame,dh, year, season_woba, full_year_woba) %>%
  rename(wobapost = woba)

##Convert pre and post performance into differences##
diffsal <- inner_join(posttreat_al, pretreat_al, by = c('mergegame', 'playerID')) %>%
  mutate(d_woba = wobapost - wobapre) %>%
  select(game_id,last_team_game, teamID, playerID, d_woba, position, dh, year, season_woba, full_year_woba)

##Combine AL and NL

alldiffs <- rbind(diffsal, diffs)
summary(lm(d_woba ~ dh, data = alldiffs))
summary(lm(d_woba ~ dh + factor(year), data = alldiffs))
head(summary(lm(d_woba ~ dh + factor(year) + factor(playerID), data = alldiffs))$coef)

##Create variable for consecutive games played##
alldiffs$consecutive_played <- 0
for (i in 1:nrow(alldiffs)){
  print(i)
  prev_game <- alldiffs$last_team_game[i]
  team <- alldiffs$teamID[i]
  consec <- 0
  ##While loop lookinig for previous games played
  while(alldiffs$playerID[i] %in% wobas$playerID[is.na(wobas$last_team_game)==F & wobas$game_id==prev_game & wobas$teamID==team]){
    consec <- consec + 1
    prev_game <- unique(wobas$last_team_game[is.na(wobas$last_team_game)==F & wobas$game_id==prev_game & wobas$teamID==team])
  }
  alldiffs$consecutive_played[i] <- consec
}

write.csv(alldiffs, "baseball/dhDID.csv", row.names = F)

##Match on full season woba and games played##
library(Matching)
pscore <- glm(dh ~ full_year_woba + consecutive_played, data = alldiffs, family = 'binomial')
match<- Match(Y=alldiffs$d_woba, Tr=alldiffs$dh, X=pscore$fitted, M=1,estimand="ATT", ties=TRUE, Weight=2)

ggplot(alldiffs, aes(x = d_woba, fill = factor(dh))) + 
  geom_density(alpha = .3) + 
  scale_fill_manual(values = c('red','green'), labels = c('Non-DH','DH')) +
  geom_rug() +
  ylab('Density') +
  xlab('wOBA Difference, t-1 to t') +
  theme(legend.title=element_blank())
ggsave('baseball/wobadifferences.pdf')

ks.test(alldiffs$d_woba[alldiffs$dh==1], alldiffs$d_woba[alldiffs$dh==0])
