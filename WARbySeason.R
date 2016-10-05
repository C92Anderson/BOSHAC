library(ggplot2); library(dplyr); library(re)

###load all player stats

load.stats <- function(season) {
  
  library(dplyr)
  data <- read.csv(paste0("~/Documents/CWA/Hockey Data/Hockey Analysis/SuperSkaterStats ",season,".csv"))

  ##tag season
  data <- cbind(season, data)
  colnames(data) <- c('Season.Split','Rank','LastName','FirstName','Team','Pos','GP','TOI','GF','GA','GF60','GA60','GFPct','TM.GF60','TM.GA60','TM.GFPct','GF60.RelTM','GA60.RelTM','GFPct.RelTM','GF60.Rel','GA60.Rel','GFPct.Rel','Opp.GF60','Opp.GA60','Opp.GFPct','SF','SA','ShPct','SvPct','PDO','TM.ShPct','TM.SvPct','ShPct.RelTM','SvPct.RelTM','ShPct.Rel','SvPct.Rel','Opp.ShPct','Opp.SvPct','CF','CA','CF60','CA60','CFPct','CF60.RelTM','CA60.RelTM','CFPct.RelTM','CF60.Rel','CA60.Rel','CFPct.Rel','Opp.CF60','Opp.CA60','Opp.CFPct','CShPct','CSvPct','CPDO','TM.CShPct','TM.CSvPct','CShPct.RelTM','CSvPct.RelTM','CShPct.Rel','CSvPct.Rel','Opp.CShPct','Opp.CSvPct','PctofTeam.TOI','PctofTeam.GF','PctofTeam.GA','PctofTeam.CF','PctofTeam.CA','PctofTeam.OZFO','PctofTeam.DZFO','PctofTeam.NZFO','G','A','FirstA','Pts','Pri.Pts','Shots','iCorsi','iShPct','iCShPct','Goals.per60','Assists.per60','FirstA.per60','Points.per60','Pri.Pts.per60','Shots.per60','iCorsi.per60','IGP','IAP','IPP','IPPP','TotFO','NZFO','DZFO','OZFO','NZFOPct','DZFOPct','OZFOPct')
  
  ##add metrics
  data$iFirstA.60 <- data$FirstA / (data$TOI / 60)
  data$TOI.Gm <- data$TOI / (data$GP / 2)
  
  ##pos
  data$Pos <- ifelse(data$Pos=="D","D",
                     ifelse(data$Pos=="F(C)","C",
                            ifelse(data$Pos=="F(LW)","L",
                                   ifelse(data$Pos=="F(RW)","R",
                            "W"))))
  
  ##complete cases for model scoring
  data <- data[complete.cases(data), ] 
  
  ##########
  ##Load Player Scoring Models 
  #########
  
  load("~/Documents/CWA/Blog/Data/rf.model.rda")
  load("~/Documents/CWA/Blog/Data/lm.model.rda")
  rf.model.trained <- rf.model[[2]]
  lm.model.trained <- lm.model[[2]]
  
  ##########
  ##Scoring Season 
  #########  
  
  rf.predictions <- predict(rf.model.trained,data)
  lm.predictions <- predict(lm.model.trained,data)
  
  data <- cbind(rf.predictions, lm.predictions, data)
 
  return(data)

  }

player.seasons <- list("2015-16_Even","2014-15_Even","2013-14_Even","2012-13_Even","2011-12_Even","2010-11_Even","2009-10_Even","2008-09_Even","2007-08_Even",
                       "2015-16_Odd","2014-15_Odd","2013-14_Odd","2012-13_Odd","2011-12_Odd","2010-11_Odd","2009-10_Odd","2008-09_Odd","2007-08_Odd")

player.splits.scored <-  do.call(rbind,lapply(FUN=load.stats,player.seasons))


###Create Replacement 'Player' and Position Percentile Scores
player.seasons.wreplacement <- player.splits.scored %>%
         mutate(Player.Name = ifelse(TOI > 100, trimws(paste0(FirstName,".",LastName)), "Replacement"),
                Season = substr(Season.Split, 1, 7),
                Team = ifelse(Player.Name == "Replacement", "", as.character(Team))) %>%
                group_by(Player.Name, Team, Season, Pos) %>%
        summarise(rf.mean = mean(rf.predictions),
                           rf.max = max(rf.predictions),
                           rf.min = min(rf.predictions),
                           lm.mean = mean(lm.predictions),
                           lm.max = max(lm.predictions),
                           lm.min = min(lm.predictions),
                           TOI.Gm = sum(TOI) / mean(GP),
                           TOI = sum(TOI)) %>%
        mutate(Predicted.Score = (rf.mean * .8) + (lm.mean * .2)) %>%
        select(Player.Name, Season, Team, Pos, Predicted.Score, TOI.Gm, TOI) %>%
        group_by(Pos, Season) %>%
        mutate(Rank = rank(-Predicted.Score),
               Perc.Rank = rank(Predicted.Score)/length(Predicted.Score),
               Max.Score = max(Predicted.Score))

####replacement scores by position, season
replacement.season.pos <- player.seasons.wreplacement %>%
                          filter(Player.Name == "Replacement") %>%
                          mutate(Replacement.Rank = Perc.Rank,
                                 Replacement.Score = Predicted.Score,
                                 Replacement.Rank = Rank) %>%
                          select(Season, Pos, Replacement.Rank, Replacement.Score, Replacement.Rank) 
 

####index player season to replacement
player.seasons.vs.replacement <- player.seasons.wreplacement %>%
        inner_join(replacement.season.pos, by = c("Season", "Pos")) %>%
        mutate(SVT = Predicted.Score-Replacement.Score,
               Description = ifelse(Pos %in% c("L","R","C"),
                                    ifelse(Rank < (Replacement.Rank / 4),"1FWD",
                                    ifelse(Rank < (Replacement.Rank / 2),"2FWD",
                                    ifelse(Rank < (Replacement.Rank / 1.33),"3FWD",
                                    ifelse(Rank < Replacement.Rank,"4FWD","OtherFWD")))),
                             ifelse(Pos %in% c("D"),
                                    ifelse(Rank < 60,"1DEF",
                                    ifelse(Rank < 120,"2DEF",
                                    ifelse(Rank < 180,"3DEF","OtherDEF"))))))

#######
##function team, season
#######
team.season <- function(team, season) {
    
  team.season <- player.seasons.vs.replacement %>%
      filter(Season == season & Team == team) %>%
      arrange(Pos, -Predicted.Score)
  
  team.scores <- team.season %>%
                mutate(Pos2 = ifelse(Pos %in% c("L","R","C"),"F","D")) %>%
                group_by(Pos2) %>%
                summarise(weighted.mean(x=SVT, w=TOI.Gm))
  
  p <- ggplot(team.season, aes(x=reorder(-SVT, Player.Name), y=SVT, fill=Description, label=Player.Name)) + 
        geom_bar(stat='identity') +
        geom_text(data=team.season, aes(label = Player.Name, angle=270, size=TOI.Gm)) +
        labs(title=paste0("Team Skater Composition\n", team, ", ", season)) +
        labs(y="Indexed Ability Over Position Replacement", x="") +
        labs(fill="Player Descripton", size="TOI / GM") +
        annotate("text", x = length(team.season$Player.Name) - 8, y = (max(team.season$SVT) - 15), hjust = 0,
                 label = paste0("@CrowdScoutSprts\nData courtesy puckalytics.com &\ncrowdscoutsports.com\nTeam Ability Above Replacement\n(weighted by TOI/Gm):\nForward: ", 
                                round(team.scores[2,2],1),
                                "\nDefensemen: ", round(team.scores[1,2],1))) +
        theme(panel.background = element_blank(),axis.text.x=element_blank(),axis.ticks=element_blank())
  
  return(p)
}

team.season("Boston","2015-16")

########################################
#####test team scores against team success
########################################
team.season.fun <- function(season) {
  
  require(xlsx)
  data <- read.xlsx("~/Documents/CWA/Github/BOSHAC/Team-Season Results.xlsx",
                      sheetName=season)
    data$Season <- season  
  return(data) 
  
}

seasons <- list("2015-16","2014-15","2013-14","2012-13","2011-12","2010-11","2009-10","2008-09","2007-08")

team.season.results <- do.call(rbind,lapply(FUN=team.season.fun,seasons))

xwalk <- read.xlsx("~/Documents/CWA/Github/BOSHAC/Team-Season Results.xlsx",
                  sheetName="xwalk")

####team, season overall scores
best.teams <- player.seasons.vs.replacement %>%
  group_by(Season, Team) %>%
  summarise(mean.SVT = weighted.mean(x=SVT, w=TOI))

###clean team
team.season.results$Team.Clean <- as.factor(gsub("[*]","",team.season.results$Team))

###merge and aggregate by team, season
team.season.all <- team.season.results %>%
                      select(Team.Clean, Season, PTS., GF, GA) %>%
                       inner_join(xwalk, by = "Team.Clean") %>%
                       inner_join(best.teams, by = c("Team","Season")) %>%
                      mutate(GF.Pct = GF / (GF + GA))

###season correlations
season.corrs = group_by(team.season.all, Season) %>%
                dplyr::summarize(Correlation = cor(PTS., mean.SVT))

###plot Correlation
ggplot(season.corrs, aes(x=Season, y=Correlation)) +
    geom_bar(stat="identity") +
    labs(title="Points Share vs Team 'Skill' Correlations") +
    theme(panel.background = element_blank())
  

###plot Correlation by Season
ggplot(team.season.all, aes(x=mean.SVT, y=PTS.)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Points Share vs Team 'Skill' Correlations") +
  theme(panel.background = element_blank()) +
  labs(x="Indexed Ability Over Position Replacement", y="Points Percentage") + 
  facet_wrap(~Season)