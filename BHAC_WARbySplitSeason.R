library(ggplot2)

###load all player stats

load.stats <- function(season) {
  
  data <- read.csv(paste0("~/Documents/CWA/Hockey Data/Hockey Analysis/SuperSkaterStats ",season,".csv"))

  ##tag season
  data <- cbind(season, data)
  colnames(data) <- c('Season.Split','Rank','LastName','FirstName','Team','Pos','GP','TOI','GF','GA','GF60','GA60','GFPct','TM.GF60','TM.GA60','TM.GFPct','GF60.RelTM','GA60.RelTM','GFPct.RelTM','GF60.Rel','GA60.Rel','GFPct.Rel','Opp.GF60','Opp.GA60','Opp.GFPct','SF','SA','ShPct','SvPct','PDO','TM.ShPct','TM.SvPct','ShPct.RelTM','SvPct.RelTM','ShPct.Rel','SvPct.Rel','Opp.ShPct','Opp.SvPct','CF','CA','CF60','CA60','CFPct','CF60.RelTM','CA60.RelTM','CFPct.RelTM','CF60.Rel','CA60.Rel','CFPct.Rel','Opp.CF60','Opp.CA60','Opp.CFPct','CShPct','CSvPct','CPDO','TM.CShPct','TM.CSvPct','CShPct.RelTM','CSvPct.RelTM','CShPct.Rel','CSvPct.Rel','Opp.CShPct','Opp.CSvPct','PctofTeam.TOI','PctofTeam.GF','PctofTeam.GA','PctofTeam.CF','PctofTeam.CA','PctofTeam.OZFO','PctofTeam.DZFO','PctofTeam.NZFO','G','A','FirstA','Pts','Pri.Pts','Shots','iCorsi','iShPct','iCShPct','Goals.per60','Assists.per60','FirstA.per60','Points.per60','Pri.Pts.per60','Shots.per60','iCorsi.per60','IGP','IAP','IPP','IPPP','TotFO','NZFO','DZFO','OZFO','NZFOPct','DZFOPct','OZFOPct')
  
  ##add metrics
  ###fix
  data$iFirstA.60 <- data$FirstA / (data$TOI / 60)
  data$TOI.Gm <- data$TOI / (data$GP / 2)
  
  ##pos
  data$Pos <- ifelse(data$Pos=="D","D",
                     ifelse(data$Pos=="F(C)","C",
                            ifelse(data$Pos=="F(LW)","L",
                                   ifelse(data$Pos=="F(RW)","R",
                            "W"))))
  
  ###unique & complete
  data <- data[complete.cases(data) & data$TOI > 100, ]
  print(dim(data))
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
  data$Predicted.Score <- (data$rf.predictions + data$lm.predictions) / 2
  
  data$Player.Name <- trimws(paste0(data$FirstName,".",data$LastName))
  
  return(data)

  }

player.seasons <- list("2015-16_Even","2014-15_Even","2013-14_Even","2012-13_Even","2011-12_Even","2010-11_Even","2009-10_Even","2008-09_Even","2007-08_Even",
                       "2015-16_Odd","2014-15_Odd","2013-14_Odd","2012-13_Odd","2011-12_Odd","2010-11_Odd","2009-10_Odd","2008-09_Odd","2007-08_Odd")

player.splits.scored <-  do.call(rbind,lapply(FUN=load.stats,player.seasons))

####collapse to player-season level
player.splits.scored$Season <- substr(player.splits.scored$Season.Split, 1, 7)

library(dplyr)
player.season.scored <- summarise(group_by(player.splits.scored, Player.Name, Season, Pos),
                                  rf.mean = mean(rf.predictions),
                                  rf.max = max(rf.predictions),
                                  rf.min = min(rf.predictions),
                                  lm.mean = mean(lm.predictions),
                                  lm.max = max(lm.predictions),
                                  lm.min = min(lm.predictions),
                                  TOI = sum(TOI))

player.season.scored$Predicted.Score <- (player.season.scored$rf.mean + player.season.scored$lm.mean) /2

player.15.scored <- subset(player.season.scored, Season == "2014-15" & TOI > 250)
player.16.scored <- subset(player.season.scored, Season == "2015-16" & TOI > 250)

names(player.15.scored)[names(player.15.scored)=="Predicted.Score"] <- "Predicted.Score.15"
names(player.16.scored)[names(player.16.scored)=="Predicted.Score"] <- "Predicted.Score.16"
names(player.15.scored)[names(player.15.scored)=="TOI"] <- "TOI.15"
names(player.16.scored)[names(player.16.scored)=="TOI"] <- "TOI.16"

player.season.compare <- merge(player.15.scored[c("Player.Name","Predicted.Score.15","TOI.15")],
                               player.16.scored[c("Player.Name","Predicted.Score.16","TOI.16")],
                               by = c("Player.Name"))

player.season.compare$Total.TOI <- player.season.compare$TOI.15 + player.season.compare$TOI.16

summary(lm(Predicted.Score.16 ~ Predicted.Score.15, 
                      data=player.season.compare, weights = Total.TOI))


###########################
####Year Over Year Repeatability
###########################

ha.16 <- read.csv("~/Documents/CWA/Hockey Abstract Data/NHL 2015-16.csv")
ha.15 <- read.csv("~/Documents/CWA/Hockey Abstract Data/NHL 2014-15.csv")

ha.15 <- ha.15[c("First.Name","Last.Name","GVT","TOI")]
ha.16 <- ha.16[c("First.Name","Last.Name","GVT","TOI")]

names(ha.15)[names(ha.15)=="GVT"] <- "GVT.15"
names(ha.16)[names(ha.16)=="GVT"] <- "GVT.16"
names(ha.15)[names(ha.15)=="TOI"] <- "TOI.15"
names(ha.16)[names(ha.16)=="TOI"] <- "TOI.16"

ha.15$Player.Name <- toupper(paste0(ha.15$First.Name,".",ha.15$Last.Name))
ha.16$Player.Name <- toupper(paste0(ha.16$First.Name,".",ha.16$Last.Name))

GVT.data <- merge(ha.15, ha.16, by="Player.Name")
GVT.data$Total.TOI <- (GVT.data$TOI.15 + GVT.data$TOI.16) / 60

GVT.data <- subset(GVT.data, TOI.15 > 250 & TOI.16 > 250)

###Line of Best Fit
gvt.fit <- summary(lm(GVT.16 ~ GVT.15, 
                      data=GVT.data, weights = Total.TOI))

gvt.corr <- cor(GVT.data$GVT.16,GVT.data$GVT.15)


gvt.plot <- (ggplot(data=GVT.data, aes(x=GVT.15, y=GVT.16)) + 
        geom_point(aes(color=Total.TOI)) +
        #geom_smooth(method = "lm", se = FALSE) +
        #theme(text = element_text(size=20)) +
          theme(panel.background = element_blank()) +
          theme(legend.position="none") + 
          labs(color="Total TOI") +
         labs(title="Goals Versus Threshold Repeatability") +
        labs(x="Goals Versus Threshold\n2014-15", y="Goals Versus Threshold\n2015-16") +
          annotate("segment", x = 0, y = 0, xend = 25, yend = 25) +
          scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green",
                                 midpoint = 2000, limits = c(0,4000)) +
        annotate("text", x = -2, y = 26, hjust=0,
                 label = paste0("YoY Correlation: ",
                                round(gvt.corr,3),
                                "\nData courtesy hockeyabstract.com")))
print(gvt.plot)


css.corr <- cor(player.season.compare$Predicted.Score.16,player.season.compare$Predicted.Score.15)

css.plot <- (ggplot(data=player.season.compare, aes(x=Predicted.Score.15, y=Predicted.Score.16)) + 
               geom_point(aes(color=Total.TOI)) +
              # geom_smooth(method = "lm", se = FALSE) +
              # theme(text = element_text(size=20)) +
               theme(legend.position = c(.9, .3)) + 
               theme(panel.background = element_blank()) +
               labs(color="Total TOI") +
              labs(title="Predicted Crowd-Sourced Rating Repeatability") +
               labs(x="Predicted Crowd-Sourced Rating\n2014-15", y="Predicted Crowd-Sourced Rating\n2015-16") +
               annotate("segment", x = 0, y = 0, xend = 100, yend = 100) +
               scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green",
                                      midpoint = 2000, limits = c(0,4000)) +
               annotate("text", x = 0, y = 95, hjust=0,
                        label = paste0("YoY Correlation: ",
                                       round(css.corr,3),
                                       "\nData courtesy puckalytics.com")))
print(css.plot)

  require(cowplot)
  plot_grid(gvt.plot, css.plot, align='h')
  


player.scored.odd <- player.splits.scored %>%
                        subset(substr(player.splits.scored$Season.Split, 9, 10) == "Od") %>%
                        dplyr::select(Player.Name, Season, Predicted.Score) %>%
                        plyr::rename(c('Predicted.Score'='Predicted.Score.Odd'))

player.scored.even <- player.splits.scored %>%
                        subset(substr(player.splits.scored$Season.Split, 9, 10) == "Ev") %>%
                        dplyr::select(Player.Name, Season, Predicted.Score) %>%
                        plyr::rename(c('Predicted.Score'='Predicted.Score.Even'))

intra.season <- merge(player.scored.even, player.scored.odd, by = c("Player.Name","Season"))

intra.corr <- cor(intra.season$Predicted.Score.Even,intra.season$Predicted.Score.Odd)

ggplot(data=intra.season, aes(x=Predicted.Score.Odd, y=Predicted.Score.Even)) + 
  geom_point() +
  facet_grid(. ~ Season) +
  geom_smooth(method = "lm", se = FALSE) +
  # theme(text = element_text(size=20)) +
  #theme(legend.position = c(.9, .3)) + 
  theme(panel.background = element_blank()) +
  #labs(color="Total TOI") +
  labs(title="Predicted Crowd-Sourced Rating Intraseason Repeatability") +
  labs(x="Predicted Crowd-Sourced Rating\nOdd", y="Predicted Crowd-Sourced Rating\nEven") +
  annotate("segment", x = 0, y = 0, xend = 100, yend = 100) 
  annotate("text", x = 0, y = -10, hjust=0,
           label = paste0("All-Season Correlation: ",
                          round(intra.corr,3),
                          "\nData courtesy puckalytics.com"))