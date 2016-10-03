library(randomForest); library(reshape2) ;library(ggplot2);library(dplyr)
library(caret); library(devtools); library(RMySQL); library(lubridate); library(gridExtra)

#player.elo <- dbGetQuery(conn, "SELECT Player, Elo, Score, game_count, c.player_name, c.Pos, c.team, c.dob
#                                  FROM `hockey_daily_elo_py` as a 
#                                  INNER JOIN 
#                                  (SELECT player_id, count(*) as game_count 
#                                  FROM nhl_all.hockey_elo_v1 GROUP BY player_id) as b 
#                                  ON a.ID=b.player_id
#                                  INNER JOIN `nhl_all`.`hockey_roster_v1` as c
#                                 ON a.ID=c.nhl_id
#                                  WHERE Date = CURRENT_DATE()")
player.elo <- read.csv("hockey_daily_elo_py.csv")

####################################
#######Prep CrowdScout Data
###################################

###create variables & clean
cs.data <- player.elo[which(player.elo$game_count > 50 & player.elo$dob !='NULL' & player.elo$Pos != "G"),]

###age start of 2015 season
cs.data$dob <- as.Date(cs.data$dob, format = "%Y-%m-%d")
cs.data$age15 <- as.numeric(difftime("2015-10-01", cs.data$dob, units="days") / 365.25)

###position variables
cs.data$Pos <- as.factor(cs.data$Pos) 

####name collapse
cs.data$name <- toupper(gsub(" ",".",cs.data$player_name))

###########################################
####Hockey Analysis Data
###########################################

###load all player stats

load.stats <- function(season) {
  
  data <- read.csv(paste0("~/Documents/CWA/Hockey Data/Hockey Analysis/SuperSkaterStats ",season,".csv"))
  
  data <- data[complete.cases(data[6]), ]
  
  return(cbind(season,data))
}

player.seasons <- c("2015-16_Even","2015-16_Odd") # only last season in analysis

player.stats <-  do.call(rbind,lapply(FUN=load.stats,player.seasons))

colnames(player.stats) <- c('Season','Rank','LastName','FirstName','Team','Pos','GP','TOI','GF','GA','GF60','GA60','GFPct','TM.GF60','TM.GA60','TM.GFPct','GF60.RelTM','GA60.RelTM','GFPct.RelTM','GF60.Rel','GA60.Rel','GFPct.Rel','Opp.GF60','Opp.GA60','Opp.GFPct','SF','SA','ShPct','SvPct','PDO','TM.ShPct','TM.SvPct','ShPct.RelTM','SvPct.RelTM','ShPct.Rel','SvPct.Rel','Opp.ShPct','Opp.SvPct','CF','CA','CF60','CA60','CFPct','CF60.RelTM','CA60.RelTM','CFPct.RelTM','CF60.Rel','CA60.Rel','CFPct.Rel','Opp.CF60','Opp.CA60','Opp.CFPct','CShPct','CSvPct','CPDO','TM.CShPct','TM.CSvPct','CShPct.RelTM','CSvPct.RelTM','CShPct.Rel','CSvPct.Rel','Opp.CShPct','Opp.CSvPct','PctofTeam.TOI','PctofTeam.GF','PctofTeam.GA','PctofTeam.CF','PctofTeam.CA','PctofTeam.OZFO','PctofTeam.DZFO','PctofTeam.NZFO','G','A','FirstA','Pts','Pri.Pts','Shots','iCorsi','iShPct','iCShPct','Goals.per60','Assists.per60','FirstA.per60','Points.per60','Pri.Pts.per60','Shots.per60','iCorsi.per60','IGP','IAP','IPP','IPPP','TotFO','NZFO','DZFO','OZFO','NZFOPct','DZFOPct','OZFOPct')

###edit names
player.stats$name <- trimws(toupper(paste0(player.stats$FirstName,".",player.stats$LastName)))

names(player.stats)[names(player.stats)=="Pos"] <- "Pos.HA"

###fix
player.stats$TOI.Gm <- (player.stats$TOI * 2) / player.stats$GP

#####combine player data
player.data <- merge(cs.data[, c("name", "Score", "Pos", "age15")], player.stats, by="name")

#####minimum 100 minutes 
player.data <- subset(player.data, TOI > 100)

###########################################
####Variable Selection
###########################################

linear.combos <- findLinearCombos(player.data[,c(11:ncol(player.data))])
print(linear.combos$remove)
player.data <- player.data[-c(linear.combos$remove)]

######################
##automatically select features using Caret R PackageR
######################

recursive.feature.elimin <- function(data, model.vars) {
      # ensure the results are repeatable
      set.seed(7)
      # load the library
      library(mlbench); library(caret)
      # load the data
      data <- data[model.vars]

      # define the control using a random forest selection function
      control <- rfeControl(functions=rfFuncs, method="cv", number=10)
      # run the RFE algorithm
      results <- rfe(data[3:ncol(data)], data[,2], sizes=c(3:ncol(data)), rfeControl=control)
      # summarize the results
      print(results)
      # list the chosen features
      predictors(results)
      # plot the results
      return(results)
  }

model.vars.rfe <- c(names(player.data[,c(11:ncol(player.data))]))

rate.vars <- c("GP","GF60",
               "GA60","GFPct","TM.GF60","TM.GA60","TM.GFPct",
               "GF60.RelTM","GA60.RelTM","GFPct.RelTM","GA60.Rel","GFPct.Rel",  
               "Opp.GF60","Opp.GA60","Opp.GFPct",  
               "ShPct","SvPct","PDO","TM.ShPct","TM.SvPct",
               "ShPct.RelTM","SvPct.RelTM","ShPct.Rel","SvPct.Rel","Opp.ShPct",   
               "Opp.SvPct","CF60","CA60",  
               "CFPct","CF60.RelTM","CFPct.RelTM","CF60.Rel","CA60.Rel",   
               "CFPct.Rel","Opp.CF60","Opp.CA60","Opp.CFPct","CShPct",  
               "CSvPct","CPDO","TM.CShPct","TM.CSvPct","CShPct.RelTM",  
               "CSvPct.RelTM","CShPct.Rel","CSvPct.Rel","Opp.CShPct","Opp.CSvPct",    
               "PctofTeam.TOI","PctofTeam.CF","PctofTeam.CA","PctofTeam.OZFO","PctofTeam.DZFO",
               "PctofTeam.NZFO","iShPct","iCShPct", 
               "Goals.per60","Assists.per60","FirstA.per60","Points.per60","Shots.per60", 
               "iCorsi.per60","IGP","IAP","IPP","IPPP",  
               "DZFOPct","OZFOPct","TOI.Gm")

rfe <- recursive.feature.elimin(player.data,rate.vars)

ggplot(rfe, type=c("g", "o")) +
  labs(title="Recursive Feature Elimination\nPlayer Metrics Predicting CrowdScout Score") +
  labs(x="Number of Variables", y="RMSE (Cross-Validation") +
   theme(panel.background = element_blank())

##########################################
####Linear Model Cross-Validation
##########################################

lm.cross.v10 <- function(input, model.vars) {
  
  library(caret)
 
  # load the dataset
  input
  input.cc <- input[ ,model.vars]
 
  # define training control
  set.seed(1234)
  train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
  
  # train the model
  model.output <- train( Score ~ . , data=input.cc[2:ncol(input.cc)], trControl=train_control, method="glm", 
                          tuneLength=10)
  
  # predict
  predictions <- predict(model.output, newdata=input)
  
  # var importance
  var.imp <- varImp(model.output)
  
  return(list(cbind(predictions,input.cc),model.output,var.imp))
  
}

###only significant features
model.vars <- c('TOI.Gm',
                 'CF60','CA60',
                'CFPct.RelTM',
                'OZFOPct',
                'FirstA.per60','Goals.per60')

################
####GLM Skaters
################
lm.model <- lm.cross.v10(player.data, c("name","Score","Pos",model.vars))

lm.scored <- lm.model[[1]]
lm.scored$Residuals <- lm.scored$predictions - lm.scored$Score

####output coefficients
lm.model[[2]]$results$RMSE
# 13.29134
lm.model[[2]]$results$Rsquared
# 0.6040395

fm = lm.model[[2]]$finalModel
anova(fm, test="Chisq")

###output sumamry
summary(lm.model[[2]])
lm.model[[2]]
###RMSE      Rsquared 
###13.29134  0.6040395

###plot variable importance
ggplot(data=lm.model[[3]]) +
    labs(title="Variable Importance\nGeneralized Linear Model") +
  theme(panel.background = element_blank())

lm.scored$lastname = sapply(strsplit(as.character(lm.scored$name), '[.]'), function(x) x[length(x)])

###plot
ggplot(data=lm.scored, aes(x=Score,y=predictions, color=Residuals)) + 
  geom_text(aes(label=lastname), angle=-45, check_overlap = TRUE) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Predicted vs Actual Crowd-Sourced Rating\nGeneralized Linear Model, Even/Odd Seasons 2015-16") +
  labs(x="Actual Crowd-Sourced Rating", y="Predicted Crowd-Sourced Rating") +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green") +
  theme(legend.position = c(0.9, .3)) + 
  annotate("segment",x=0,y=0,xend=100,yend=100) +
  #annotate("text", x = 70, y = 5 , hjust=0, label = "@CrowdScoutSprts") +
  annotate("text", x = 0, y = 80 , hjust=0, label = paste("Fit Metrics: \nRMSE: ", 
                                  round(as.numeric(lm.model[[2]]$results$RMSE), 3),
                                  "\nR2: ", round(as.numeric(lm.model[[2]]$results$Rsquared), 3))) 

####################
##RANDOM FOREST
####################

####Random Forest Model
rf.cross.v10 <- function(input, model.vars, label.vars) {
  
  library(caret)
  # load the dataset
  input
  input.cc <- input[names(input) %in% model.vars]
  input.cc <- input.cc[complete.cases(input.cc),]
  input.model <- input.cc[!(names(input.cc) %in% c("name"))]
  

  
  # define training control
  set.seed(1234)

  train_control <- trainControl(method="cv", number=5)
  # train the model
  model <- train( Score ~ . ,
                  data=input.model, trControl=train_control, method="rf", tuneLength=6, ntree=500, importance = TRUE)
  
  predictions <- predict(model, newdata=input.cc)
  
  # var importance
  var.imp <- varImp(model, scale=FALSE)

  #par(op)
  return(list(cbind(predictions,input.cc),model, var.imp))
  
}

####################################
######Skater Random Forest
####################################

rf.model <- rf.cross.v10(player.data,c("Score","name","Pos",model.vars))

###output dataset
rf.scored <- rf.model[[1]]
rf.scored$Residuals <- rf.scored$predictions - rf.scored$Score

####output metrics, explaining variation, not variance
rf.model[[2]]$results$RMSE
##13.35776 12.92346 12.68816 12.56338 12.56050 12.44313
rf.model[[2]]$results$Rsquared
##0.6255687 0.6381313 0.6453466 0.6500478 0.6494041 0.6554105

rf.scored$lastname <- sapply(strsplit(as.character(rf.scored$name), '[.]'), function(x) x[length(x)])

lm <- summary(lm(predictions~Score,data=rf.scored))

cor(rf.scored$predictions,rf.scored$Score)

###plot
rf.scored.plot <- ggplot(data=rf.scored, aes(x=Score,y=predictions, color=Residuals, label=lastname)) + 
        geom_text(aes(label=lastname), angle=-45, check_overlap = TRUE) +
        #geom_text_repel( data = subset(rf.scored, abs(Residuals) > 10),aes(label = lastname)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        #theme(text = element_text(size=20)) +
        theme(legend.position = c(0.9, .4)) + 
        theme(panel.background = element_blank()) +
        #labs(shape="Position") +
        labs(title="Predicted vs Actual Crowd-Sourced Ratings\nRandom Forest Model, Even/Odd Seasons 2015-16") +
        labs(x="Actual Crowd-Sourced Rating", y="Predicted Crowd-Sourced Rating") +
        scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green",
                               midpoint = 0, limits = c(-20, 20)) +
        annotate("segment",x=0,y=0,xend=100,yend=100) +
        annotate("text", x = 0, y = 80 , hjust=0, label = paste("R-squared: ",
                                          round(head(rf.model[[2]]$results$Rsquared,1), 3),
                                          "\nData courtesy\npuckalytics.com"))

###plot variable importance
rf.varimp <- ggplot(data=rf.model[[3]]) +
  labs(title="", x="", y="Variable Importance") +
  theme(text = element_text(size=10), axis.ticks=element_blank(),plot.title = element_text(size=10),
        panel.background=element_blank(),axis.text.x=element_blank(),axis.line=element_blank(),
        axis.text.y=element_text(size=10)) 

print(rf.scored.plot)
print(rf.varimp, vp=viewport(0.2, .6, .3, .5))


#############################################
##Save Models
#############################################

###save models
save(rf.model,file="rf.model.rda")
###save models
save(lm.model,file="lm.model.rda")


#############################################
##Check Model Residual Relationship
#############################################

player.resids <- cbind(lm.scored[c("lastname","Residuals","Score","Pos")],rf.scored[c("Residuals")])
colnames(player.resids) <- c("name","GLM.Residuals","Score","Pos","RF.Residuals")

resid.fit <- lm(GLM.Residuals ~ RF.Residuals,data=player.resids)
predict <- predict(resid.fit,data=player.resids)
resid.data <- cbind(predict,player.resids)

###plot
ggplot(data=player.resids, aes(x=RF.Residuals,y=GLM.Residuals, color=Score, label=name)) + 
  geom_text(aes(label=name), angle=0, check_overlap = TRUE) +
  geom_point(aes(shape = factor(Pos))) +
  geom_smooth(method = "lm", se = FALSE) +
  #theme(text = element_text(size=20)) +
  labs(shape="Position") +
  labs(title="GLM vs. RF Model Residual Compare\nPlayer-Level Residuals") +
  labs(x="Random Forest Model Residuals", y="Generalized Linear Model Model Residuals") +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green",midpoint = 50, limits = c(0, 100)) +
  annotate("segment",x=-25,y=-25,xend=25,yend=25) +
  annotate("text", x = -22, y = 45, hjust=0, label = "@CrowdScoutSprts") +
  annotate("text", x = -22, y = 35, hjust=0,
           label = paste0("y = ", round(resid.fit$coefficients[1],2), " + ",
                                  round(resid.fit$coefficients[2],2), " x\nR-squared = ",
                                  round(summary(resid.fit)$r.squared,2)))