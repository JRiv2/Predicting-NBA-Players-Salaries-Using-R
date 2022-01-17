library(readr)
library(ggplot2)
library(leaps)
library(dplyr)
library(car)
library(gridExtra)
library(GGally)

# Read in datasets
NBATrain <- read_csv('./NBATrain.csv')
NBATest <- read_csv('./NBATestNoY.csv')
dim(NBATrain)
dim(NBATest)

# NEW VARIABLES
NBATrain$GS. <- NBATrain$GS / NBATrain$G
NBATest$GS. <- NBATest$GS / NBATest$G

NBATrain$MPG <- NBATrain$MP / NBATrain$G
NBATest$MPG <- NBATest$MP / NBATest$G

NBATrain$TeamPayroll<- ifelse(NBATrain$TM == "MEM" |
                                NBATrain$TM == "BOS" |
                                NBATrain$TM == "SAS" |
                                NBATrain$TM == "OKC","High",
                              ifelse(NBATrain$TM == "HOU" |
                                       NBATrain$TM == "LAC" |
                                       NBATrain$TM == "CLE" |
                                       NBATrain$TM == "TOT" |
                                       NBATrain$TM == "MIL" |
                                       NBATrain$TM == "GSW" |
                                       #NBATrain$TM == "NOP" |
                                       NBATrain$TM == "DAL", "Medium", "Low"))
NBATest$TeamPayroll <-ifelse(NBATest$TM == "MEM" |
                               NBATest$TM == "BOS" |
                               NBATest$TM == "SAS" |
                               NBATest$TM == "OKC","High",
                             ifelse(NBATest$TM == "HOU" |
                                      NBATest$TM == "LAC" |
                                      NBATest$TM == "CLE" |
                                      NBATest$TM == "TOT" |
                                      NBATest$TM == "MIL" |
                                      NBATest$TM == "GSW" |
                                      #NBATest$TM == "NOP" |
                                      NBATest$TM == "DAL", "Medium", "Low"))

NBATrain$Position <- ifelse(NBATrain$Pos == "PG" |
                              NBATrain$Pos == "SG" |
                              NBATrain$Pos == "PG-SG", "Guard",
                            ifelse(NBATrain$Pos == "SF" |
                                     NBATrain$Pos == "SF-SG" 
                                   #| NBATrain$Pos == "PF"
                                   , "Forward", "Big"))
NBATest$Position <- ifelse(NBATest$Pos == "PG" |
                             NBATest$Pos == "SG" |
                             NBATest$Pos == "PG-SG", "Guard",
                           ifelse(NBATest$Pos == "SF" |
                                    NBATest$Pos == "SF-SG" 
                                  #| NBATest$Pos == "PF"
                                  , "Forward", "Big"))

NBATrain$AgeBracket <- ifelse(NBATrain$Age < 24, "<24",
                              ifelse(NBATrain$Age < 29, "24-28", 
                                     ifelse(NBATrain$Age < 37, "29-36", "37+")))
NBATest$AgeBracket <- ifelse(NBATest$Age < 24, "<24",
                             ifelse(NBATest$Age < 29, "24-28", 
                                    ifelse(NBATest$Age < 37, "29-36", "37+")))

NBATrain$AgeBracketSimp <- ifelse(NBATrain$Age < 24, "<24", #CURRENT
                                  ifelse(NBATrain$Age < 29, "24-28", "29+"))
NBATest$AgeBracketSimp <- ifelse(NBATest$Age < 24, "<24", #CURRENT
                                 ifelse(NBATest$Age < 29, "24-28", "29+"))

# TRANSFORMATIONS USING BOX-COX
#summary(powerTransform(cbind(Salary, insert_var_here)~1, data = NBATrain))
NBATrain$AgeT <- log(NBATrain$Age)
NBATest$AgeT <- log(NBATest$Age)

NBATrain$MPGT <- (NBATrain$MPG)^(3/4)
NBATrain$MPGT <- log(NBATrain$MPG)

NBATrain$TRB.T <- sqrt(NBATrain$TRB.)
NBATest$TRB.T <- sqrt(NBATest$TRB.)

NBATrain$USG.T <- sqrt(NBATrain$USG.)
NBATest$USG.T <- sqrt(NBATest$USG.)

NBATrain$AST.T <- (NBATrain$AST.)^(1/3)
NBATest$AST.T <- (NBATest$AST.)^(1/3)

NBATrain$FTA_T <- (NBATrain$FTA)^(1/2)
NBATest$FTA_T <- (NBATest$FTA)^(1/2)

NBATrain$VORP_T <- sqrt(NBATrain$VORP + 1.20000001)
NBATest$VORP_T <- sqrt(NBATest$VORP + 1.20000001)

NBATrain$BLK_T <- (NBATrain$BLK)^(1/3)
NBATest$BLK_T <- (NBATest$BLK)^(1/3)

NBATrain$Rk_T <- (NBATrain$Rk)^(3/4)
NBATest$Rk_T <- (NBATest$Rk)^(3/4)

NBATrain$USG._T <- (NBATrain$USG.)^(3/4)
NBATest$USG._T <- (NBATest$USG.)^(3/4)

NBATrain$AST_T <- (NBATrain$AST)^(1/3)
NBATest$AST_T <- (NBATest$AST)^(1/3)

NBATrain$STL._T <- (NBATrain$STL.)^(1/3)
NBATest$STL._T <- (NBATest$STL.)^(1/3)

NBATrain$DWS_T <- (NBATrain$DWS)^(1/3)
NBATest$DWS_T <- (NBATest$DWS)^(1/3)

NBATrain$TOV._T <- (NBATrain$TOV.)^(1/2) 
NBATest$TOV._T <- (NBATest$TOV.)^(1/2)

# Cross Validation Set-Up
smp_size <- floor(0.70 * nrow(NBATrain)) 
smp_size

#########################################################################################
# MODEL VERSIONS
#########################################################################################

# Model 1

model.1 <- lm(Salary~log(Age)+PTS+VORP+GS., data = NBATrain)
summary(model.1)
#diagPlot(model.1)
#mmps(model.1)
y.hat <- predict(model.1, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission1.csv")


# Model 2 - added more predictors

model.2 <- lm(Salary ~ log(Age)+PTS+VORP+GS.+MPG+WS.48 + Position:PTS, data = NBATrain)
summary(model.2)
#diagPlot(model.2)
#mmps(model.2)
y.hat <- predict(model.2, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission2.csv")


# Model 3 - added G, changed Position:PTS to just Position

model.3 <- lm(Salary ~ VORP + GS. + Age + PTS + Position + MPG + G, data = NBATrain)
summary(model.3)
#diagPlot(model.3)
#mmps(model.3)
y.hat <- predict(model.3, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission3.csv")


# Model 4 - tried log transformation on y variable, along with box-cox transformations on predictors
model.4 <- lm(log(Salary) ~ VORP + GS. + AgeT + PTS + Position + MPG + G + WS.48 
              + X3P + PER + USG.T + TRB.T + AST.T + FGA + FG, data = NBATrain)
summary(model.4)
#diagPlot(model.4)
#mmps(model.4)
y.hat <- predict(model.4, data = NBATrain, newdata = NBATest)
y.hat.exp <- exp(y.hat)
summary(y.hat.exp)
sub1 <- data.frame(1:180, y.hat.exp)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission4.csv")


# Model 5 - TEST BECAUSE LAST R^2 was HORRIBLE - doing the Letzi test shown in Office Hours
model.5 <- lm(log(Salary)~Age + AST, data = NBATrain)
summary(model.5)
#diagPlot(model.5)
#mmps(model.5)
y.hat <- predict(model.5, data = NBATrain, newdata = NBATest)
summary(y.hat)
y.hat.exp <- exp(y.hat)
summary(y.hat.exp)
sub1 <- data.frame(1:180, y.hat.exp)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "akram.csv")


# Model 6 - abandoned y transformation, added new variables

model.6 <- lm(Salary ~ VORP + GS. + AgeT + PTS + Position + MPG + G + WS.48 
              + X3P + PER + TRB. + BLK., data = NBATrain)
summary(model.6)
#diagPlot(model.6)
#mmps(model.6)
y.hat <- predict(model.6, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission6.csv")


# Model 7 - added new variables then did backwards AIC to make new model

starting_model <- lm(formula = Salary ~ VORP + GS. + Age + PTS + Position + MPG + G 
                     + ORB. + WS.48 + WS + TRB. + STL. + BLK. + OWS + DWS + OBPM + DBPM + PER
                     + FG + FGA + TRB + X3P, data = NBATrain)
#step(starting_model, direction = "backward", k = 2)

model.7 <- lm(formula = Salary ~ VORP + GS. + AgeT + PTS + Position + MPG + 
                G + TRB. + OBPM + WS.48 + X3P + PER + FG, data = NBATrain)
summary(model.7)
#diagPlot(model.7)
#mmps(model.7)
y.hat <- predict(model.7, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission7.csv")


# Model 8 - added teams as a predictor

model.8 <- lm(formula = Salary ~ VORP + GS. + AgeT + PTS + Position + MPG + 
                G + TRB. + OBPM + WS.48 + X3P + PER + FG + TM, data = NBATrain)
summary(model.8)
#diagPlot(model.8)
#mmps(model.8)
y.hat <- predict(model.8, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission8.csv")


# Model 9 - used predictors that had correlation with Salary > 0.2, then used Backward AIC

model.9.full <- lm(Salary~GS. + Position + MPG + DRB. + DBPM + FT + FTA + TS. + PER + 
                     OBPM + WS.48 + BPM + Ortg + VORP + OWS + WS + G + MP + DWS + GS +
                     AST. + AST + X2P + X2PA + FG + PTS + USG. + FGA + AgeT, data = NBATrain)
summary(model.9.full)
#step(model.9.full, direction = "backward")

model.9 <- lm(formula = Salary ~ GS. + Position + DBPM + FT + PER + OBPM + 
                WS.48 + BPM + VORP + G + MP + GS + X2P + FG + PTS + USG. + 
                AgeT + MPG, data = NBATrain)
summary(model.9)
#diagPlot(model.9)
#mmps(model.9)
y.hat <- predict(model.9, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission9.csv")


# Model 10 - Model 7 + PTS:AGEBracket
model.10 <- lm(Salary ~ VORP + GS. + AgeT + PTS + MPG + 
                 G + TRB. + OBPM + WS.48 + X3P + PER + FG + Position + PTS:AgeBracket, data = NBATrain)
summary(model.10)
#diagPlot(model.10)
#mmps(model.10)
y.hat <- predict(model.10, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission10.csv")


# Model 11 - more interactions

model.11 <- lm(Salary ~ VORP + GS. + AgeT + PTS + MPG + G + TRB. + OBPM + WS.48 + X3P + PER + FG + Position + PTS:AgeBracket + MPG:AgeBracket + VORP:AgeBracket, data = NBATrain)
summary(model.11)
#diagPlot(model.11)
#mmps(model.11)
y.hat <- predict(model.11, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission11.csv")


# Model 12 - another interaction

model.12 <- lm(Salary ~ VORP + GS. + AgeT + PTS + MPG + G + TRB. + OBPM + WS.48 + X3P + PER + FG + Position + PTS:AgeBracket + MPG:AgeBracket + VORP:AgeBracket + GS.:AgeBracket, data = NBATrain)
summary(model.12)
#diagPlot(model.12)
#mmps(model.12)
y.hat <- predict(model.12, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission12.csv")


# Model 13 - simplified model 12 using step function
model.13 <- lm(Salary ~ VORP + GS. + AgeT + PTS + MPG + G + WS.48 + X3P + PER + FG + Position + PTS:AgeBracket + MPG:AgeBracket + VORP:AgeBracket + GS.:AgeBracket, data = NBATrain)
summary(model.13)
#diagPlot(model.13)
#mmps(model.13)
anova(model.12, model.13)
y.hat <- predict(model.13, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission13.csv")


# Model 14 - replaced FG with FTA
model.14 <- lm(Salary ~ VORP + GS. + AgeT + PTS + MPG + G + WS.48 + X3P + PER + FTA + Position + PTS:AgeBracket + MPG:AgeBracket + VORP:AgeBracket + GS.:AgeBracket, data = NBATrain)
summary(model.14)
#diagPlot(model.14)
#mmps(model.14)
y.hat <- predict(model.14, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission14.csv")


# Model 15 - trying new predictors, used my comprehensive search code
model.15 <- lm(Salary ~ AgeT + Position + MPG + G + TS. + 
                 X3PAr + FTr + DRB. + TRB. + DBPM + VORP + Rk + FG + FGA + 
                 X3PA + X2P + X2PA + FT + FT. + DRB + TRB + PTS + GS. + PTS:AgeBracket, data = NBATrain)
summary(model.15)
#diagPlot(model.15)
#mmps(model.15)
y.hat <- predict(model.15, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission15.csv")


#Model 16 ADDED PAYROLL, used optimal_seed function for first time to compare
model.16 <- lm(Salary ~ AgeT + Position + MPG + G + TS. + 
                 X3PAr + FTr + DRB. + TRB. + DBPM + VORP + Rk + FG + FGA + 
                 X3PA + X2P + X2PA + FT + FT. + DRB + TRB + PTS + GS. + PTS:AgeBracket + TeamPayroll, data = NBATrain)
summary(model.16)
#diagPlot(model.16)
#mmps(model.16)
y.hat <- predict(model.16, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission16.csv")


# Model 17 add more GS.:AgeBracket - VORP:AgeBracket is bad
model.17 <- lm(Salary ~ AgeT + Position + MPG + GS. + TeamPayroll + VORP + TS. + G + 
                 X3PAr + FTr + DRB. + TRB. + DBPM + Rk + FG + FGA + 
                 X3PA + X2P + X2PA + FT + FT. + DRB + TRB + PTS + 
                 PTS:AgeBracket + GS.:AgeBracket, data = NBATrain)
summary(model.17)
#diagPlot(model.17)
#mmps(model.17)
y.hat <- predict(model.17, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission17.csv")


# Model 18 - switched out some predictors
model.18 <- lm(Salary ~ AgeT + Position + MPG + GS. + TeamPayroll + VORP + TS. + G +
                 X3PAr + FTr + TRB. + DBPM + Rk + FG + FGA + 
                 X3PA + X2P + X2PA + FT + FT. + TRB  + 
                 PTS:AgeBracket + GS.:AgeBracket + MPG:AgeBracket + Position:AST., data = NBATrain)
summary(model.18)
#diagPlot(model.18)
#mmps(model.18)
y.hat <- predict(model.18, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission18.csv")


# Model 19 - Relooked at single predictors - 
# went variable by variable to see how it affects R^2 for cross validation
model.19 <- lm(Salary ~ AgeT + GS. + Position + MPG + TeamPayroll + G + FTr + VORP + USG. + OWS 
               + FTA + FT. + STL + BLK + TOV
               +PTS:AgeBracket + GS.:AgeBracket + MPG:AgeBracket + Position:AST. + Position:STL., data = NBATrain)
summary(model.19)
#diagPlot(model.19)
#mmps(model.19)
y.hat <- predict(model.19, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission19.csv")


# Model 20 - adjusted Age brackets - before it was <25, 25-29, 30-34, 35+
model.20 <- lm(Salary ~ AgeT + GS. + Position + MPG + TeamPayroll + G + FTr + VORP + USG. + OWS 
               + FTA + FT. + STL + BLK + TOV
               +PTS:AgeBracket + GS.:AgeBracket + MPG:AgeBracket + Position:AST. + Position:FTr, data = NBATrain)
summary(model.20)
#diagPlot(model.20)
#mmps(model.20)
y.hat <- predict(model.20, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
#write.csv(sub1, "submission20.csv")


# Model 21 - took out FTr, added Position:FTr
model.21 <- lm(Salary ~ AgeT + GS. + Position + MPG + TeamPayroll + G + VORP + USG. + OWS 
               + FTA + FT. + STL + BLK + TOV
               +PTS:AgeBracket + GS.:AgeBracket + MPG:AgeBracket + Position:AST. + Position:FTr, data = NBATrain)
summary(model.21)
#diagPlot(model.21)
#mmps(model.21)
y.hat <- predict(model.21, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission21.csv")


# Model 22 - Bringing back the transformation
model.22 <- lm(log(Salary) ~ VORP + AgeBracket + MPG + FTA + Rk + TeamPayroll + 
                 WS.48 + X2P. + FT. + STL. + FTr + TS. +  OWS, data = NBATrain)
summary(model.22)
#diagPlot(model.22)
#mmps(model.22)
y.hat <- predict(model.22, data = NBATrain, newdata = NBATest)
y.hat.exp <- exp(y.hat)
sub1 <- data.frame(1:180, y.hat.exp)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat.exp)
#write.csv(sub1, "submission22.csv")


# Model 23 - add the interaction

model.23 <- lm(log(Salary) ~ VORP + AgeBracket + MPG + FTA + Rk + TeamPayroll + 
                 WS.48 + X2P. + FT. + STL. + FTr + TS. +  OWS + AgeBracket:MPG, data = NBATrain)
summary(model.23)
#diagPlot(model.23)
#mmps(model.23)
y.hat <- predict(model.23, data = NBATrain, newdata = NBATest)
y.hat.exp <- exp(y.hat)
sub1 <- data.frame(1:180, y.hat.exp)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat.exp)
#write.csv(sub1, "submission23.csv")


# Model 24 - back to 21 - took out some variables then used comp_search function

model.24 <- lm(Salary ~ PTS:AgeBracket + GS.:AgeBracket + MPG:AgeBracket + Position:AST. + Position:FTr 
               + FTA + VORP + TeamPayroll + BLK + Rk + USG. + FT. +  AST, data = NBATrain)
summary(model.24)
#diagPlot(model.24)
#mmps(model.24)
y.hat <- predict(model.24, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission24.csv")


# Model 25 - added Position:MPG to Model 24
model.25 <- lm(Salary ~ PTS:AgeBracket + GS.:AgeBracket + MPG:AgeBracket + Position:AST. + Position:FTr 
               + FTA + VORP + TeamPayroll + BLK + Rk + USG. + FT. +  AST + Position:MPG, data = NBATrain)
summary(model.25)
#diagPlot(model.25)
#mmps(model.25)
y.hat <- predict(model.25, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission25.csv")

# Model 26 - added TeamPayroll:AgeBracket
model.26 <- lm(Salary ~ PTS:AgeBracket + GS.:AgeBracket + MPG:AgeBracket + Position:AST. + Position:FTr 
               + FTA + VORP + TeamPayroll + BLK + Rk + USG. + FT. +  AST + Position:MPG + TeamPayroll:AgeBracket, data = NBATrain)
summary(model.26)
#diagPlot(model.26)
#mmps(model.26)
y.hat <- predict(model.26, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission26.csv")

# Model 27 - changed TeamPayroll:AgeBracket to TeamPayroll:AgeBracketSimp
model.27 <- lm(Salary ~ PTS:AgeBracket + GS.:AgeBracket + MPG:AgeBracket + Position:AST. + Position:FTr 
               + FTA + VORP + TeamPayroll + BLK + Rk + USG. + FT. + AST + Position:MPG + TeamPayroll:AgeBracketSimp, data = NBATrain)
summary(model.27)
#diagPlot(model.27)
#mmps(model.27)
y.hat <- predict(model.27, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission27.csv")


# Model 28 - backwards AIC
#step(model.27, direction = "backward")
model.28 <- lm(formula = Salary ~ FTA + VORP + TeamPayroll + BLK + Rk + USG. + 
                 AST + PTS:AgeBracket + AgeBracket:GS. + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp, 
               data = NBATrain)
summary(model.28)
#diagPlot(model.28)
#mmps(model.28)
y.hat <- predict(model.28, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission28.csv")


# Model 29 added another interaction, but too many betas now
model.29 <- lm(Salary ~ PTS:AgeBracket + GS.:AgeBracket + MPG:AgeBracket + Position:AST. + Position:FTr 
               + FTA + VORP + TeamPayroll + BLK + Rk + USG. + FT. + AST + Position:MPG + TeamPayroll:AgeBracketSimp + Position:OWS, data = NBATrain)
summary(model.29)
#diagPlot(model.29)
#mmps(model.29)

y.hat <- predict(model.29, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission29.csv")

# Model 30 - used comp_search() with #27
model.30 <- lm(Salary ~ FTA + VORP + TeamPayroll + BLK + Rk  + USG. +
                 AST + PTS:AgeBracket + AgeBracket:GS. + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp +STL. +  DWS + TOV. , data = NBATrain)
summary(model.30)
anova(model.30)
#diagPlot(model.30)
#mmps(model.30)
y.hat <- predict(model.30, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission30.csv")


# Model 31- used backwards AIC on 30
step(model.30, direction = "backward")
model.31 <- lm(Salary ~ FTA + VORP + TeamPayroll + BLK + Rk  + 
                 AST + PTS:AgeBracket + AgeBracket:GS. + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp +STL. +  TOV. , data = NBATrain)
summary(model.31)
anova(model.31)
#diagPlot(model.31)
#mmps(model.31)
y.hat <- predict(model.31, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission31.csv")


# Model 32- added more predictors to model 31 using comp_search

model.32 <- lm(Salary ~ FTA + VORP + TeamPayroll + BLK + Rk  + 
                 AST + PTS:AgeBracket + AgeBracket:GS. + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp +STL. +  TOV. +X3P. + FT., data = NBATrain)
summary(model.32)
anova(model.32)
#diagPlot(model.32)
#mmps(model.32)
y.hat <- predict(model.32, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission32.csv")


# Model 33 - went back to Model 30, tried transformations
model.33 <- lm(Salary ~ FTA + VORP + TeamPayroll + BLK + Rk  + USG._T +
                 AST_T + STL._T +  DWS + TOV._T + PTS:AgeBracket + AgeBracket:GS. + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp, data = NBATrain)
summary(model.33)
anova(model.33)
#diagPlot(model.33)
#mmps(model.33)
y.hat <- predict(model.33, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission33.csv")


# Model 34 - only difference is DWS_T instead of DWS - not as good

model.34 <- lm(Salary ~ FTA + VORP + TeamPayroll + BLK + Rk  + USG._T +
                 AST_T + STL._T +  DWS_T + TOV._T + PTS:AgeBracket + AgeBracket:GS. + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp, data = NBATrain)
summary(model.34)
anova(model.34)
#diagPlot(model.34)
#mmps(model.34)
y.hat <- predict(model.34, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission34.csv")


# Model 35 - changed Position brackets - now grouping PF with C
model.35 <- lm(Salary ~ FTA + VORP + TeamPayroll + BLK + Rk  + USG._T +
                 AST_T + STL._T +  DWS + TOV._T + PTS:AgeBracket + AgeBracket:GS. + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp, data = NBATrain)
summary(model.35)
anova(model.35)
#diagPlot(model.35)
#mmps(model.35)
y.hat <- predict(model.35, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission35.csv")


# Model 36 - took out interaction AgeBracket:GS. and made it just GS. - also tested transformation
model.36 <- lm(Salary ~ FTA + VORP + TeamPayroll + BLK + Rk  + USG._T +
                 AST_T + STL._T +  DWS + TOV._T + PTS:AgeBracket + GS. + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp, data = NBATrain)
summary(model.36)
anova(model.36)
#diagPlot(model.36)
#mmps(model.36)
y.hat <- predict(model.36, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission36.csv")


#Model 37 - added Position:WS then deleted DWS and USG._T, used backwards AIC to determine this
model.37 <- lm(Salary ~ TeamPayroll + GS. + FTA + VORP + BLK + Rk +
                 AST_T + STL._T + TOV._T + PTS:AgeBracket + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp + Position:WS, data = NBATrain)
summary(model.37)
anova(model.37)
#diagPlot(model.33)
#mmps(model.33)
y.hat <- predict(model.37, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission37.csv")


#Model 38 - removed bad leverage points (or some of them)
cutoff <- (2 * length(model.37$model)) / 420
leverage <- hatvalues(model.37)
standard_residuals <- rstandard(model.37) 
sum(leverage > cutoff & abs(standard_residuals) > 2)
bad <- unname(which(leverage > cutoff & abs(standard_residuals) > 2))
leverage[bad]
standard_residuals[bad]
NBATrain_BLRemoved <- NBATrain[-c(407, 310, 59, 260), ]

new_model <- lm(Salary ~ TeamPayroll + GS. + FTA + VORP + BLK + Rk +
                  AST_T + STL._T + TOV._T + PTS:AgeBracket + AgeBracket:MPG + 
                  Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp + Position:WS, data = NBATrain_BLRemoved)
summary(new_model)

model.38 <- lm(Salary ~ TeamPayroll + GS. + FTA + VORP + BLK + Rk +
                 AST_T + STL._T + TOV._T + PTS:AgeBracket + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp + Position:WS, data = NBATrain_BLRemoved)
#diagPlot(model.38)
#mmps(model.38)
summary(model.38)
anova(model.38)
y.hat <- predict(model.38, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission38.csv")

# Model 39
step(model.38, direction = "backward", k = log(416))
model.39 <- lm(Salary ~ TeamPayroll + GS. + FTA + VORP + BLK + Rk +
                 AST_T + STL._T + TOV._T + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp + Position:WS, data = NBATrain_BLRemoved)
summary(model.39)
anova(model.39)
#diagPlot(model.39)
#mmps(model.39)
y.hat <- predict(model.39, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission39.csv")


# Model 40 - new AgeBracket
NBATrain$AgeBracketAlt <- ifelse(NBATrain$Age < 23, "<24", #CURRENT
                                 ifelse(NBATrain$Age < 32, " 24-28", "Else"))
#ifelse(NBATrain$Age < 37, "29-36", "37+")))
NBATest$AgeBracketAlt <- ifelse(NBATest$Age < 23, "<24", #CURRENT
                                ifelse(NBATest$Age < 32, " 24-28", "Else"))
#ifelse(NBATest$Age < 37, "29-36", "37+")))
NBATrain_BLRemoved <- NBATrain[-c(407, 310, 59, 260), ]
model.40 <- lm(Salary ~ TeamPayroll + GS. + FTA + VORP + BLK + Rk +
                 AST_T + STL._T + TOV._T + PTS:AgeBracketAlt + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp + Position:WS, data = NBATrain_BLRemoved)
summary(model.40)
anova(model.40)
#diagPlot(model.40)
#mmps(model.40)
y.hat <- predict(model.40, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
write.csv(sub1, "submission40.csv")


# Model 41
NBATrain$AgeBracketAlt <- ifelse(NBATrain$Age < 23, "<24", #CURRENT
                                 ifelse(NBATrain$Age < 32, " 24-28", "Else"))
#ifelse(NBATrain$Age < 37, "29-36", "37+")))
NBATest$AgeBracketAlt <- ifelse(NBATest$Age < 23, "<24", #CURRENT
                                ifelse(NBATest$Age < 32, " 24-28", "Else"))
#ifelse(NBATest$Age < 37, "29-36", "37+")))
NBATrain_BLRemoved <- NBATrain[-c(407, 310, 59, 260), ]
model.41 <- lm(Salary ~ TeamPayroll + GS. + FTA + VORP + BLK + Rk + PF +
                 AST_T + STL._T + TOV._T + PTS:AgeBracketAlt + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp + Position:WS, data = NBATrain_BLRemoved)
summary(model.41)
anova(model.41)
#diagPlot(model.41)
#mmps(model.41)

y.hat <- predict(model.41, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission41.csv")


#Model 42
NBATrain$PF_T <- (NBATrain$PF)^(1/2)
NBATest$PF_T <- (NBATest$PF)^(1/2)
NBATrain_BLRemoved <- NBATrain[-c(407, 310, 59, 260), ]
#summary(powerTransform(cbind(Salary, PF_T)~1, data = NBATrain))
model.42 <- lm(Salary ~ TeamPayroll + GS. + FTA + VORP + BLK + Rk + PF_T +
                 AST_T + STL._T + TOV._T + PTS:AgeBracketAlt + AgeBracket:MPG + 
                 Position:AST. + Position:FTr + TeamPayroll:AgeBracketSimp + Position:WS, data = NBATrain_BLRemoved)
summary(model.42)
anova(model.42)
#diagPlot(model.42)
#mmps(model.42)
y.hat <- predict(model.42, data = NBATrain, newdata = NBATest)
sub1 <- data.frame(1:180, y.hat)
colnames(sub1)[1] <- "Ob"
colnames(sub1)[2] <- "Salary"
head(sub1)
summary(y.hat)
#write.csv(sub1, "submission42.csv")


#########################################################################################
# Functions and Other Code
#########################################################################################

# DiagPlot Code - taken from Professor's lecture notes
diagPlot<-function(model){
  p1<-ggplot(model, aes(model$fitted, model$residuals),label=rownames(NBATrain))+geom_point()
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals") 
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
  
  p2<-ggplot(model,aes(sample=rstandard(model))) + stat_qq() + stat_qq_line() 
  p2<-p2+xlab("Theoretical Quantiles")+ylab("Standardized Residuals") 
  p2<-p2+ggtitle("Normal Q-Q")
  
  p3<-ggplot(model, aes(model$fitted, sqrt(abs(rstandard(model)))))+geom_point(na.rm=TRUE)
  p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value") 
  p3<-p3+ylab(expression(sqrt("|Standardized residuals|"))) 
  p3<-p3+ggtitle("Scale-Location")+theme_bw()+geom_hline(yintercept=sqrt(2),
                                                         col="red", linetype="dashed")
  p4<-ggplot(model, aes(seq_along(cooks.distance(model)),
                        cooks.distance(model)))+geom_bar(stat="identity", position="identity")
  p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
  p4<-p4+ggtitle("Cook's distance")+theme_bw()+geom_hline(yintercept=4/(length(model$residuals-2)), 
                                                          col="red", linetype="dashed")
  p5<-ggplot(model, aes(hatvalues(model), rstandard(model)))+geom_point(aes(size=cooks.distance(model)),
                                                                        na.rm=TRUE)
  p5<-p5+stat_smooth(method="loess", na.rm=TRUE) 
  p5<-p5+xlab("Leverage")+ylab("Standardized Residuals") 
  p5<-p5+ggtitle("Residual vs Leverage Plot") 
  p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5)) 
  p5<-p5+theme_bw()+theme(legend.position="bottom")+geom_hline(yintercept=c(-2,2), col="red",
                                                               linetype="dashed")+geom_vline(xintercept=2*(length(model$model))/(length(model$residuals)),
                                                                                             col="blue", linetype="dashed")+ylim(-4,4) # 2*(p-1)/n
  
  p6<-ggplot(model, aes(hatvalues(model), cooks.distance(model)))+geom_point(na.rm=TRUE)+
    stat_smooth(method="loess", na.rm=TRUE)
  p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
  p6<-p6+ggtitle("Cook's dist vs Leverage") 
  p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed") 
  p6<-p6+theme_bw()
  return(grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3))
}


# MY FIND OPTIMAL SEED CODE - this function finds the optimal seed that will split the training data into
# training and testing subsets for cross validation, such that the resulting R^2 is as close as possible to Kaggle R^2
optimal_seed <- function(prev_mod, kaggle_r2) {
  differences <- numeric(1000)
  for(i in 1:1000) {
    set.seed(i)
    train_ind <- sample(seq_len(nrow(NBATrain)), size = smp_size)
    train <- NBATrain[train_ind, ] 
    test <- NBATrain[-train_ind, ]
    
    y.hat <- predict(prev_mod, data = train, newdata = test)
    comp <- lm(test$Salary~ y.hat)
    summary_comp <- summary(comp)
    r_2 <- summary_comp$r.squared
    differences[i] <- abs(kaggle_r2 - r_2)
  }
  which(differences == min(differences))
}


# MY COMPREHENSIVE SEARCH CODE - I can use this function to search through all predictors and add the
# one that will increase R^2 the most, using cross validation with the optimal seed

comp_search <- function(opt){ # opt is the optimal seed obtained from optimal_seed function
  preds <- c("AgeT", "GS.", "Position", "MPG", "AgeBracket", "TeamPayroll", "G", "MP", "PER", "TS.", "X3PAr", "FTr", "ORB.", "DRB.", "TRB.", "AST.", "STL.",
             "BLK.", "TOV.", "USG.", "OWS", "DWS", "WS", "WS.48", "OBPM", "DBPM", "BPM", "VORP", "Rk", "GS", "FG",
             "FGA", "FG.", "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "FT", "FTA", "FT.", "ORB", "DRB", "TRB", "AST",
             "STL", "BLK", "TOV", "PF", "PTS", "Ortg", "DRtg") # take out any that are already in the model
  
  added_preds <- "" # add existing model here
  best <- ""
  max <- 0
  cur_max <- 0
  while(TRUE) {
    for(i in 1:length(preds)){
      
      model <- eval(parse(text=paste("lm(Salary ~", added_preds, preds[i], ", data = NBATrain)",sep="")))
      
      set.seed(opt)
      train_ind <- sample(seq_len(nrow(NBATrain)), size = smp_size)
      train <- NBATrain[train_ind, ] 
      test <- NBATrain[-train_ind, ]
      
      y.hat <- predict(model, data = train, newdata = test)
      comp <- lm(test$Salary~ y.hat)
      
      R2 <- summary(comp)$r.squared
      cat(preds[i], ": ", R2, "\n")
      if(R2 > max) {
        max <- R2
        best <- preds[i]
      }
    }
    cat("Best:", best, "\nR^2 =", max, "\n")
    if (max - 0.003 > cur_max) {
      cur_max <- max
    }
    else {
      break
    }
    preds <- preds[preds != best]
    cat("lm(Salary ~", added_preds, best, ", data = NBATrain)\n")
    added_preds <- paste(added_preds, best, " + ", sep = "")
  }
}

