library(tidyverse)
library(rpart)
library(Metrics)
library(randomForest)
library(data.table)
library(mlr)
library(h2o)
library(DescTools)
library(rpart.plot)
library(missForest)
library(caret)
library(stringi)
library(stringr)

#determining pitcher level
fangraphs_season_level <- read_csv("fangraphs_season_level.csv")
starters <- fangraphs_season_level %>%
  select(PlayerId, MLBAMID, Name, NameASCII, Throws, Season, Age, Team, Role, G, GS, IP, TBF,
         BABIP, xFIP, SIERA, HardHit, HardHit_pct, FA_pct, SL_pct, CT_pct, CB_pct, CH_pct, 
         SF_pct, KN_pct, XX_pct, botStf, botCmd) %>%
  filter(Role == 'SP') %>%
  mutate(SIERA_level = case_when(
    SIERA <= 3 ~ 4,
    SIERA <= 3.5 ~ 3,
    SIERA <= 4 ~ 2,
    SIERA <= 4.5 ~ 1,
    TRUE ~ 0), 
    xFIP_level = case_when(
      xFIP <= 3 ~ 4,
      xFIP <= 3.5 ~ 3,
      xFIP <= 4 ~ 2,
      xFIP <= 4.5 ~ 2,
      TRUE ~ 0), 
    Stuff_level = case_when(
      botStf >= 65 ~ 4,
      botStf >= 55 ~ 3,
      botStf >= 45 ~ 2,
      botStf >= 35 ~ 1,
      TRUE ~ 0),
    Location_level = case_when(
      botCmd >= 70 ~ 4,
      botCmd >= 60 ~ 3,
      botCmd >= 50 ~ 2, 
      botCmd >= 40 ~ 1,
      TRUE ~ 0)
  ) %>%
  filter(IP > 100)

relievers <- fangraphs_season_level %>%
  select(PlayerId, MLBAMID, Name, NameASCII, Throws, Season, Age, Team, Role, G, GS, IP, TBF,
         BABIP, xFIP, SIERA, HardHit, HardHit_pct, FA_pct, SL_pct, CT_pct, CB_pct, CH_pct, 
         SF_pct, KN_pct, XX_pct, botStf, botCmd) %>%
  filter(Role == 'RP') %>%
  mutate(SIERA_level = case_when(
    SIERA <= 3 ~ 4,
    SIERA <= 3.5 ~ 3,
    SIERA <= 4 ~ 2,
    SIERA <= 4.5 ~ 1,
    TRUE ~ 0), 
    xFIP_level = case_when(
      xFIP <= 3 ~ 4,
      xFIP <= 3.5 ~ 3,
      xFIP <= 4 ~ 2,
      xFIP <= 4.5 ~ 1,
      TRUE ~ 0), 
    Stuff_level = case_when(
      botStf >= 70 ~ 4,
      botStf >= 60 ~ 3,
      botStf >= 50 ~ 2,
      botStf>= 40 ~ 1,
      TRUE ~ 0),
    Location_level = case_when(
      botCmd >= 70 ~ 4,
      botCmd >= 60 ~ 3,
      botCmd >= 50 ~ 2, 
      botCmd >= 40 ~ 1,
      TRUE ~ 0)) %>%
  filter(IP > 20)

#calculating career babip
starters_babip <- starters %>%
  group_by(PlayerId) %>%
  summarise(avg_babip = mean(BABIP)) %>%
  mutate(babip_level = case_when(
    avg_babip <= 0.295 ~ 4,
    avg_babip <= 0.300 ~ 3,
    avg_babip <= 0.305 ~ 2,
    avg_babip <= 0.310 ~ 1,
    TRUE ~ 0)
  )

relievers_babip <- relievers %>%
  group_by(PlayerId) %>%
  summarise(avg_babip = mean(BABIP)) %>%
  mutate(babip_level = case_when(
    avg_babip <= 0.295 ~ 4,
    avg_babip <= 0.300 ~ 3,
    avg_babip <= 0.305 ~ 2,
    avg_babip <= 0.310 ~ 1,
    TRUE ~ 0)
  )

#level calculations
sp_levels <- starters %>%
  group_by(PlayerId, MLBAMID, NameASCII) %>%
  summarise(avg_SIERA_level = mean(SIERA_level),
            avg_xFIP_level = mean(xFIP_level),
            avg_Stuff_level = mean(Stuff_level),
            avg_Location_level = mean(Location_level)
  ) %>%
  full_join(starters_babip, by = join_by(PlayerId == PlayerId)) %>%
  mutate(avg_level = case_when(
    (mean(avg_SIERA_level, 
          avg_xFIP_level,
          avg_Stuff_level,
          avg_Location_level)) > 3.5 ~ "Elite",
    (mean(avg_SIERA_level, 
          avg_xFIP_level,
          avg_Stuff_level,
          avg_Location_level)) > 2.5 ~ "High-Level",
    (mean(avg_SIERA_level, 
          avg_xFIP_level,
          avg_Stuff_level,
          avg_Location_level)) > 1.5 ~ "Good",
    (mean(avg_SIERA_level, 
          avg_xFIP_level,
          avg_Stuff_level,
          avg_Location_level)) > 0.5 ~ "Mediocre",
    TRUE ~ "Bad"
  ))

rp_levels <- relievers %>%
  group_by(PlayerId, MLBAMID, NameASCII) %>%
  summarise(avg_SIERA_level = mean(SIERA_level),
            avg_xFIP_level = mean(xFIP_level),
            avg_Stuff_level = mean(Stuff_level),
            avg_Location_level = mean(Location_level)
  ) %>%
  full_join(relievers_babip, by = join_by(PlayerId == PlayerId)) %>%
  mutate(avg_level = case_when(
    (mean(avg_SIERA_level, 
          avg_xFIP_level,
          avg_Stuff_level,
          avg_Location_level)) > 3.5 ~ "Elite",
    (mean(avg_SIERA_level, 
          avg_xFIP_level,
          avg_Stuff_level,
          avg_Location_level)) > 2.5 ~ "High-Level",
    (mean(avg_SIERA_level, 
          avg_xFIP_level,
          avg_Stuff_level,
          avg_Location_level)) > 1.5 ~ "Good",
    (mean(avg_SIERA_level, 
          avg_xFIP_level,
          avg_Stuff_level,
          avg_Location_level)) > 0.5 ~ "Mediocre",
    TRUE ~ "Bad"
  ))

#limiting data to only players it makes sense to move
sp_moveable <- sp_levels %>%
  filter(avg_level == "Good" | avg_level == "Mediocre" | avg_level == "Bad")

rp_moveable <- rp_levels %>%
  filter(avg_level == "Good" | avg_level == "Mediocre" | avg_level == "Bad")

#reliever -> starter prediction Random Forest Model
set.seed(123)

#data manipulation
sp_list <- fangraphs_season_level %>%
  select(PlayerId, MLBAMID, Name, NameASCII, Role, IP, Throws, Season, Age, Team, Role, G, GS, IP, TBF,
         BABIP, xFIP, SIERA, HardHit, HardHit_pct, FA_pct, SL_pct, CT_pct, CB_pct, CH_pct, 
         SF_pct, KN_pct, XX_pct, botStf, botCmd, botStf_CH, botStf_CU, botStf_FA, botStf_SI,
         botStf_SL, botStf_KC, botStf_FC, botStf_FS, K_pct, BB_pct, WHIP, BABIP, WAR, HR_per_9,
         H_per_9, EV, botxRV100) %>%
  filter(Role == 'SP') %>%
  filter(IP > 100) %>%
  mutate(botStf_CH = ifelse(is.na(botStf_CH), 0, botStf_CH)) %>%
  mutate(botStf_CU = ifelse(is.na(botStf_CU), 0, botStf_CU)) %>%
  mutate(botStf_FA = ifelse(is.na(botStf_FA), 0, botStf_FA)) %>%
  mutate(botStf_SI = ifelse(is.na(botStf_SI), 0, botStf_SI)) %>%
  mutate(botStf_SL = ifelse(is.na(botStf_SL), 0, botStf_SL)) %>%
  mutate(botStf_KC = ifelse(is.na(botStf_KC), 0, botStf_KC)) %>%
  mutate(botStf_FC = ifelse(is.na(botStf_FC), 0, botStf_FC)) %>%
  mutate(botStf_FS = ifelse(is.na(botStf_FS), 0, botStf_FS)) %>%
  group_by(PlayerId, MLBAMID, NameASCII) %>%
  summarise(avg_SIERA = mean(SIERA),
            avg_xFIP = mean(xFIP),
            avg_Stuff = mean(botStf),
            avg_Location = mean(botCmd),
            avg_CH = mean(botStf_CH),
            avg_CU = mean(botStf_CU),
            avg_FA = mean(botStf_FA),
            avg_SI = mean(botStf_SI),
            avg_SL = mean(botStf_SL),
            avg_KC = mean(botStf_KC),
            avg_FC = mean(botStf_FC),
            avg_FS = mean(botStf_FS),
            avg_Cmd = mean(botCmd),
            avg_K = mean(K_pct),
            avg_BB = mean(BB_pct),
            avg_WHIP = mean(WHIP),
            avg_BABIP = mean(BABIP),
            avg_WAR = mean(WAR),
            avg_HR9 = mean(HR_per_9),
            avg_H9 = mean(H_per_9),
            avg_EV = mean(EV),
            avg_xRV = mean(botxRV100))
sp_list <- replace(sp_list, is.na(sp_list), 0)

train_list <- createDataPartition(sp_list$PlayerId, p=(2/3), list=FALSE)
train_data <- sp_list[train_list,]
test_data <- sp_list[-train_list,]

model <- randomForest(
  formula = avg_SIERA ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC +
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + avg_xRV +
    avg_K + avg_BB + avg_Cmd + avg_WAR,
  data = train_data
)

model

which.min(model$mse) #192

sqrt(model$mse[which.min(model$mse)]) #0.229338

varImpPlot(model) 

preds <- train_data %>%
  select(avg_CH, avg_CU, avg_FA, avg_SI,
         avg_SL, avg_KC, avg_FC, avg_FS,
         avg_HR9, avg_H9, avg_WHIP, avg_BABIP,
         avg_EV, avg_xRV, avg_K, avg_BB, avg_Cmd,
         avg_WAR)

model_tuned <- tuneRF(
  x = preds, #define predictor variables
  y = train_data$avg_SIERA, #define response variable
  ntreeTry = 192,
  mtryStart = 18, 
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE #don't show real-time progress
)

#SIERA MODEL FOR PREDS
siera_model <- randomForest(
  formula = avg_SIERA ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC + 
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + 
    avg_xRV + avg_K + avg_BB + avg_Cmd + avg_WAR,
  data = train_data,
  ntree = 192,
  mtry = 12)

starter_classification_siera <- predict(siera_model, newdata = test_data)

sp_class_siera <- as.data.frame(starter_classification_siera)
sp_new_preds_siera <- cbind(sp_class_siera, test_data)

RMSE(starter_classification_siera, test_data$avg_SIERA) #0.1882878

#xFIP MODEL
model <- randomForest(
  formula = avg_xFIP ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC +
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + 
    avg_xRV + avg_K + avg_BB + avg_Cmd + avg_WAR,
  data = train_data
)

model

which.min(model$mse) #356

sqrt(model$mse[which.min(model$mse)]) #0.2890941

plot(model)

varImpPlot(model) 

preds <- train_data %>%
  select(avg_CH, avg_CU, avg_FA, avg_SI,
         avg_SL, avg_KC, avg_FC, avg_FS,
         avg_HR9, avg_H9, avg_WHIP, avg_BABIP,
         avg_EV, avg_xRV, avg_K, avg_BB, avg_Cmd,
         avg_WAR)

model_tuned <- tuneRF(
  x = preds, #define predictor variables
  y = train_data$avg_xFIP, #define response variable
  ntreeTry = 356,
  mtryStart = 18, 
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE #show real-time progress
)

#xFIP MODEL FOR PREDS
xfip_model <- randomForest(
  formula = avg_xFIP ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC + 
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + 
    avg_xRV + avg_K + avg_BB + avg_Cmd + avg_WAR,
  data = train_data,
  ntree = 356,
  mtry = 12)

starter_classification_xfip <- predict(xfip_model, newdata = test_data)

sp_class_xfip <- as.data.frame(starter_classification_xfip)
sp_new_preds_xfip <- cbind(sp_class_xfip, test_data)

RMSE(starter_classification_siera, test_data$avg_xFIP) #0.304386

model <- randomForest(
  formula = avg_WAR ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC +
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + 
    avg_xRV + avg_K + avg_BB + avg_Cmd,
  data = train_data
)

model

which.min(model$mse) #361
sqrt(model$mse[which.min(model$mse)]) #0.6455831
plot(model)

varImpPlot(model) 

preds <- train_data %>%
  select(avg_CH, avg_CU, avg_FA, avg_SI,
         avg_SL, avg_KC, avg_FC, avg_FS,
         avg_HR9, avg_H9, avg_WHIP, avg_BABIP,
         avg_EV, avg_xRV, avg_K, avg_BB, avg_Cmd)

model_tuned <- tuneRF(
  x = preds, #define predictor variables
  y = train_data$avg_WAR, #define response variable
  ntreeTry = 361,
  mtryStart = 17, 
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE #show real-time progress
)

#WAR MODEL FOR PREDS
war_model <- randomForest(
  formula = avg_WAR ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC + 
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + 
    avg_xRV + avg_K + avg_BB + avg_Cmd,
  data = train_data,
  ntree = 361,
  mtry = 12)

starter_classification_war <- predict(war_model, newdata = test_data)

sp_class_war <- as.data.frame(starter_classification_war)
sp_new_preds_war <- cbind(sp_class_war, test_data)

RMSE(starter_classification_war, test_data$avg_WAR) #0.617458

#creates difference and percent difference columns
sp_new_preds_siera <- sp_new_preds_siera %>%
  mutate(siera_diff = abs(starter_classification_siera - avg_SIERA),
         siera_pct_diff = (abs((starter_classification_siera - avg_SIERA) / 
                                 avg_SIERA) * 100))

sp_new_preds_xfip <- sp_new_preds_xfip %>%
  mutate(xfip_diff = abs(starter_classification_xfip - avg_xFIP),
         xfip_pct_diff = (abs((starter_classification_xfip - avg_xFIP) / 
                                avg_xFIP) * 100))

sp_new_preds_war <- sp_new_preds_war %>%
  mutate(war_diff = abs(starter_classification_war - avg_WAR),
         war_pct_diff = (abs((starter_classification_war - avg_WAR) / 
                               avg_WAR) * 100))

write_csv(sp_new_preds_siera, "rp_sp_siera_preds_final.csv")
write_csv(sp_new_preds_xfip, "rp_sp_xfip_preds_final.csv")
write_csv(sp_new_preds_war, "rp_sp_war_preds_final.csv")


#Pitcher Predictions -> AJ Puk & Tim Herrin
pitchers <- fangraphs_season_level %>%
  select(PlayerId, MLBAMID, Name, NameASCII, Role, Throws, Season, Age, Team, Role, G, GS, IP, TBF,
         BABIP, xFIP, SIERA, HardHit, HardHit_pct, FA_pct, SL_pct, CT_pct, CB_pct, CH_pct, 
         SF_pct, KN_pct, XX_pct, botStf, botCmd, botStf_CH, botStf_CU, botStf_FA, botStf_SI,
         botStf_SL, botStf_KC, botStf_FC, botStf_FS, K_pct, BB_pct, WHIP, BABIP, WAR, HR_per_9,
         H_per_9, EV, botxRV100) %>%
  mutate(botStf_CH = ifelse(is.na(botStf_CH), 0, botStf_CH)) %>%
  mutate(botStf_CU = ifelse(is.na(botStf_CU), 0, botStf_CU)) %>%
  mutate(botStf_FA = ifelse(is.na(botStf_FA), 0, botStf_FA)) %>%
  mutate(botStf_SI = ifelse(is.na(botStf_SI), 0, botStf_SI)) %>%
  mutate(botStf_SL = ifelse(is.na(botStf_SL), 0, botStf_SL)) %>%
  mutate(botStf_KC = ifelse(is.na(botStf_KC), 0, botStf_KC)) %>%
  mutate(botStf_FC = ifelse(is.na(botStf_FC), 0, botStf_FC)) %>%
  mutate(botStf_FS = ifelse(is.na(botStf_FS), 0, botStf_FS)) %>%
  group_by(PlayerId, MLBAMID, NameASCII) %>%
  summarise(avg_SIERA = mean(SIERA),
            avg_xFIP = mean(xFIP),
            avg_Stuff = mean(botStf),
            avg_Location = mean(botCmd),
            avg_CH = mean(botStf_CH),
            avg_CU = mean(botStf_CU),
            avg_FA = mean(botStf_FA),
            avg_SI = mean(botStf_SI),
            avg_SL = mean(botStf_SL),
            avg_KC = mean(botStf_KC),
            avg_FC = mean(botStf_FC),
            avg_FS = mean(botStf_FS),
            avg_Cmd = mean(botCmd),
            avg_K = mean(K_pct),
            avg_BB = mean(BB_pct),
            avg_WHIP = mean(WHIP),
            avg_BABIP = mean(BABIP),
            avg_WAR = mean(WAR),
            avg_HR9 = mean(HR_per_9),
            avg_H9 = mean(H_per_9),
            avg_EV = mean(EV),
            avg_xRV = mean(botxRV100)
  )

#AJ PUK
aj_puk <- pitchers %>%
  filter(NameASCII == "A.J. Puk")

puk_siera <- predict(siera_model, aj_puk) #3.76
puk_xfip <- predict(xfip_model, aj_puk) #3.79
puk_war <- predict(war_model, aj_puk) #2.86
#TIM HERRIN
tim_herrin <- pitchers %>%
  filter(NameASCII == "Tim Herrin")

herrin_siera <- predict(siera_model, tim_herrin) #4.07
herrin_xfip <- predict(xfip_model, tim_herrin) #4.13
herrin_war <- predict(war_model, tim_herrin) #2.53

#Starter -> Reliever Prediction Random Forest Models
rp_list <- fangraphs_season_level %>%
  select(PlayerId, MLBAMID, Name, NameASCII, Role, Throws, Season, Age, Team, Role, G, GS, IP, 
         TBF,BABIP, xFIP, SIERA, HardHit, HardHit_pct, FA_pct, SL_pct, CT_pct, CB_pct, CH_pct, 
         SF_pct, KN_pct, XX_pct, botStf, botCmd, botStf_CH, botStf_CU, botStf_FA, botStf_SI,
         botStf_SL, botStf_KC, botStf_FC, botStf_FS, K_pct, BB_pct, WHIP, BABIP, WAR, HR_per_9,
         H_per_9, EV, botxRV100) %>%
  filter(Role == 'RP') %>%
  filter(IP >= 20) %>%
  mutate(botStf_CH = ifelse(is.na(botStf_CH), 0, botStf_CH)) %>%
  mutate(botStf_CU = ifelse(is.na(botStf_CU), 0, botStf_CU)) %>%
  mutate(botStf_FA = ifelse(is.na(botStf_FA), 0, botStf_FA)) %>%
  mutate(botStf_SI = ifelse(is.na(botStf_SI), 0, botStf_SI)) %>%
  mutate(botStf_SL = ifelse(is.na(botStf_SL), 0, botStf_SL)) %>%
  mutate(botStf_KC = ifelse(is.na(botStf_KC), 0, botStf_KC)) %>%
  mutate(botStf_FC = ifelse(is.na(botStf_FC), 0, botStf_FC)) %>%
  mutate(botStf_FS = ifelse(is.na(botStf_FS), 0, botStf_FS)) %>%
  group_by(PlayerId, MLBAMID, NameASCII) %>%
  summarise(avg_SIERA = mean(SIERA),
            avg_xFIP = mean(xFIP),
            avg_CH = mean(botStf_CH),
            avg_CU = mean(botStf_CU),
            avg_FA = mean(botStf_FA),
            avg_SI = mean(botStf_SI),
            avg_SL = mean(botStf_SL),
            avg_KC = mean(botStf_KC),
            avg_FC = mean(botStf_FC),
            avg_FS = mean(botStf_FS),
            avg_Cmd = mean(botCmd),
            avg_K = mean(K_pct),
            avg_BB = mean(BB_pct),
            avg_WHIP = mean(WHIP),
            avg_BABIP = mean(BABIP),
            avg_WAR = mean(WAR),
            avg_HR9 = mean(HR_per_9),
            avg_H9 = mean(H_per_9),
            avg_EV = mean(EV),
            avg_xRV = mean(botxRV100)
  )
rp_list <- replace(rp_list, is.na(rp_list), 0) %>%
  mutate(across(c(6, 7, 8, 9, 10, 11, 12, 13), ~ . * 0.91))

train_list <- createDataPartition(rp_list$PlayerId, p=(2/3), list=FALSE)
train_data <- rp_list[train_list,]
test_data <- rp_list[-train_list,]

#SIERA Predictions
model <- randomForest(
  formula = avg_SIERA ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC +
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + avg_xRV +
    avg_K + avg_BB + avg_Cmd + avg_WAR,
  data = train_data
)

model

which.min(model$mse) #446

sqrt(model$mse[which.min(model$mse)]) #0.2857779

plot(model)

varImpPlot(model) 

preds <- train_data %>%
  select(avg_CH, avg_CU, avg_FA, avg_SI,
         avg_SL, avg_KC, avg_FC, avg_FS,
         avg_HR9, avg_H9, avg_WHIP, avg_BABIP,
         avg_EV, avg_xRV, avg_K, avg_BB, avg_Cmd,
         avg_WAR)

model_tuned <- tuneRF(
  x = preds, #define predictor variables
  y = train_data$avg_SIERA, #define response variable
  ntreeTry = 446,
  mtryStart = 18, 
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE #don't show real-time progress
)

#SIERA MODEL FOR PREDS
siera_model <- randomForest(
  formula = avg_SIERA ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC + 
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + 
    avg_xRV + avg_K + avg_BB + avg_Cmd + avg_WAR,
  data = train_data,
  ntree = 446,
  mtry = 18)

reliever_classification_siera <- predict(siera_model, newdata = test_data)

rp_class_siera <- as.data.frame(reliever_classification_siera)
rp_new_preds_siera <- cbind(rp_class_siera, test_data)

RMSE(reliever_classification_siera, test_data$avg_SIERA) #0.0.2519784

#xFIP Predictions
model <- randomForest(
  formula = avg_xFIP ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC +
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + 
    avg_xRV + avg_K + avg_BB + avg_Cmd + avg_WAR,
  data = train_data
)

model

which.min(model$mse) #455
sqrt(model$mse[which.min(model$mse)]) #0.421577

plot(model)

varImpPlot(model) 

preds <- train_data %>%
  select(avg_CH, avg_CU, avg_FA, avg_SI,
         avg_SL, avg_KC, avg_FC, avg_FS,
         avg_HR9, avg_H9, avg_WHIP, avg_BABIP,
         avg_EV, avg_xRV, avg_K, avg_BB, avg_Cmd,
         avg_WAR)

model_tuned <- tuneRF(
  x = preds, #define predictor variables
  y = train_data$avg_xFIP, #define response variable
  ntreeTry = 455,
  mtryStart = 18, 
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE #show real-time progress
)

#xFIP MODEL FOR PREDS
xfip_model <- randomForest(
  formula = avg_xFIP ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC + 
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + 
    avg_xRV + avg_K + avg_BB + avg_Cmd + avg_WAR,
  data = train_data,
  ntree = 455,
  mtry = 18)


reliever_classification_xfip <- predict(xfip_model, newdata = test_data)

rp_class_xfip <- as.data.frame(reliever_classification_xfip)
rp_new_preds_xfip <- cbind(rp_class_xfip, test_data)

RMSE(reliever_classification_xfip, test_data$avg_xFIP) #0.3523209

#WAR Model
model <- randomForest(
  formula = avg_WAR ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC +
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + 
    avg_xRV + avg_K + avg_BB + avg_Cmd,
  data = train_data
)

model

which.min(model$mse) #225

sqrt(model$mse[which.min(model$mse)]) #0.2567841
plot(model)

varImpPlot(model) 

preds <- train_data %>%
  select(avg_CH, avg_CU, avg_FA, avg_SI,
         avg_SL, avg_KC, avg_FC, avg_FS,
         avg_HR9, avg_H9, avg_WHIP, avg_BABIP,
         avg_EV, avg_xRV, avg_K, avg_BB, avg_Cmd)

model_tuned <- tuneRF(
  x = preds, #define predictor variables
  y = train_data$avg_WAR, #define response variable
  ntreeTry = 225,
  mtryStart = 17, 
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE #show real-time progress
)

#WAR MODEL FOR PREDS
war_model <- randomForest(
  formula = avg_WAR ~ avg_CH + avg_CU + avg_FA + avg_SI + avg_SL + avg_KC + 
    avg_FC + avg_FS + avg_HR9 + avg_H9 + avg_WHIP + avg_BABIP + avg_EV + 
    avg_xRV + avg_K + avg_BB + avg_Cmd,
  data = train_data,
  ntree = 225,
  mtry = 17)

reliever_classification_war <- predict(war_model, newdata = test_data)

rp_class_war <- as.data.frame(reliever_classification_war)
rp_new_preds_war <- cbind(rp_class_war, test_data)

RMSE(reliever_classification_war, test_data$avg_WAR) #0.2999529

rp_new_preds_siera <- rp_new_preds_siera %>%
  mutate(siera_diff = abs(reliever_classification_siera - avg_SIERA),
         siera_pct_diff = (abs((reliever_classification_siera - avg_SIERA) / 
                                 avg_SIERA) * 100))


rp_new_preds_xfip <- rp_new_preds_xfip %>%
  mutate(xfip_diff = abs(reliever_classification_xfip - avg_xFIP),
         xfip_pct_diff = (abs((reliever_classification_xfip - avg_xFIP) / 
                                avg_xFIP) * 100))

rp_new_preds_war <- rp_new_preds_war %>%
  mutate(war_diff = abs(reliever_classification_war - avg_WAR),
         war_pct_diff = (abs((reliever_classification_war - avg_WAR) / 
                               avg_WAR) * 100))

write_csv(rp_new_preds_siera, "sp_rp_siera_preds.csv")
write_csv(rp_new_preds_xfip, "sp_rp_xfip_preds.csv")
write_csv(rp_new_preds_war, "sp_rp_war_preds.csv")


# Model Accuracy Visuals

## RELIEVER TO STARTERS

rp_sp_siera_pred <- read.csv("rp_sp_siera_preds_final.csv")
rp_sp_xfip_pred <- read.csv("rp_sp_xfip_preds_final.csv")
rp_sp_war_pred <- read.csv("rp_sp_war_preds_final.csv")

par(mfrow = c(1,1))

boxplot(rp_sp_war_pred$war_diff,
        xlab = "WAR",
        ylab = "Difference",
        ylim = c(0,2.5),
        outpch = 16,
        outline = TRUE
)

## STARTERS TO RELIEVERS
sp_rp_siera_pred <- read.csv("sp_rp_siera_preds.csv")
sp_rp_xfip_pred <- read.csv("sp_rp_xfip_preds.csv")
sp_rp_war_pred <- read.csv("sp_rp_war_preds.csv")

par(mfrow = c(1,1))

boxplot(sp_rp_war_pred$war_diff,
        xlab = "WAR",
        ylab = "Difference",
        ylim = c(0,1.5),
        outpch = 16,
        outline = TRUE
)

library(tidyverse)
sp_levels <- read_csv("sp_moveable_final.csv")
rp_levels <- read_csv("rp_moveable.csv")
fangraphs_season_level <- read_csv("fangraphs_season_level.csv") %>%
  filter(ifelse(Role == "RP", IP > 20, Role == "SP" & IP > 100))

fg_starters <- fangraphs_season_level %>%
  filter(Role == "SP")  %>%
  select(NameASCII, MLBAMID, 252,253,255,256,258,259,261,262,264,265,267,268,270,271,273,274) %>%
  select_if(~!all(is.na(.))) %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  rowwise() %>%
  mutate(num_pitches = sum(c_across(3:18) != 0) / 2) %>%
  filter_all(~. != 20 & . != 80)

fg_relievers <- fangraphs_season_level %>%
  filter(Role == "RP")  %>%
  select(NameASCII, MLBAMID, 252,253,255,256,258,259,261,262,264,265,267,268,270,271,273,274) %>%
  select_if(~!all(is.na(.))) %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  rowwise() %>%
  mutate(num_pitches = sum(c_across(3:18) != 0) / 2) %>%
  filter_all(~. != 20 & . != 80)

#average_starter_stuff <- mean(starters$botStf)
#average_rp_stuff <- mean(relievers$botStf)

starters_merged <- merge(sp_levels, fg_starters)

# Reliever requirements:
# Need to throw at least 4 pitches
# Decrease of only 1 level
# Stuff adjustment
#stuff_adj_constant <- 1-(average_rp_stuff - average_starter_stuff)/average_rp_stuff

# Create model to classify starting pitchers based on Stuff and Command
sp_model_data <- starters_merged %>%
  select(-c(MLBAMID, NameASCII, ...1, avg_SIERA_level, avg_xFIP_level,
            avg_Stuff_level, avg_Location_level, avg_babip,
            PlayerId, babip_level))
sp_model_data$avg_level <- as.factor(sp_model_data$avg_level)


library(caret)
set.seed(123)
train_list <- createDataPartition(sp_model_data$avg_level, p=(2/3), list=FALSE)
train_data <- sp_model_data[train_list,]
test_data <- sp_model_data[-train_list,]

library(e1071)

sp_svm_model <- svm_model <- svm(avg_level ~ ., data = train_data, kernel = "radial")
svm_predictions <- predict(svm_model, newdata = test_data)
svm_confusion_matrix <- table(svm_predictions, test_data$avg_level)
sum(diag(svm_confusion_matrix)) / sum(svm_confusion_matrix)

library(xgboost)
xtrain_list <- createDataPartition(sp_model_data$avg_level, p=(2/3), list=FALSE)
xtrain_data <- sp_model_data[xtrain_list,]
xtest_data <- sp_model_data[-xtrain_list,]

sp_xgb_model <- train(
  avg_level ~ .,
  data = xtrain_data,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  verbose = FALSE,
  tuneGrid = expand.grid(
    nrounds = c(50, 100, 150),
    max_depth = c(3, 6, 9),
    eta = c(0.01, 0.1, 0.3),
    gamma = c(0, 1, 2),
    colsample_bytree = c(0.5, 0.7, 1),
    min_child_weight = c(1, 3, 5),
    subsample = c(0.8, 1)
  )
)

xgb_predictions <- predict(sp_xgb_model, newdata = xtest_data)
confusion_matrix_xgb <- confusionMatrix(xgb_predictions, xtest_data$avg_level)

accuracy_xgb <- confusion_matrix_xgb$overall["Accuracy"]
precision <- confusion_matrix_xgb$byClass["Precision"]
recall <- confusion_matrix_xgb$byClass["Recall"]
f1_score <- confusion_matrix_xgb$byClass["F1"]

confusion_matrix_xgb$overall
reliever_model_data <- fg_relievers %>%
  select(-c(NameASCII, MLBAMID)) %>%
  mutate(across(c(1, 3, 5, 7, 9, 11, 13, 15), ~ . * 0.91))

reliever_classification <- predict(sp_xgb_model, newdata = reliever_model_data)

rp_class <- as.data.frame(reliever_classification)
rp_new_preds <- cbind(rp_class, fg_relievers)

high_level_relievers <- rp_new_preds %>%
  filter(rp_class == "High-Level") %>%
  filter(num_pitches > 3)

high_level_good_relievers <- rp_new_preds %>%
  filter(rp_class == "High-Level" | rp_class == "Good") %>%
  filter(num_pitches > 3)

############## STARTER TO RELIEVER #################

relievers_merged <- merge(rp_levels, fg_relievers)

rp_model_data <- relievers_merged %>%
  select(-c(MLBAMID, NameASCII, avg_SIERA_level, avg_xFIP_level,
            avg_Stuff_level, avg_Location_level, avg_babip,
            PlayerId, babip_level))
rp_model_data$avg_level <- as.factor(rp_model_data$avg_level)


library(caret)
set.seed(123)
rp_train_list <- createDataPartition(rp_model_data$avg_level, p=(2/3), list=FALSE)
rp_train_data <- rp_model_data[rp_train_list,]
rp_test_data <- rp_model_data[-rp_train_list,]

rp_xgb_model <- train(
  avg_level ~ .,
  data = rp_train_data,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  verbose = FALSE,
  tuneGrid = expand.grid(
    nrounds = c(50, 100, 150),
    max_depth = c(3, 6, 9),
    eta = c(0.01, 0.1, 0.3),
    gamma = c(0, 1, 2),
    colsample_bytree = c(0.5, 0.7, 1),
    min_child_weight = c(1,  3, 5),
    subsample = c(0.8, 1)
  )
)

rp_xgb_predictions <- predict(rp_xgb_model, newdata = rp_test_data)
rp_confusion_matrix_xgb <- confusionMatrix(rp_xgb_predictions, rp_test_data$avg_level)

accuracy_xgb <- rp_confusion_matrix_xgb$overall["Accuracy"]

confusion_matrix_xgb$overall
sp_model_data <- fg_starters %>%
  select(-c(NameASCII, MLBAMID))

starter_classification <- predict(sp_xgb_model, newdata = sp_model_data)

sp_class <- as.data.frame(starter_classification)
sp_new_preds <- cbind(sp_class, fg_starters) %>%
  filter(sp_class == "Good")

library(ggplot2)
fangraphs_season_level <- read_csv("fangraphs_season_level.csv") %>%
  filter(ifelse(Role == "RP", IP > 20, Role == "SP" & IP > 100))

fg_starters <- fangraphs_season_level %>%
  filter(Role == "SP")

fg_relievers <- fangraphs_season_level %>%
  filter(Role == "RP")

###### starters, Stuff+ and WAR ######
fg_starters$Season <- as.factor(fg_starters$Season)

plot <- ggplot(fg_starters, aes(x = botStf, y = WAR, color = Season)) +
  geom_point(size = 3, alpha = 0.7) +
  
  labs(x = "Stuff Grade",
       y = "WAR") +
  
  ggtitle("WAR by Stuff Grade for Starters (min. 100 IP)") +
  
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  
  scale_color_manual(name = "Season", values = c("2021" = "blue", "2022" = "green", "2023" = "orange"))

lm_model <- lm(WAR ~ botStf, data = fg_starters)
r_squared <- summary(lm_model)$r.squared

plot + 
  geom_text(aes(x = max(fg_starters$botStf), y = max(fg_starters$WAR), 
                label = paste("R-squared =", round(r_squared, 3))),
            hjust = 0.8, vjust = -0.25, color = "black", size = 3)

###### Relievers, Stuff+ and WAR ######

fg_relievers$Season <- as.factor(fg_relievers$Season)

plot <- ggplot(fg_relievers, aes(x = botStf, y = WAR, color = Season)) +
  geom_point(size = 3, alpha = 0.7) +
  
  labs(x = "Stuff Grade",
       y = "WAR") +
  
  ggtitle("WAR by Stuff Grade for Relievers (min. 20 IP)") +
  
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  
  scale_color_manual(name = "Season", values = c("2021" = "blue", "2022" = "green", "2023" = "orange"))

lm_model <- lm(WAR ~ botStf, data = fg_relievers)
r_squared <- summary(lm_model)$r.squared

plot + 
  geom_text(aes(x = max(fg_relievers$botStf), y = max(fg_relievers$WAR), 
                label = paste("R-squared =", round(r_squared, 3))),
            hjust = 0.8, vjust = -0.25, color = "black", size = 3)

###### starters, Command+ and WAR ######
fg_starters$Season <- as.factor(fg_starters$Season)

plot <- ggplot(fg_starters, aes(x = botCmd, y = WAR, color = Season)) +
  geom_point(size = 3, alpha = 0.7) +
  
  labs(x = "Command Grade",
       y = "WAR") +
  
  ggtitle("WAR by Command Grade for Starters (min. 100 IP)") +
  
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  
  scale_color_manual(name = "Season", values = c("2021" = "blue", "2022" = "green", "2023" = "orange"))

lm_model <- lm(WAR ~ botCmd, data = fg_starters)
r_squared <- summary(lm_model)$r.squared

plot + 
  geom_text(aes(x = max(fg_starters$botCmd), y = max(fg_starters$WAR), 
                label = paste("R-squared =", round(r_squared, 3))),
            hjust = 0.8, vjust = -0.25, color = "black", size = 3)

###### Relievers, Command+ and WAR ######

fg_relievers$Season <- as.factor(fg_relievers$Season)

plot <- ggplot(fg_relievers, aes(x = botCmd, y = WAR, color = Season)) +
  geom_point(size = 3, alpha = 0.7) +
  
  labs(x = "Command Grade",
       y = "WAR") +
  
  ggtitle("WAR by Command Grade for Relievers (min. 20 IP)") +
  
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  
  scale_color_manual(name = "Season", values = c("2021" = "blue", "2022" = "green", "2023" = "orange"))

lm_model <- lm(WAR ~ botCmd, data = fg_relievers)
r_squared <- summary(lm_model)$r.squared

plot + 
  geom_text(aes(x = max(fg_relievers$botCmd), y = max(fg_relievers$WAR), 
                label = paste("R-squared =", round(r_squared, 3))),
            hjust = 0.8, vjust = -0.25, color = "black", size = 3)

###### starters, Stuff+ and swStr ######

fg_starters$Season <- as.factor(fg_starters$Season)

plot <- ggplot(fg_starters, aes(x = botStf, y = SwStr_pct, color = Season)) +
  geom_point(size = 3, alpha = 0.7) +
  
  labs(x = "Stuff Grade",
       y = "Whiff %") +
  
  ggtitle("Stuff Grade and Whiff% for starters (min. 100 IP)") +
  
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  
  scale_color_manual(name = "Season", values = c("2021" = "blue", "2022" = "green", "2023" = "orange"))

lm_model <- lm(SwStr_pct ~ botStf, data = fg_starters)
r_squared <- summary(lm_model)$r.squared

plot + 
  geom_text(aes(x = max(fg_starters$botStf), y = max(fg_starters$SwStr_pct), 
                label = paste("R-squared =", round(r_squared, 3))),
            hjust = 0.8, vjust = -0.25, color = "black", size = 3)

###### relievers, Stuff+ and swStr ######

fg_relievers$Season <- as.factor(fg_starters$Season)

plot <- ggplot(fg_relievers, aes(x = botStf, y = SwStr_pct, color = Season)) +
  geom_point(size = 3, alpha = 0.7) +
  
  labs(x = "Stuff Grade",
       y = "Whiff %") +
  
  ggtitle("Stuff Grade and Whiff% for Relievers (min. 20 IP)") +
  
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  
  scale_color_manual(name = "Season", values = c("2021" = "blue", "2022" = "green", "2023" = "orange"))

lm_model <- lm(SwStr_pct ~ botStf, data = fg_relievers)
r_squared <- summary(lm_model)$r.squared

plot + 
  geom_text(aes(x = max(fg_relievers$botStf), y = max(fg_relievers$SwStr_pct), 
                label = paste("R-squared =", round(r_squared, 3))),
            hjust = 0.8, vjust = -0.25, color = "black", size = 3)

