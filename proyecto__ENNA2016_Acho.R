rm(list=ls());
install.packages("mice")

library("haven");
library("dplyr");
library(tidyverse)
library(caret)
library(e1071)
library(class)
library(labelled)
library(mice)
library(caret)
library(pROC)

  getwd();

trabajo_ninos = read_sav("ENNA_2016/ENNA_2016.sav");

db_ninos = trabajo_ninos %>% select(depto, area, ns001a_02, ns001a_03, ns01a_01, ns01a_02a, ns01a_03, ns02a_01, ns02a_02, ns02a_03, ns02a_07, ns02b_11a1, ns02b_11a2, ns02b_14, ns02b_14a)
db_ninos = db_ninos %>% mutate(y = ns02a_01==1 | ns02a_07 ==1 | !is.na(ns02b_11a1) | !is.na(ns02b_14) | !is.na(ns02b_14a))
db_ninos = db_ninos %>% mutate(y = !is.na(y))
db_ninos = db_ninos %>% select(-ns02a_01, -ns02a_07, -ns02b_11a1, -ns02b_11a2, -ns02b_14, -ns02b_14a)
# dividimos el data frame en dos distintas partes, asi podemos probar los modelos que generamos


split_df <- split(db_ninos, rep(1:2, length.out = nrow(db_ninos)))
df_train <- split_df[[1]];
df_test <- split_df[[2]];

categorical_vars <- c("depto", "area", "ns001a_02", "ns01a_01", "ns01a_02a", "ns01a_03", "ns02a_02","ns02a_03");
df_train[categorical_vars] <- lapply(df_train[categorical_vars], as_factor)
df_train[categorical_vars] <- lapply(df_train[categorical_vars], as.factor)

imputed_data <- mice(df_train, m = 1, method = 'pmm', maxit = 5)
df_imputed <- complete(imputed_data)
imputed_data2 <- mice(df_test,m=1, method = 'pmm', maxit = 5)
df_imputed_2 <- complete(imputed_data2)

df_test[categorical_vars] <- lapply(df_test[categorical_vars], as_factor)
df_test[categorical_vars] <- lapply(df_test[categorical_vars], as.factor)

preProcValues <- preProcess(df_train["ns001a_03"], method = c("center", "scale"))
df_train["ns001a_03"] <- predict(preProcValues, df_train["ns001a_03"])

preProcValues <- preProcess(df_test["ns001a_03"], method = c("center", "scale"))
df_test["ns001a_03"] <- predict(preProcValues, df_test["ns001a_03"])

df_tes <- df_test %>% filter(ns01a_02a != '32.SECUNDARIA (1 A 4 AÑOS)') %>% filter(ns02a_02!= '6.¿Dar servicios a otras personas por dinero o pago en espec')


logit_model <- glm(y ~ ., data = df_imputed, family = binomial)
logit_pred <- predict(logit_model, newdata = df_tes, type = "response")
logit_pred_class <- ifelse(logit_pred > 0.5, TRUE, FALSE)


probit_model <- glm(y ~ ., data = df_imputed, family = binomial(link = "probit"))
probit_pred <- predict(probit_model, newdata = df_tes, type = "response")
probit_pred_class <- ifelse(probit_pred > 0.5, TRUE, FALSE)

nb_model <- naiveBayes(y ~ ., data = df_imputed)
nb_pred <- predict(nb_model, newdata = df_tes)

#knn



test_y <- df_tes$y
knn_test<-df_tes %>% drop_na()
model_knn <- knn(train = df_imputed %>% drop_na(), test = df_tes %>% drop_na(), cl = knn_test$y, k = 5)

#evaluacion de logit
confusionMatrix(as.factor(logit_pred_class), as.factor(test_y))
roc_logit <- roc(test_y, logit_pred)
auc(roc_logit)

#evaluacion de probit
confusionMatrix(as.factor(probit_pred_class), as.factor(test_y))
roc_probit <- roc(test_y, probit_pred)
auc(roc_probit)

#evaluacion de naive bayes
confusionMatrix(as.factor(nb_pred), as.factor(test_y))
roc_nb <- roc(test_y, as.numeric(nb_pred))
auc(roc_nb)
