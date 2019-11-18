setwd("C:/Users/Desktop/??????/????? ????")
library(ggcorrplot)
library(caret)
library(caretEnsemble)
library(ggforce)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(readr)
library(ROSE)
library(e1071)
Data <- read.table(file = "ALLNA0.txt", sep = "\t", header = TRUE)
glimpse(Data)
pPbI <- preProcess(Data[, 7:11], method='bagImpute')
Data[,7:11] <- predict(pPbI, Data[,7:11])
gcred = Data[,7:11]
save(gcred, file="ALLNA0.RData")
Data <- Data[,c(-1,-3,-5,-6,-10)]
#Distribution by quantile
ggplot(Data) + aes(BackLegs, fill = Breed) +
  geom_density(position = "stack")+
  geom_vline(xintercept = quantile(Data$BackLegs,
                                   p = c(0.25, 0.5, 0.75,1)),colour = "blue",
             linetype = 5, size = 1.5)
table (cut(Data$BackLegs,breaks=quantile(Data$BackLegs,
                                         c(0,0.5,1), include.lowest=TRUE)))
Data$LEG <- cut(Data$BackLegs, breaks=c(1,2,5),
                labels=c("Q1","Q2"), include.lowest=TRUE)#,2;
Data$LEG <- as.factor(Data$LEG)
summary(Data)
Data<-Data[,c(-1,-6)]

#Data proportion
prop.table(table(Data$LEG)) %>% round(digits = 2)
n_observations <- nrow(Data)
predictions <- rep(x = "Q1",  n_observations)
mean(predictions == Data$LEG) * 100
Data$Breed<-as.factor(Data$Breed)
summary(Data)
#Balancing the data
data_balanced_both <- ovun.sample(LEG ~ ., data = Data, method = "both", p=0.5, N=24584, seed = 42)$data
table(data_balanced_both$LEG)
Data<-data_balanced_both
ggplot(data = Data, aes(x = LEG, y = ..count.., fill = LEG)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  theme_bw()+ 
  xlab("Leg conformation")+
  ylab("Count")+
  theme(text = element_text(family = "Optima", face = "bold"))+
  theme(text = element_text(size=15))+
  ggtitle(label = "Balanced data")

#Analysis of continuous data
library(ggpubr)
p1 <- ggplot(data = Data, aes(x = AverageDailyGain, fill = LEG)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = LEG), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  xlab("AverageDailyGain")+
  ylab("Density")+
  theme_bw()
p2 <- ggplot(data = Data, aes(x = LEG, y = AverageDailyGain, color = LEG)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  xlab("LEG")+
  ylab("AverageDailyGain")+
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("AverageDailyGain", size = 15))
final_plot


p1 <- ggplot(data = Data, aes(x = Backfat, fill = LEG)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = LEG), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = Data, aes(x = LEG, y = Backfat, color = LEG)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Backfat", size = 15))
final_plot

p1 <- ggplot(data = Data, aes(x = MuscleThikness, fill = LEG)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = LEG), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = Data, aes(x = LEG, y = MuscleThikness, color = LEG)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("MuscleThikness", size = 15))
final_plot

#Analysis of categorical data
ggplot(data = Data, aes(x = Breed, y = ..count.., fill = LEG)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Breed") +
  theme_bw() +
  ylab("Count")+
  theme(legend.position = "bottom")
prop.table(table(Data$LEG)) %>% round(digits = 2)
n_observations <- nrow(Data)
predictions <- rep(x = "Q1",  n_observations)
mean(predictions == Data$LEG) * 100
M <- cor(Data[,2:4])
ggcorrplot(M, hc.order = TRUE, type = "lower",
           colors = c("white","yellow","purple" ), lab = TRUE)+
  theme(text = element_text(family = "Optima", face = "bold"))+
  theme(text = element_text(size=15))
#Analysis quality
quality_data <- Data %>%
  select(-AverageDailyGain, -Backfat, -MuscleThikness)

quality_data_tidy <- quality_data %>%
  gather(key = "variable", value = "grup",-LEG)


quality_data_tidy <- quality_data_tidy %>% filter(!is.na(grup))


quality_data_tidy <- quality_data_tidy %>%
  mutate(variable_grup = paste(variable, grup, sep = "_"))

test_proporcion <- function(df){
  n_conf <- sum(df$LEG == "Q1") 
  n_noconf     <- sum(df$LEG == "Q2")
  n_total <- n_conf + n_noconf
  test <- prop.test(x = n_conf, n = n_total, p = 0.5011)
  prop_conf <- n_conf / n_total
  return(data.frame(p_value = test$p.value, prop_conf))
}


analisis_prop <- quality_data_tidy %>%
  group_by(variable_grup) %>%
  nest() %>%
  arrange(variable_grup) %>%
  mutate(prop_test = map(.x = data, .f = test_proporcion)) %>%
  unnest(prop_test) %>%
  arrange(p_value) %>% 
  select(variable_grup,p_value, prop_conf)
analisis_prop 
analisis_prop<-as.data.frame(analisis_prop)

top4_group <- analisis_prop %>% pull(variable_grup) %>% head(4)
# ????????? ???????, ???????, ??????? ??????? ?????? ? ???????? ??????, ?????????? ??????????? ????????????? ? ??????? ? ?? ??????? ????????.
plot_group <- function(grup, df, threshold_line = 0.5011){
  
  p <- ggplot(data = df, aes(x = 1, y = ..count.., fill = LEG)) +
    geom_bar() +
    scale_fill_manual(values = c("gray50", "orangered2")) +
    # Se anade una linea horizontal en el nivel base
    geom_hline(yintercept = nrow(df) * threshold_line,
               linetype = "dashed") +
    labs(title = grup) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank())
  return(p)
}

data_grafic <- quality_data_tidy %>%
  filter(variable_grup %in% top4_group) %>%
  group_by(variable_grup) %>%
  nest() %>%
  arrange(variable_grup)

plots <- map2(data_grafic$variable_grup, .y = data_grafic$data, .f = plot_group)

ggarrange(plotlist = plots, common.legend = TRUE)

library(randomForest)
Data_rf <- map_if(.x = Data, .p = is.character, .f = as.factor) %>%
  as.data.frame()
model_randforest <- randomForest(formula = LEG ~ . ,
                                 data = Data_rf,
                                 mtry = 3,
                                 importance = TRUE, 
                                 ntree = 1000) 
importance <- as.data.frame(model_randforest$importance)
importance <- rownames_to_column(importance,var = "variable")

p1 <- ggplot(data = importance, aes(x = reorder(variable, MeanDecreaseAccuracy),
                                    y = MeanDecreaseAccuracy,
                                    fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Accuracy ") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importance, aes(x = reorder(variable, MeanDecreaseGini),
                                    y = MeanDecreaseGini,
                                    fill = MeanDecreaseGini)) +
  labs(x = "variable", title = " Purity (Gini)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)
#Choose predictors
library(caret)
set.seed(42)
index <- createDataPartition(Data$LEG, p = 0.9, list = FALSE)
data_1 <- Data[index, ]
independent_tes?_data_1  <- Data[-index, ]
index <- createDataPartition(data_1$LEG, p = 0.7, list = FALSE)
train_data_global_1<-data_1[index, ]
test_data_1<-data_1[-index, ]
#index<-createDataPartition(train_data_global$LEG, p = 0.7, list = FALSE)
train_data_1<-train_data_global_1#[-index, ]
prop.table(table(train_data_1$LEG))
prop.table(table(test_data_1$LEG))
library(recipes)             
object_recipe <- recipe(formula = LEG ~ Breed + AverageDailyGain + Backfat + MuscleThikness,
                        data =  train_data_1)
object_recipe
#?????????????????? ??????
Data %>% select(Breed,AverageDailyGain,Backfat,MuscleThikness) %>%
  nearZeroVar(saveMetrics = TRUE)
object_recipe <- object_recipe %>% step_nzv(all_predictors())
object_recipe <- object_recipe %>% step_center(all_numeric())
object_recipe <- object_recipe %>% step_scale(all_numeric())
object_recipe <- object_recipe %>% step_dummy(all_nominal(), -all_outcomes())
trained_recipe <- prep(object_recipe, training = train_data_1)
trained_recipe
train_data_prep_1 <- bake(trained_recipe, new_data = train_data_1)
test_data_prep_1  <- bake(trained_recipe, new_data = test_data_1)
glimpse(train_data_prep_1)
summary(train_data_prep_1)
train_data_prep_1<-train_data_prep_1[,c(4,1,2,3,5)]
test_data_prep_1<-test_data_prep_1[,c(4,1,2,3,5)]
# ?????? ????????????? ??????? ???????????
subsets <- c(3:5)

# ?????????? ???????? ??? ???????? ????????? bootstrapping
repeats <- 30
set.seed(123)
seeds <- vector(mode = "list", length = repeats + 1)
for (i in 1:repeats) {
  seeds[[i]] <- sample.int(1000, length(subsets))
} 
seeds[[repeats + 1]] <- sample.int(1000, 1)

ctrl_rfe <- rfeControl(functions = rfFuncs, method = "boot", number = repeats,
                       returnResamp = "all", allowParallel = TRUE, verbose = FALSE,
                       seeds = seeds)

#The recursive deletion of predictors is executed.
set.seed(342)
rf_rfe1 <- rfe(LEG ~ ., data = train_data_prep_1,
               sizes = subsets,
               metric = "Accuracy",
               # El accuracy es la proporcion de clasificaciones correctas
               rfeControl = ctrl_rfe,
               ntree = 500)

rf_rfe1
rf_rfe1$resample %>% select(1, 2, 3, 8) %>% head(8)
rf_rfe1$resample %>% group_by(Variables) %>%
  summarise(media_accuracy = mean(Accuracy),
            media_kappa = mean(Kappa)) %>%
  arrange(desc(media_accuracy))

head(rf_rfe1$variables, 4)
rf_rfe1$variables %>% filter(Variables == 4) %>% group_by(var) %>%
  summarise(mean_influence = mean(Overall),
            sd_influence = sd(Overall)) %>%
  arrange(desc(mean_influence))


model_svmlineal1 <- train(LEG ~ ., method = "svmLinear", data = train_data_prep_1)
model_svmlineal1$finalModel
partitions  <- 10
repeats <- 10


set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  
  seeds[[i]] <- sample.int(1000, 1) 
}
# The last seed is used to adjust the final model with all observations.
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

# training
#===============================================================================

control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "all", verboseIter = FALSE,
                              allowParallel = TRUE)

# Model adjustment
# ==============================================================================
set.seed(342)
model_svmlineal1 <- train(LEG ~ ., data = train_data_prep_1,
                           method = "svmLinear",
                           metric = "Accuracy",
                           trControl = control_train)
model_svmlineal1
model_svmlineal1$resample %>% head(10)
summary(model_svmlineal1$resample$Accuracy)

p1 <- ggplot(data = model_svmlineal1$resample, aes(x = Accuracy)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(model_svmlineal1$resample$Accuracy),
             linetype = "dashed") +
  theme_bw() 
p2 <- ggplot(data = model_svmlineal1$resample, aes(x = 1, y = Accuracy)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

final_plot <- ggarrange(p1, p2)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Accuracy obtained in the validation", size = 15))
final_plot
#Tuning
partitions  <- 10
repeats <- 10

hyperparameters <- data.frame(C = c(0.001, 0.01, 0.1, 0.5, 1, 10))

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters)) 
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

#####
control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "all", verboseIter = FALSE,
                              allowParallel = TRUE)

#####
set.seed(342)
model_svmlineal1 <- train(LEG ~ ., data = train_data_prep_1,
                           method = "svmLinear",
                           tuneGrid = hyperparameters,
                           metric = "Accuracy",
                           trControl = control_train)
model_svmlineal1
model_svmlineal1$resample %>% head()
ggplot(data = model_svmlineal1$resample,
       aes(x = as.factor(C), y = Accuracy, color = as.factor(C))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  # Linea horizontal en el accuracy base
  geom_hline(yintercept = 0.62, linetype = "dashed") +
  labs(x = "C") +
  theme_bw() + theme(legend.position = "none")
ggplot(model_svmlineal1, highlight = TRUE) +
  labs(title = "Evolucion del accuracy del model en funcion de C") +
  theme_bw()
partitions  <- 10
repeats <- 10
hyperparameters <- expand.grid(C = c(1))

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters)) 
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "all", verboseIter = FALSE,
                              classProbs = TRUE, allowParallel = TRUE)

set.seed(342)
model_svmlineal1 <- train(LEG ~ ., data = train_data_prep_1,
                           method = "svmLinear",
                           tuneGrid = hyperparameters,
                           metric = "Accuracy",
                           trControl = control_train)

predictions_prob <- predict(model_svmlineal1, newdata = test_data_prep_1,
                            type = "prob")
predictions_prob %>% head()
predictions <- extractPrediction(
  models = list(svm = model_svmlineal1),
  testX = test_data_prep_1[, -4],
  testY = test_data_prep_1$LEG
)
predictions_raw <- predict(model_svmlineal1, newdata = test_data_prep_1,
                           type = "raw")

predictions %>% head()
confusionMatrix(data = predictions_raw, reference = test_data_prep_1$LEG,
                positive = "Q1")
model_svmlineal1$resample %>% head()
ggplot(data = model_svmlineal1$resample,
       aes(x = as.factor(C), y = Accuracy, color = as.factor(C))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  # Linea horizontal en el accuracy base
  geom_hline(yintercept = 0.62, linetype = "dashed") +
  labs(x = "C") +
  theme_bw() + theme(legend.position = "none")
ggplot(model_svmlineal1, highlight = TRUE) +
  labs(title = "Evolucion del accuracy del model en funcion de C") +
  theme_bw()
partitions  <- 10
repeats <- 10

#random of hyperparameters 
set.seed(123)
hyperparameters <- data.frame(C = runif(n = 5, min = 0.001, max = 20))

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters)) 
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

###
control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "all", verboseIter = FALSE,
                              allowParallel = TRUE)

set.seed(342)
model_svmlineal1_random <- train(LEG ~ ., data = train_data_prep_1,
                                  method = "svmLinear",
                                  tuneGrid = hyperparameters,
                                  metric = "Accuracy",
                                  trControl = control_train)

model_svmlineal1_random
partitions  <- 10
repeats <- 10
hyperparameters <- expand.grid(C = c(1))

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters)) 
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "all", verboseIter = FALSE,
                              classProbs = TRUE, allowParallel = TRUE)

set.seed(342)
model_svmlineal1 <- train(LEG ~ ., data = train_data_prep_1,
                           method = "svmLinear",
                           tuneGrid = hyperparameters,
                           metric = "Accuracy",
                           trControl = control_train)

predictions_prob <- predict(model_svmlineal1, newdata = test_data_prep_1,
                            type = "prob")
predictions_prob %>% head()
predictions <- extractPrediction(
  models = list(svm = model_svmlineal1),
  testX = test_data_prep_1[, -1],
  testY = test_data_prep_1$LEG
)
predictions %>% head()
confusionMatrix(data = predictions_raw, reference = test_data_prep_1$LEG)
error_test <- mean(predictions_raw != test_data_prep_1$LEG)
paste("El error de test del model:", round(error_test*100, 2), "%")

#Knn
partitions  <- 10
repeats <- 10

# hyperparameters
hyperparameters <- data.frame(k = c(1, 2, 5, 10, 15, 20, 30, 50))

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters)) 
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

####
control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

###
set.seed(342)
model_knn <- train(LEG ~ ., data = train_data_prep_1,
                    method = "knn",
                    tuneGrid = hyperparameters,
                    metric = "Accuracy",
                    trControl = control_train)
model_knn
ggplot(model_knn, highlight = TRUE) +
  scale_x_continuous(breaks = hyperparameters$k) +
  labs(title = "Evolucion del accuracy del model KNN", x = "K") +
  theme_bw()
confusionMatrix(model_knn)
#Naive Bayes
partitions  <- 10
repeats <- 10

# hyperparameters
hyperparameters <- data.frame(usekernel = FALSE, fL = 0 , adjust = 0)

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters))
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

#####
control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

###### 
set.seed(342)
model_nb <- train(LEG ~ ., data = train_data_prep_1,
                   method = "nb",
                   tuneGrid = hyperparameters,
                   metric = "Accuracy",
                   trControl = control_train)
model_nb

#Logistic
partitions  <- 10
repeats <- 10

# hyperparameters
hyperparameters <- data.frame(parameter = "none")

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters))
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

#### 
control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

####
set.seed(342)
model_logistic <- train(LEG ~ ., data = train_data_prep_1,
                         method = "glm",
                         tuneGrid = hyperparameters,
                         metric = "Accuracy",
                         trControl = control_train,
                         family = "binomial")
model_logistic
summary(model_logistic$finalModel)

#LDA
partitions  <- 10
repeats <- 10

# hyperparameters
hyperparameters <- data.frame(parameter = "none")

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters))
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

#### 
control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)
######
set.seed(342)
model_lda <- train(LEG ~ ., data = train_data_prep_1,
                    method = "lda",
                    tuneGrid = hyperparameters,
                    metric = "Accuracy",
                    trControl = control_train)
model_lda
model_lda$finalModel
confusionMatrix(model_lda)
# C5.0Tree 
partitions  <- 10
repeats <- 10

# hyperparameters
hyperparameters <- data.frame(parameter = "none")

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters))
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

#######
control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

##### 
set.seed(342)
model_C50Tree <- train(LEG ~ ., data = train_data_prep_1,
                        method = "C5.0Tree",
                        tuneGrid = hyperparameters,
                        metric = "Accuracy",
                        trControl = control_train)
model_C50Tree
summary(model_C50Tree$finalModel)
confusionMatrix(model_C50Tree)
mod1 <- C5.0(LEG ~ ., data = test_data_1, weights=, subset)
plot(mod1)

#Random Forest
partitions  <- 10
repeats <- 10

# hyperparameters
hyperparameters <- expand.grid(mtry = c(3, 4),
                               min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
                               splitrule = "gini")

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters))
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

##### 
control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

#####
set.seed(342)
model_rf <- train(LEG ~ ., data = train_data_prep_1,
                   method = "ranger",
                   tuneGrid = hyperparameters,
                   metric = "Accuracy",
                   trControl = control_train,
                   # Numero de arboles ajustados
                   num.trees = 500)
model_rf
model_rf$finalModel
ggplot(model_rf, highlight = TRUE) +
  scale_x_continuous(breaks = 1:30) +
  labs(title = "Evolucion del accuracy del model Random Forest") +
  guides(color = guide_legend(title = "mtry"),
         shape = guide_legend(title = "mtry")) +
  theme_bw()

confusionMatrix(model_rf)
#GBOOST
partitions  <- 10
repeats <- 10

# hyperparameters
hyperparameters <- expand.grid(interaction.depth = c(1, 2),
                               n.trees = c(500, 1000, 2000),
                               shrinkage = c(0.001, 0.01, 0.1),
                               n.minobsinnode = c(2, 5, 15))

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters))
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

##### 
control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

##### 
set.seed(342)
model_boost <- train(LEG ~ ., data = train_data_prep_1,
                      method = "gbm",
                      tuneGrid = hyperparameters,
                      metric = "Accuracy",
                      trControl = control_train,
                      distribution = "adaboost",
                      verbose = FALSE)
model_boost

pred = predict(model_boost, test_data_prep_1[,2:5], type="class")

nn.table = table(test_data_prep_1$LEG, pred)

confusionMatrix(nn.table)
#NNET
partitions  <- 10
repeats <- 10

# hyperparameters
hyperparameters <- expand.grid(size = c(10, 20, 50, 80, 100, 120),
                               decay = c(0.0001, 0.1, 0.5))

set.seed(123)
seeds <- vector(mode = "list", length = (partitions * repeats) + 1)
for (i in 1:(partitions * repeats)) {
  seeds[[i]] <- sample.int(1000, nrow(hyperparameters))
}
seeds[[(partitions * repeats) + 1]] <- sample.int(1000, 1)

###### 
control_train <- trainControl(method = "repeatedcv", number = partitions,
                              repeats = repeats, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

#####
set.seed(342)
model_nnet <- train(LEG ~ ., data = train_data_prep_1,
                     method = "nnet",
                     tuneGrid = hyperparameters,
                     metric = "Accuracy",
                     trControl = control_train,
                     rang = c(-0.7, 0.7),
                     MaxNWts = 2000,
                     trace = FALSE)
model_nnet
source("nnet_plot_update.r")
library(nnet)
nn.aba <- nnet(LEG ~ ., data = train_data_prep_1,
               decay = 1e-04, size = 10, niter=100)
plot.nnet(nn.aba)
pred = predict(nn.aba, test_data_prep_1[,2:5], type="class")

nn.table = table(test_data_prep_1$LEG, pred)

confusionMatrix(nn.table)

ggplot(model_nnet, highlight = TRUE) +
  labs(title = "Evolucion del accuracy del model NNET") +
  theme_bw()
#estimation models
models <- list(KNN = model_knn, NB = model_nb, logistic = model_logistic,
                LDA = model_lda, arbol = model_C50Tree, rf = model_rf,
                boosting = model_boost, 
                NNET = model_nnet)#SVMradial = model_svmrad,

results_resamples <- resamples(models)
results<-results_resamples$values %>% head(10)
write.table(results,"estimation_models.txt", col.names = T)
metrics_resamples <- results_resamples$values %>%
  gather(key = "model", value = "value", -Resample) %>%
  separate(col = "model", into = c("model", "metrica"),
           sep = "~", remove = TRUE)
metrics_resamples %>% head()
metrics_resamples %>% 
  group_by(model, metrica) %>% 
  summarise(media = mean(value)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))
#plot1
metrics_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(model) %>% 
  summarise(media = mean(value)) %>%
  ggplot(aes(x = reorder(model, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(model, media), y = 0,
                   xend = model, yend = media),
               color = "grey50") +
  geom_point(size = 7, color = "firebrick") +
  geom_text(color = "white", size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy base
  geom_hline(yintercept = 0.5011, linetype = "dashed") +
  annotate(geom = "text", y = 0.72, x = 8.5, label = "Accuracy base") +
  labs(title = "Validacion: Accuracy medio repeated-CV",
       subtitle = "Models sorted by average",
       x = "model") +
  coord_flip() +
  theme_bw()
#plot2
metrics_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(model) %>% 
  mutate(media = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(model, media), y = value, color = model)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy base
  geom_hline(yintercept = 0.5011, linetype = "dashed") +
  annotate(geom = "text", y = 0.65, x = 8.5, label = "Accuracy base") +
  theme_bw() +
  labs(title = "Validation: Accuracy means repeated-CV",
       subtitle = "models sorted by average") +
  coord_flip() +
  theme(legend.position = "none")
#???? ???????? ??? ????????? ???????? ???????.
matriz_metrics <- metrics_resamples %>% filter(metrica == "Accuracy") %>%
  spread(key = model, value = value) %>%
  select(-Resample, -metrica) %>% as.matrix()
friedman.test(y = matriz_metrics)
# Multiple comparisons with a Wilcoxon range sum test
# ==============================================================================

metrics_accuracy <- metrics_resamples %>% filter(metrica == "Accuracy")
comparisons  <- pairwise.wilcox.test(x = metrics_accuracy$value, 
                                       g = metrics_accuracy$model,
                                       paired = TRUE,
                                       p.adjust.method = "holm")

# p_values are stored in the form of dataframe
comparisons <- comparisons$p.value %>%
  as.data.frame() %>%
  rownames_to_column(var = "modelA") %>%
  gather(key = "modelB", value = "p_value", -modelA) %>%
  na.omit() %>%
  arrange(modelA) 
comparisons
#??????? ??? ???? ??????? 
predictions <- extractPrediction(
  models = models,
  testX = test_data_prep_1[, -1],
  testY = test_data_prep_1$LEG
)
predictions %>% head()


metrics_predictions <- predictions %>%
  mutate(success = ifelse(obs == pred, TRUE, FALSE)) %>%
  group_by(object, dataType) %>%
  summarise(accuracy = mean(success))

metrics_predictions %>%
  spread(key = dataType, value = accuracy) %>%
  arrange(desc(Test))
#plot3
ggplot(data = metrics_predictions,
       aes(x = reorder(object, accuracy), y = accuracy,
           color = dataType, label = round(accuracy, 2))) +
  geom_point(size = 8) +
  scale_color_manual(values = c("orangered2", "gray50")) +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy base
  geom_hline(yintercept = 0.5011, linetype = "dashed") +
  annotate(geom = "text", y = 0.66, x = 8.5, label = "Accuracy base") +
  coord_flip() +
  labs(title = "Accuracy training and test", 
       x = "model") +
  theme_bw() + 
  theme(legend.position = "bottom")

confusionMatrix(model_nnet)

confusionMatrix(model_boost)
confusionMatrix(model_rf)
pred.knn.roc <- predict(model_knn,test_data_prep_1[,2:5],type = "prob")
pred.rf.roc <- predict(model_nnet,test_data_prep_1[,2:5],type = "prob")
pred.boost.roc <- predict(model_boost,test_data_prep_1[,2:5],type = "prob")
library(pROC) # ?????? ??? ROC-??????
m1.roc <- roc(test_data_prep_1$LEG, pred.knn.roc[,1])
m2.roc <- roc(test_data_prep_1$LEG, pred.rf.roc[,1])
m3.roc <- roc(test_data_prep_1$LEG, pred.boost.roc[,1])
plot(m1.roc, grid.col=c("green", "red"), grid=c(0.1, 0.2),
     print.auc=TRUE,print.thres=TRUE)
plot(m2.roc , add = T, col="green", print.auc=T,
     print.auc.y=0.45,print.thres=TRUE)
plot(m3.roc , add = T, col="blue", print.auc=T,
     print.auc.y=0.40,print.thres=TRUE)
legend("bottomright", c("KNN","NNET","Boost","COMBY"),lwd=2,
       col=c("black","green","blue","red"))




# ?????????? ????????? ???????
results <- resamples(models)
# ????????? ???????? ????? ????????
summary(results, metric = "Accuracy")
# ?????? ????????????? ?????????? ? ?????????? ???????
scales <- list(x=list(relation="free"),
               y=list(relation="free"))
dotplot(results, scales=scales)
diffs <- diff(results)
# summarize p-values for pair-wise comparisons
summary(diffs)
pred.fm <- data.frame(
  LDA=predict(model_lda,test_data_prep_1[,2:5]),
  KNN=predict(model_knn,test_data_prep_1[,2:5]),
  GLM=predict(model_logistic,test_data_prep_1[,2:5]),
  RF=predict(model_rf,test_data_prep_1[,2:5]),
  NNET=predict(model_nnet,test_data_prep_1[,2:5]),
  C50Tree=predict(model_C50Tree,test_data_prep_1[,2:5]),
  Boost=predict(model_boost,test_data_prep_1[,2:5])
)
CombyPred <- apply(pred.fm, 1, function (voice) {
  voice2 <- c(voice, voice[2]) # ? RF ??????? ?????
  ifelse(sum(voice2=="Q2") > 3,"Q2", "Q1") }
)
pred.fm <- cbind(pred.fm, COMB=CombyPred)
head(pred.fm)
# ??????? ???????????? ?????? ?????????
ModCrit <- function (fact, pred) {
  cM <- table(fact, pred)
  c( Accur <- (cM[1,1]+cM[2,2])/sum(cM),
     Sens <- cM[1,1]/(cM[1,1]+cM[2,1]),
     Spec <- cM[2,2]/(cM[2,2]+cM[1,2]),
     F.score <- 2 * Accur * Sens / (Accur + Sens),
     MCC <- ((cM[1,1]*cM[2,2])-(cM[1,2]*cM[2,1])) /
       sqrt((cM[1,1]+cM[1,2])*(cM[1,1]+cM[2,1])*
              (cM[2,2]+cM[1,2])*(cM[2,2]+cM[2,1])) )
}
Result <- t(apply(pred.fm,2, function (x)
  ModCrit(test_data_prep_1$LEG, x)))
colnames(Result) <- c("????????","????????.","?????????.",
                      "F-????","?? ???????")
round(Result,3)

#NB=predict(model_nb,test_data_prep_1[,2:5]),
predictions <- extractPrediction(
  models = model_knn,
  testX = test_data_prep_1[, -1],
  testY = test_data_prep_1$LEG
)
predictions %>% head()

summary(data_balanced_both)


metrics_accuracy <- metrics_resamples %>% filter(metrica == "Accuracy")
comparisons  <- pairwise.wilcox.test(x = metrics_accuracy$value, 
                                       g = metrics_accuracy$model,
                                       paired = TRUE,
                                       p.adjust.method = "holm")

# p_values are stored in the form of dataframe
comparisons <- comparisons$p.value %>%
  as.data.frame() %>%
  rownames_to_column(var = "modelA") %>%
  gather(key = "modelB", value = "p_value", -modelA) %>%
  na.omit() %>%
  arrange(modelA) 

comparisons




library(tree)
# Conversion of the variable response to type factor
data_balanced_both$LEG<-as.factor(data_balanced_both$LEG)
arbol_clasificacion <- model_C50Tree$finalModel
summary(arbol_clasificacion)
plot(x = arbol_clasificacion, type = "proportional")
text(x = arbol_clasificacion, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")
plot(arbol_clasificacion)
