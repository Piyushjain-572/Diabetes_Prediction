library(ggcorrplot)
library(ggplot2)
library(corrplot)
library(caret)
library(corrplot)
library(caret)
library(tidyverse)
library(e1071)
library(gridExtra)
library(graphics)
library(randomForest)
library(tree)

pima <- read.csv("D:/Downloads/pima-indians-diabetes.csv",
                 col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))

head(pima)

str(pima)      # show the structure of the dataset

sapply(pima, function(x) sum(is.na(x)))   # To check number of missing values in dataset

ggplot(pima, aes(Diabetes)) +
  geom_bar(fill = c("pink","cyan")) +
  theme_bw() +
  labs(title = "Diabetes Classification", x = "Diabetes") +
  theme(plot.title = element_text(hjust = 0.5)) 

corrplot(cor(pima[, -9]), type = "lower", method = "number")

corrplot(cor(pima[, -9]), type = "lower", method = "pie")

univar_graph <- function(univar_name, univar, data, output_var) {
  
  g_1 <- ggplot(data, aes(x=univar)) + 
    geom_density(fill = "pink") + 
    xlab(univar_name) + 
    theme_bw()
  
  gridExtra::grid.arrange(g_1, ncol=2, top = paste(univar_name,"variable", "/ [ Skew:",timeDate::skewness(univar),"]"))
  
}

for (x in 1:(ncol(pima)-1)) {
  univar_graph(univar_name = names(pima)[x], univar = pima[,x], data = pima, output_var = pima[,'Diabetes'])
}

set.seed(123)
n <- nrow(pima)
train <- sample(n, trunc(0.80*n))
pima_training <- pima[train, ]
pima_testing <- pima[-train, ]

# Training The logistic Regression Model
glm_fm1 <- glm(Diabetes ~., data = pima_training, family = binomial)
summary(glm_fm1)

glm_fm2 <- update(glm_fm1, ~. - Triceps_Skin - Serum_Insulin - Age )
summary(glm_fm2)

par(mfrow = c(2,2))
plot(glm_fm2)

# Testing the logistic Regression  Model
glm_probs <- predict(glm_fm2, newdata = pima_testing, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)
confusionMatrix(as.factor(glm_pred), as.factor(pima_testing$Diabetes )) # Confusion Matrix for logistic regression
acc_glm_fit <- confusionMatrix(as.factor(glm_pred), as.factor(pima_testing$Diabetes ))



pima <- read.csv("D:/Downloads/pima-indians-diabetes.csv",
                 col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))
pima$Diabetes <- as.factor(pima$Diabetes)

set.seed(1000)
intrain <- createDataPartition(y = pima$Diabetes, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]

# Training The Decision tree Model
treemod <- tree(Diabetes ~ ., data = train)

summary(treemod)
treemod
plot(treemod)
text(treemod, pretty = 0)

# Testing the DEcision tree Model
tree_pred <- predict(treemod, newdata = test, type = "class" )
confusionMatrix(tree_pred, test$Diabetes)
acc_treemod <- confusionMatrix(tree_pred, test$Diabetes)


# Training the random forest 
set.seed(123)
rf_pima <- randomForest(Diabetes ~., data = pima_training, mtry = 8, ntree=50, importance = TRUE)

# Testing the Model
rf_probs <- predict(rf_pima, newdata = pima_testing)
rf_pred <- ifelse(rf_probs > 0.5, 1, 0)
confusionMatrix(as.factor(rf_pred),as.factor(pima_testing$Diabetes))
acc_rf_pima <- confusionMatrix(as.factor(rf_pred),as.factor(pima_testing$Diabetes))

importance(rf_pima)

par(mfrow = c(1, 2))
varImpPlot(rf_pima, type = 2, main = "Variable Importance",col = 'red')
plot(rf_pima, main = "Error vs no. of trees grown",col = "blue")



pima <- read.csv("D:/Downloads/pima-indians-diabetes.csv",
                 col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))
pima$Diabetes <- as.factor(pima$Diabetes)

set.seed(1000)
intrain <- createDataPartition(y = pima$Diabetes, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]

#Training the SVM model
tuned <- tune.svm(Diabetes ~., data = train, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned)

svm_model  <- svm(Diabetes ~., data = train, kernel = "radial", gamma = 0.01, cost = 10) 
summary(svm_model)

#Testing the SVM  model
svm_pred <- predict(svm_model, newdata = test)
confusionMatrix(svm_pred, test$Diabetes)
acc_svm_model <- confusionMatrix(svm_pred, test$Diabetes)

#Evaluating the performance 0f all models

result_glm <- c(acc_glm_fit$byClass['Sensitivity'],acc_glm_fit$byClass['Specificity'], acc_glm_fit$byClass['Precision'], 
                acc_glm_fit$byClass['Recall'], acc_glm_fit$byClass['F1'])

result_tree <- c(acc_treemod$byClass['Sensitivity'],acc_treemod$byClass['Specificity'], acc_treemod$byClass['Precision'], 
                 acc_treemod$byClass['Recall'], acc_treemod$byClass['F1'])

result_rf <- c(acc_rf_pima$byClass['Sensitivity'],acc_rf_pima$byClass['Specificity'], acc_rf_pima$byClass['Precision'], 
               acc_rf_pima$byClass['Recall'], acc_rf_pima$byClass['F1'])

result_svm <- c(acc_svm_model$byClass['Sensitivity'],acc_svm_model$byClass['Specificity'], acc_svm_model$byClass['Precision'], 
                acc_svm_model$byClass['Recall'], acc_svm_model$byClass['F1'])

all_results <- data.frame(rbind(result_glm, result_tree, result_rf, result_svm))
names(all_results) <- c("Sensitivity", "Specificity", "Precision", "Recall", "F1")
all_results

col <- c("#ed3b3b", "#0099ff")

graphics::fourfoldplot(acc_glm_fit$table, color = col, conf.level = 0.95, margin = 1, 
                       main = paste("Logistic Regression Accuracy(",round(acc_glm_fit$overall[1]*100),"%)", sep = ""))

graphics::fourfoldplot(acc_treemod$table, color = col, conf.level = 0.95, margin = 1, 
                       main = paste("Decision Tree Accuracy(",round(acc_treemod$overall[1]*100),"%)", sep = ""))

graphics::fourfoldplot(acc_rf_pima$table, color = col, conf.level = 0.95, margin = 1, 
                       main = paste("Random Forest Accuracy(",round(acc_rf_pima$overall[1]*100),"%)", sep = ""))

graphics::fourfoldplot(acc_svm_model$table, color = col, conf.level = 0.95, margin = 1, 
                       main = paste("SVM Model Accuracy(",round(acc_svm_model$overall[1]*100),"%)", sep = ""))


accuracy <- data.frame(Model=c("Logistic Regression","Decision Tree","Random Forest", "Support Vector Machine (SVM)"), Accuracy=c(acc_glm_fit$overall['Accuracy'], acc_treemod$overall['Accuracy'], acc_rf_pima$overall['Accuracy'], acc_svm_model$overall['Accuracy'] ))
ggplot(accuracy,aes(x=Model,y=Accuracy)) + geom_bar(stat='identity',fill = c("pink")) + theme_bw() + ggtitle('Comparison of Model Accuracy')

plot_feat1_feat2('Plasma_Glucose','Age')
