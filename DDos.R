setwd("C:/Users/Sanika/DMMLProject")
getwd()

install.packages("caTools")
install.packages('PRROC')
install.packages("corrplot")
install.packages('Amelia')

library(dplyr)
library(Amelia)
library(ggplot2)
library(dplyr)
library(caTools)
library(yardstick)
library(ggplot2)
library(caret)
library(pROC)
library(PRROC)
library(randomForest)

###########################Load Dataset##################################
df <- read.csv("DDos.csv")
head(df)
str(df)

########################Extraction##################################

df_DoS_Hulk <- df[df$Label== "DoS Hulk",]
df_BENIGN <- df[df$Label== "BENIGN",]
df_DoS_slowloris <- df[df$Label== "DoS slowloris",]

df_DoS_Hulk <- head(df_DoS_Hulk,5000)
df_BENIGN  <- head(df_BENIGN ,5000)
df_DoS_slowloris <- head(df_DoS_slowloris,5000)

finaldf <- rbind(df_DoS_Hulk,df_BENIGN)
finaldf <- rbind(finaldf,df_DoS_slowloris)

################### Missing Data #############################

missmap(finaldf)

########################### Visualization ############################


# Creating a Bar of Labels
par(mar = c(5, 5, 4, 10))
tbl <- table(finaldf$Label)
barplot(tbl , xlab = "Types Of Labels",
        col = rainbow(3))


#########################Split Train Test##############################
finaldf$Label <- as.factor(finaldf$Label)
str(finaldf)
set.seed(101)
sample <- sample.split(finaldf$Label, SplitRatio = 0.70) 
train = subset(finaldf, sample == TRUE)
test = subset(finaldf, sample == FALSE)

########################xgboost###################################
library(xgboost)
set.seed(123)
model <- train(
  Label ~., data = train, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
  ) 


result <- predict(model, test)
result
result [result >=0.5] = "TRUE"
result [result <0.5] = "FALSE"
table <- table(result,test$Label)
table

#confusion matrix
cm <- confusionMatrix(table )
cm
cm <- confusionMatrix(factor(test$Label), factor(result), dnn = c("Prediction", "Reference"))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
        geom_tile() + geom_text(aes(label=Freq)) +
        scale_fill_gradient(low="white", high="#009194") +
        labs(x = "Reference",y = "Prediction") +
        scale_x_discrete(labels=c("DoS slowloris","DoS Hulk", "BENIGN")) +
        scale_y_discrete(labels=c("BENIGN","DoS Hulk", "DoS slowloris"))



