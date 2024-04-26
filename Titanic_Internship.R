library(randomForest)
library(caret)
Titanic.Dataset <- read.csv("~/Downloads/Titanic-Dataset.csv")
Titanic.Dataset <- Titanic.Dataset[, -c(1, 4, 9, 11)]  
Titanic.Dataset$Age[is.na(Titanic.Dataset$Age)] <- mean(Titanic.Dataset$Age, na.rm = TRUE)  
Titanic.Dataset$Embarked[is.na(Titanic.Dataset$Embarked)] <- mode(Titanic.Dataset$Embarked)  
Titanic.Dataset <- cbind(Titanic.Dataset, model.matrix(~ Sex + Embarked, data = Titanic.Dataset)[, -1])

X <- Titanic.Dataset[, -3]  
y <- Titanic.Dataset$Survived

set.seed(42)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
train_data <- Titanic.Dataset[train_index, ]
test_data <- Titanic.Dataset[-train_index, ]

train_data$Survived <- as.factor(train_data$Survived)
test_data$Survived <- as.factor(test_data$Survived)

model <- randomForest(Survived ~ ., data = train_data, ntree = 100)

y_pred <- predict(model, newdata = test_data)
accuracy <- mean(y_pred == test_data$Survived)
print(paste("Accuracy:", accuracy))

#Accuracy :0.78651685393258