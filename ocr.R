
library(kernlab)
library(knitr)
library(caret)

ocrdata <- read.csv("ocrdata.csv")

str(ocrdata)

names(ocrdata)

names(ocrdata) = c("letter", "xbox", "ybox", "width", "height", "onpix", "xbar", "ybar", "x2bar", "y2bar", "xybar", "x2ybar", 
                   "xy2bar", "xedge", "xedgey", "yedge", "yedgex")
names(ocrdata)

ocrTrain <- ocrdata[1:16000,]
ocrTest <- ocrdata[16001:20000,]

classifier <- ksvm(letter ~ ., data = ocrTrain, kernel ="vanilladot")
classifier

ocrPredictions <- predict(classifier, ocrTest)

c(as.character(head(ocrPredictions)), as.character(tail(ocrPredictions)))

matches <- ocrPredictions == ocrTest$letter

table(matches)

(sum(matches, na.rm = TRUE) / nrow(ocrTest)) * 100

classifierRbf <- ksvm(letter ~ ., data = ocrTrain, kernel = "rbfdot")

ocrPredictionsRbf <- predict(classifierRbf, ocrTest)

ocrPredictionsRbf[4000] = NA

table(ocrPredictionsRbf, ocrTest$letter, dnn=c("Prediction","Original"))[,1:16]   

matchesNew <- ocrPredictionsRbf == ocrTest$letter

table(matchesNew)

(sum(matchesNew, na.rm = TRUE) / nrow(ocrTest)) * 100

table(is.na(ocrdata))
