# takes in settings from Master.R and uses them to 1) create train/test sets
# 2) model and 3) plot curves

trainYears <- c("2006", "2007", "2008", "2009","2010", "2011", "2012", "2013", "2014", "2015")
if (!productionMode) {
  trainYears <- trainYears[! trainYears %in% testYears]
}
model_cols <- (ncol(df_model))
set.seed(111)

print("Modeling with user-defined validation data")
trainData <- df_model[,
                      c(1:model_cols)]

# Reduce train set to non-predictor beaches
trainData <- trainData[which(!trainData$Client.ID %in% excludeBeaches),]
trainData <- trainData[trainData$Year %in% trainYears,]
trainData <- trainData[complete.cases(trainData),] #remove NAs from train data
if (downsample) {
  train_high <- trainData[trainData$Escherichia.coli >= highMin
                          & trainData$Escherichia.coli < highMax, ]
  train_low <- trainData[trainData$Escherichia.coli < lowMax, ]
  # use 1:5 ratio of high days to total days
  ind <- sample(c(1:nrow(train_low)),
                nrow(train_high) * 4,
                replace = TRUE)
  train_balanced <- rbind(train_high, train_low[ind, ])
  trainData <- train_balanced
}
testData <- df_model[df_model$Year %in% testYears, ]

# Reduce test set to non-predictor beaches
testData <- testData[which(!testData$Client.ID %in% excludeBeaches),]
testData <- testData[complete.cases(testData),] #remove NAs from test data

print(paste0("Train set observations = ",nrow(trainData)))
print(paste0("Test set observations = ",nrow(testData)))

model <- modelEcoli(trainData, testData, threshBegin, threshEnd, thresh, productionMode)

if(!productionMode) {
  p <- ggplot() 
  print(p + 
          geom_smooth(aes(x = model$fpr, y = model$tpr, 
                          color = "DNA Model"), 
                      span = .9) + 
          ylim(0,1) + 
          xlim(0,1) + 
          ggtitle(title1))
  print(p + 
          geom_smooth(aes(x = model$recall, y = model$precision,
                          color = "DNA Model"),
                      span = .9) +
          ylim(0,1) + 
          xlim(0,1) +
          ggtitle(title2))
  plot_data <- as.data.frame(model[-which(names(model) == "predictions")])
}

