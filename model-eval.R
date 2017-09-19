library(data.table)
library(RSocrata)

labs <- read.socrata("https://data.cityofchicago.org/Parks-Recreation/Beach-Lab-Data/2ivx-z93u")
preds <- read.socrata("https://data.cityofchicago.org/Parks-Recreation/Beach-E-coli-Predictions/xvsz-3xcj")
labs$DNA.Sample.Timestamp <- strftime(labs$DNA.Sample.Timestamp, format = "%Y-%m-%d")
results_df <- data.table(readRDS(paste0(getwd(),"/Data/df.Rds")))

# 2017 Pilot results

thresh <- 381
df <- merge(preds, labs, by.x = c("Beach.Name", "Date"), by.y = c("Beach", "DNA.Sample.Timestamp"))
dt <- data.table(df)
dt[Predicted.Level >= thresh, predHigh := 1]
dt[Predicted.Level < thresh, predHigh := 0]
dt[DNA.Reading.Mean >= 1000, actualHigh := 1]
dt[DNA.Reading.Mean < 1000, actualHigh := 0]
tp <- sum(dt$actualHigh == 1 & dt$predHigh == 1)
fn <- sum(dt$actualHigh == 1 & dt$predHigh == 0)
fp <- sum(dt$actualHigh == 0 & dt$predHigh == 1)
tn <- sum(dt$actualHigh == 0 & dt$predHigh == 0)
tpr <- tp / (tp + fn)
fpr <- fp / (fp + tn)
tpr
fpr

labsDt <- data.table(labs)
labsDt <- labsDt[DNA.Sample.Timestamp > as.Date("2017-01-01") &
                   Beach %in% c("Rainbow",
                                "Calumet",
                                "63rd Street",
                                "Montrose",
                                "South Shore")]
labsDt[DNA.Reading.Mean >= 1000, actualHigh := 1]
labsDt[DNA.Reading.Mean < 1000, actualHigh := 0]
table(labsDt$actualHigh)

# find good example date

dt$modelwin <- (dt$predHigh == 1 & dt$actualHigh == 1) # | (dt$predHigh == 0 & dt$actualHigh == 0)
labs2 <- labs[labs$DNA.Sample.Timestamp > as.Date("2017-01-01") &
                !is.na(labs$DNA.Sample.Timestamp),]
labs2$highlevels <- labs2$DNA.Reading.Mean > 1000
labs2 <- data.table(labs2)

modelwins <- dt[,sum(modelwin),.(Date)]
modelwins[order(-rank(V1))]
rapidwins <- labs2[,sum(highlevels),.(DNA.Sample.Timestamp)]
rapidwins[order(-rank(V1))]

predicted <- dt[as.Date(Date) == as.Date("2017-06-14")]
# tested <- labs2[DNA.Sample.Timestamp == as.Date("2017-06-14") & Beach %in% c("Rainbow", "South Shore", "Calumet", "Montrose", "63rd Street")]
tested <- labs2[DNA.Sample.Timestamp == as.Date("2017-06-14")]

View(predicted)
View(tested)

write.csv(predicted, "predicted-2017-06-14.csv", row.names = FALSE)
write.csv(tested, "tested-2017-06-14.csv", row.names = FALSE)

# USGS results

dt <- results_df[!is.na(Predicted.Level) &
                   Date < as.Date("2016-01-01"),
                 .(Date, 
                   Client.ID, 
                   Predicted.Level,
                   Escherichia.coli)]
dt <- dt[Client.ID %in% c("12th",
                          "31st",
                          "39th",
                          "57th",
                          # "63rd",
                          "Albion",
                          # "Calumet",
                          "Foster",
                          "Howard",
                          "Jarvis",
                          # "Juneway",
                          "Leone",
                          # "Montrose",
                          "North Avenue",
                          "Oak Street",
                          "Ohio",
                          "Osterman",
                          # "Rainbow",
                          "Rogers")]
                          # "South Shore")]
dt <- na.omit(dt)
dt[Predicted.Level >= 235, predHigh := 1]
dt[Predicted.Level < 235, predHigh := 0]
dt[Escherichia.coli >= 235, actualHigh := 1]
dt[Escherichia.coli < 235, actualHigh := 0]
tp <- sum(dt$actualHigh == 1 & dt$predHigh == 1)
fn <- sum(dt$actualHigh == 1 & dt$predHigh == 0)
fp <- sum(dt$actualHigh == 0 & dt$predHigh == 1)
tn <- sum(dt$actualHigh == 0 & dt$predHigh == 0)
tpr <- tp / (tp + fn)
fpr <- fp / (fp + tn)
tpr
fpr

