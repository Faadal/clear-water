library(data.table)
library(RSocrata)

labs <- read.socrata("https://data.cityofchicago.org/Parks-Recreation/Beach-Lab-Data/2ivx-z93u")
preds <- read.socrata("https://data.cityofchicago.org/Parks-Recreation/Beach-E-coli-Predictions/xvsz-3xcj")
labs$DNA.Sample.Timestamp <- strftime(labs$DNA.Sample.Timestamp, format = "%Y-%m-%d")
results_df <- data.table(readRDS(paste0(getwd(),"/Data/df.Rds")))

# 2017 Pilot results

thresh <- 415
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

# USGS results

dt <- results_df[!is.na(Predicted.Level) &
                   Date > as.Date("2016-01-01"),
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
                          "Juneway",
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

