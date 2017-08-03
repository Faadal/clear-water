library(data.table)

# x - latitude
# y - freq of elevated occurances

clusters <- 5
reclusters <- 5

source("R/00_startup.R")
df <- readRDS(paste0(getwd(),"/Data/df.Rds"))
dt <- data.table(df)
dt[Escherichia.coli >= 235, exceedance := 1]
dt[Escherichia.coli < 235, exceedance := 0]
dt_byBeach <- dt[!is.na(exceedance),
                 .(exceedances = sum(exceedance)),
                 .(Client.ID, Latitude)]

set.seed(317)
km <- kmeans(scale(dt_byBeach[,2:3]),
             centers = clusters,
             nstart = 100)
plot(dt_byBeach[,2:3],
     col =(km$cluster +1),
     main=paste0("K-Means result with ", clusters, " clusters"), 
     pch=20, 
     cex=2)
dt_byBeach$cluster <- km$cluster

dt_byBeach[order(exceedances, decreasing = TRUE)]

# remove worst beaches (these are the beaches to always test)

km2 <- kmeans(scale(dt_byBeach[exceedances < 165,
                         2:3]),
             centers = reclusters,
             nstart = 100)

plot(dt_byBeach[exceedances < 165,
                2:3],
     col =(km2$cluster +1),
     main=paste0("K-Means result with ", reclusters, " reclusters"), 
     pch=20, 
     cex=2)
dt_byBeach[exceedances < 165,"recluster"] <- km2$cluster

dt_byBeach[order(exceedances, decreasing = TRUE)]

# Choice of 5 visually: Jarvis, Ohio, Foster, Oak Street, Rogers
# getting some better results with 12th, Ohio, Oak Street, and Leone. Jarvis doesn't have enough history?
