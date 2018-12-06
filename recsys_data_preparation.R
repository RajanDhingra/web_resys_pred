data_buys<-read.csv("C:/Users/e0267625/Desktop/yoochoose-data/yoochoose-buys.dat",header = F)
data_clicks<-read.csv("C:/Users/e0267625/Desktop/yoochoose-data/yoochoose-clicks.dat",header = F)
data_test<-read.csv("C:/Users/e0267625/Desktop/yoochoose-data/yoochoose-test.dat",header = F)

colnames(data_clicks)<-c("Session ID", "Timestamp", "Item ID", "Category")
colnames(data_buys)<-c("Session ID", "Timestamp", "Item ID", "Price", "Quantity")
colnames(data_test)<-c("Session ID", "Timestamp", "Item ID", "Category")

#Convert to Posixct

data_buys$Timestamp <- as.POSIXct(data_buys$Timestamp, format="%Y-%m-%dT%H:%M:%OSZ", tz="GMT")
data_clicks$Timestamp <- as.POSIXct(data_clicks$Timestamp, format="%Y-%m-%dT%H:%M:%OSZ", tz="GMT")
data_test$Timestamp <- as.POSIXct(data_test$Timestamp, format="%Y-%m-%dT%H:%M:%OSZ", tz="GMT")

#check for the total duration of data
min(data_buys$Timestamp)
max(data_buys$Timestamp)

min(data_clicks$Timestamp)
max(data_clicks$Timestamp)

min(data_test$Timestamp)
max(data_test$Timestamp)

#So timestamp is from april to september

#lets subset it to June-July-Aug only for better handling lesser data

data_clicks_july <- data_clicks[data_clicks$Timestamp >= "2014-07-01" & data_clicks$Timestamp <= "2014-07-31", ]
data_buys_july <- data_buys[data_buys$Timestamp >= "2014-07-01" & data_buys$Timestamp <= "2014-07-31", ]
data_test_july <- data_test[data_test$Timestamp >= "2014-07-01" & data_test$Timestamp <= "2014-07-31", ]

rm(data_clicks,data_buys,data_test,data_clicks_jja)

#subsetting for september

data_clicks_sep <- data_clicks[data_clicks$Timestamp >= "2014-09-01" & data_clicks$Timestamp <= "2014-09-30", ]
data_buys_sep <- data_buys[data_buys$Timestamp >= "2014-09-01" & data_buys$Timestamp <= "2014-09-30", ]
data_test_sep <- data_test[data_test$Timestamp >= "2014-09-01" & data_test$Timestamp <= "2014-09-30", ]

rm(data_clicks,data_buys,data_test,data_clicks_july,data_buys_july,data_test_july)

#join clicks and buys
#is B a category already?

data_clicks_sep[data_clicks_sep$category == 'B',]

#so no it is not... lets add a column category to data_buys with B indicating Buys and join with data_clicks

data_buys_sep["Category"] <- "B"
data_clicks_sep["Price"] <- 0
data_clicks_sep["Quantity"] <- 0

#reorder columns
data_buys_sep <- data_buys_sep[,c(1,2,3,6,4,5)]

#next append buys and clicks

train_data_sep <- rbind(data_clicks_sep,data_buys_sep)

#now need to sort the data by session id and timestamp
colnames(train_data_sep)<-c("Session_ID","Timestamp","Item_ID","Category","Price","Quantity")

library(plyr)

train_data_sep <- arrange(train_data_sep,Session_ID,Timestamp)

train_data_sep[!complete.cases(train_data_sep),]

#so all are complete cases and sequential now

rm(data_buys_sep,data_clicks_sep)
