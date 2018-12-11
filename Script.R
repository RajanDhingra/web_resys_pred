# Get the sessions that resulted in a buy

train_data_filtered<-train_data_sep[!train_data_sep$Category == '0',]

# Get the buy sequences

session_id_buy<-train_data_filtered[train_data_filtered$Category == 'B',"Session_ID"]
session_id_buy<-unique(session_id_buy)

train_data_buy<-train_data_filtered[train_data_filtered$Session_ID %in% session_id_buy,]

head(train_data_buy,n = 8)

##Create a new column with Item_ID and Category

train_data_buy["Item_Cat"]<-paste(train_data_buy$Item_ID,"-",train_data_buy$Category)

head(train_data_buy,n = 8)

#Create a transactions dataset

library(arules,arulesViz)

transactions_buy <- as(split(train_data_buy[,"Item_Cat"],train_data_buy[,"Session_ID"]),"transactions")
head(transactions_buy)

#Frequentitems
frequentItems = eclat(transactions_buy, parameter = list(supp=0.01,maxlen=15))
frequentItems = sort(frequentItems, by="support", decreasing = TRUE)
itemFrequencyPlot(transactions_buy, topN=10, type='absolute', main="Item Frequency")

#product recommendations
#get list of buy Item_Cat

buys<-unique(train_data_buy$Item_Cat[grep("B$",train_data_buy$Item_Cat)])
clicks<-unique(train_data_buy$Item_Cat[grep("([0-9+,S]$)",train_data_buy$Item_Cat)])

#Click->Buy Rules

rules=apriori(transactions_buy, parameter = list(supp=0.01, conf=0.5), appearance = list(lhs=clicks, rhs=buys), control=list(verbose=T))
rules_conf=sort(rules, by="confidence", decreasing = TRUE)
inspect(rules_conf)

#Buy->Buy Rules

buy_rules<-apriori()

#Rule Plots

library(shiny)
library(plotly)
library(grid)
library(arulesViz)
plot(rules, engine="htmlwidget")
plot(rules, method="two-key plot", engine="htmlwidget")
plot(rules, method="matrix", engine="htmlwidget")
plot(rules, method="graph", engine="htmlwidget")
plot(rules, method="paracoord")
plot(rules, method="grouped")
plot(rules, method="matrix3D")
