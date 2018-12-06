#Pre-processing of data to correct format for Sequence Mining
new = train_data_buy %>% 
  group_by(Session_ID, idx = cumsum(Session_ID == 1L)) %>% 
  mutate(counter = row_number()) %>% 
  ungroup %>% 
  select(-idx)

head(train_data_buy)

new["SIZE"]<- 1

newseq = data.frame(new$Session_ID, new$counter, new$SIZE, new$Item_Cat)
write.table(newseq, "C:\\Users\\Sabrish\\Desktop\\Web Analytics\\Recsys-Image+Script\\mydata.txt", col.names = F, row.names = F)

#Read text file of pre-processed data into baskets
library(arulesSequences)
dataseq <- read_baskets(con = "C:\\Users\\Sabrish\\Desktop\\Web Analytics\\Recsys-Image+Script\\seq_data_click_buy.txt", info = c("sequenceID","eventID","SIZE"))
dataframe = as(dataseq, "data.frame")

#Execute cSPADE sequencing algorithm
seqs <- cspade(dataseq, parameter = list(support = 0.001), control = list(verbose = FALSE))
summary(seqs)
as(seqs,"data.frame")

#Generate rules 
rules <- ruleInduction(seqs, confidence = 0.01,control = list(verbose = FALSE, reduce = TRUE, method = "ptree"))
rulesdf = as(rules,"data.frame")
rules_conf=sort(rules, by="confidence", decreasing = FALSE)
as(rules_conf,"data.frame")

#Filter out rules with buy event
buyrules = unique(rulesdf[grep(".+B.+",rulesdf$rule),])
View(buyrules)

#Sunburst Visualization
SunDF = data.frame(train_data_buy$Session_ID,train_data_buy$Item_Cat)
colnames(SunDF)=c("SessionID","ItemCategory")
write.csv(SunDF,"C:\\Users\\Sabrish\\Desktop\\Web Analytics\\Recsys-Image+Script\\SunDF.csv")

library(plyr)
library(dplyr)

path <- ddply(SunDF, "SessionID", function(df1)paste(df1$ItemCategory, collapse = "-"))
#Create a column made up of "-end"
path$V2 <- "-end"
#Combine V1 and V2 into pagepath, with no space in between
path$pagepath <- paste(path$V1,path$V2,sep="")
View(path)

#Expand the full page path into different columns
path = read.table(text=path$pagepath,sep = "-", fill = TRUE)
#Combine only the first 6 columns, with "-" in between
path$path6 <- paste(path$V1,path$V2,path$V3,path$V4,path$V5,path$V6,sep="-")
#Make new data set that contains path6 only
path_output <- subset(path,select=path6)

#Get rid of the extra "-" in the end
path_output$path6 <- gsub("end----","end",path_output$path6)
path_output$path6 <- gsub("end---","end",path_output$path6)
path_output$path6 <- gsub("end--","end",path_output$path6)
path_output$path6 <- gsub("end-","end",path_output$path6)

#Export to CSV and edit using Pivot Table in Excel
write.csv(path_output, "C:\\Users\\Sabrish\\Desktop\\Web Analytics\\Recsys-Image+Script\\path_output.csv")

#Execute Sunburst Visualization
library(sunburstR)
sequences <- read.csv("C:\\Users\\Sabrish\\Desktop\\Web Analytics\\Recsys-Image+Script\\Sunburst datafile.csv")
sunburst(sequences)
