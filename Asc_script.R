# Get the sessions that resulted in a buy

train_data_filtered<-train_data_sep[!train_data_sep$Category == '0',]

# Get the buy sequences

session_id_buy<-train_data_filtered[train_data_filtered$Category == 'B',"Session_ID"]
session_id_buy<-unique(session_id_buy)

train_data_buy<-train_data_filtered[train_data_filtered$Session_ID %in% session_id_buy,]

head(train_data_buy,n = 8)

#Create a new column with Item_ID and Category

train_data_buy["Item_Cat"]<-paste(train_data_buy$Item_ID,"-",train_data_buy$Category)

head(train_data_buy,n = 8)

#Create a transactions dataset

library(arules,arulesViz)

transactions_buy <- as(split(train_data_buy[,"Item_Cat"],train_data_buy[,"Session_ID"]),"transactions")
head(transactions_buy)

#Frequentitems
frequentItems = eclat(transactions_buy, parameter = list(supp=0.01))
frequentItems = sort(frequentItems, by="support", decreasing = TRUE)
itemFrequencyPlot(transactions_buy, topN=10, type='absolute', main="Item Frequency")

#product recommendations
#get list of buy Item_Cat

buys<-unique(train_data_buy$Item_Cat[grep("B$",train_data_buy$Item_Cat)])
clicks<-unique(train_data_buy$Item_Cat[grep("([0-9+,S]$)",train_data_buy$Item_Cat)])

#Click->Buy Rules

rules=apriori(transactions_buy, parameter = list(supp=0.01, conf=0.65), appearance = list(lhs=clicks, rhs=buys), control=list(verbose=T))
rules_conf=sort(rules, by="confidence", decreasing = TRUE)
inspect(rules_conf)

summary(rules_conf)

#Buy->Buy Rules (WIP)

#bb_rules=apriori(transactions_buy, parameter = list(supp=0.015, conf=0.5),appearance = list(lhs=buys))
#bb_rules_conf=sort(rules, by="confidence", decreasing = TRUE)
#inspect(bb_rules_conf)


#General Rules (used for predictions) - since there is no test data available for buys, we cannot test the accuracy of
#click-buy and buy-buy rules
#So we now build general rules

#Create general transactions object
transactions_gen <- as(split(train_data_filtered[,"Item_ID"],train_data_filtered[,"Session_ID"]),"transactions")
head(transactions_gen)
inspect(head(transactions_gen))

#Create general rulesets
gen_rules<-apriori(transactions_gen,parameter = list(supp=0.001,conf=0.6,maxlen=3),control = list(verbose=TRUE))
gen_rules_conf<-sort(gen_rules,by='confidence',decreasing = TRUE)
inspect(head(gen_rules_conf,10))

library(RColorBrewer)
itemFrequencyPlot(transactions_gen,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

#Augment using purchase rulesets
#transactions_pur <- as(split(train_data_buy[,"Item_ID"],train_data_buy[,"Session_ID"]),"transactions")
#pur_rules<-apriori(transactions_pur,parameter = list(supp=0.01,conf=0.65,maxlen=3),control = list(verbose=TRUE))
#pur_rules_conf<-sort(pur_rules,by='confidence',decreasing = TRUE)
#inspect(head(pur_rules_conf))

summary(gen_rules)

#Rule Plots

library(shiny)
library(plotly)
library(grid)
library(arulesViz)
plot(gen_rules, engine="htmlwidget")
plot(gen_rules, method="two-key plot")
plot(gen_rules, method="matrix", engine="htmlwidget")
plot(gen_rules, method="graph", engine="htmlwidget")
plot(gen_rules, method="paracoord")
plot(gen_rules, method="grouped")
plot(gen_rules, method="matrix3D")

#Testing the ruleset

inspect(head(transactions_gen))

#rule_frame<-as(c(gen_rules_conf,pur_rules_conf),"data.frame")

rule_frame<-as(gen_rules_conf,"data.frame")


#Prepare the test set - remove missing / 0 categories
data_test_sep<-data_test_sep[data_test_sep$Category != '0',]

#Algorithm: for every basket generated, take single and coupled combinations of items and make predictions.
#Then combine the predicted item set and measure against the actual itemset

test_trans<-as(split(data_test_sep[,"Item ID"],data_test_sep[,"Session ID"]),"transactions")
test_basket<-as(test_trans,'data.frame')
rm(test_trans)

#Get items involved in session
get_items<-function(basket){
  for(trans in basket){
    list <- strsplit(trans,split = ",")
    list <- c(sapply(list, function(x) gsub("([{,}])","",x)))
    return(list)
  }
}

#Make possible tuples of items
make_tuples<-function(items){
  if(length(items) > 1){
    combos<-combn(items,2)
    combos<-t(combos)
  } else {
    combos = c("")
  }
  return(combos)
}

#Get rules fired
rules_fired<-function(ses,rules){
  rules_fired<-vector()
  singles<-get_items(ses)
  tuples<-make_tuples(singles)
  for(single in singles){
    antecedent = paste("{",single,"} =>",sep="")
    if(length(grep(antecedent, rules$rules,fixed=TRUE))>0){
      rules_fired<-c(rules_fired,grep(antecedent, rules$rules,fixed=TRUE))
    }
  }
  if(length(tuples) > 0) {
    for(i in 1:dim(tuples)[1]){
      tuple<-tuples[i,]
      antecedent = paste("{",tuple[1],',',tuple[2],"} =>",sep="")
      if(length(grep(antecedent, rules$rules,fixed=TRUE))>0){
        rules_fired<-c(rules_fired,grep(antecedent, rules$rules,fixed=TRUE))
      }
      antecedent = paste("{",tuple[2],',',tuple[1],"} =>",sep="")
      if(length(grep(antecedent, rules$rules,fixed=TRUE))>0){
        rules_fired<-c(rules_fired,grep(antecedent, rules$rules,fixed=TRUE))
      }
    } 
  }
  return(rules_fired)
}

#Get consequent itemsets list
get_consequents_list<-function(rules_fired,rules){
 relevant_rules<-rules$rules[rules_fired]
 consq<-vector()
 for(rule in relevant_rules){
      consq<-append(consq,sapply(strsplit(rule,'=>'),function(x) {gsub("[[:space:]]", "", gsub("([{,}])","",tail(x,1)))}))
 }
 return(unique(consq))
}

#Match predicted consequents to actual
match<-function(list,acts){
  actls<-vector()
  actls<-append(actls,sapply(strsplit(as.character(acts),split = ','),function(x) {gsub("[[:space:]]", "", gsub("([{,}])","",x))}))
  actls<-unique(actls)
  return(length(intersect(list,actls))/length(list))
  #return(actls)
}

#match(get_consequents_list(rules_fired("{214819577,214854300}",rule_frame),rule_frame),"{214819577,214854300}")
unlist_match<-function(list,items){
  return(length(intersect(list,unlist(items))))
}


#Since the test dataset is a large dataset that can span several Items that may or may not be included in our train set for
#September, we need to subset the test set accordingly so that we are able to match with our extracted rulesets

#1. delete 1 item sessions

test_basket<-test_basket[grep(pattern = ",",test_basket$items),]

#2. test transactions that contain the items involved in ruleset

items_list<-unique(unlist(as(lhs(gen_rules_conf),"list")))
test_basket_1<-mutate(test_basket,itemsets = as.vector(sapply(strsplit(as.character(items),split = ','),function(x) {gsub("[[:space:]]", "", gsub("([{,}])","",x))})))
test_basket_1$is_relevant<-lapply(test_basket_1$itemsets, function(x) unlist_match(items_list,x))
test_basket_1<-test_basket_1[test_basket_1$is_relevant > 0,]

#3. take a random subset of 1000 sessions and make predictions

library(dplyr)
set.seed(60)
sample_basket<-sample_n(test_basket_1,size = 1000)
sample_basket$itemsets<-NULL;sample_basket$is_relevant<-NULL
print(mean(sapply(sample_basket$items,function(x) match(get_consequents_list(rules_fired(x,rule_frame),rule_frame),x)),na.rm=TRUE))
