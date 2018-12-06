####################################################################################################
#Preprocessing for generating event id
####################################################################################################
train_data_filtered$Timestamp_numeric <- as.numeric(train_data_filtered$Timestamp)

library(dplyr)

new <- train_data_filtered %>% 
  group_by(Session_ID, idx = cumsum(Timestamp_numeric == 1L)) %>% 
  mutate(counter = row_number()) %>% 
  ungroup %>% 
  select(-idx)
head(new)

seq_data <- as.data.frame(new)
seq_data <- seq_data[,c('Session_ID','Item_ID','counter')]
head(seq_data)
seq_data["SIZE"] = 1

seq_data <- seq_data[,c(1,3,4,2)]
head(seq_data)

write.table(seq_data,"C:\\Users\\Sabrish\\Desktop\\Web Analytics\\seq_data.txt", col.names = F, row.names = F)
###################################################################################################
#Generate Sequences and Sequence Rules
###################################################################################################
library(arulesSequences)
dataseq <- read_baskets(con = "C:\\Users\\Sabrish\\Desktop\\Web Analytics\\seq_data.txt", info = c("sequenceID","eventID","SIZE"))
as(head(dataseq), "data.frame")

seqs <- cspade(dataseq, parameter = list(support = 0.001), control = list(verbose = FALSE))
as(seqs,"data.frame")
summary(seqs)

rules <- ruleInduction(seqs, confidence = 0.3,control = list(verbose = FALSE, reduce = TRUE, method = "ptree"))
seq_rules<-as(rules,"data.frame")
rules_conf=sort(rules, by="confidence", decreasing = FALSE)
as(rules_conf,"data.frame")
summary(rules)





##################
#Testing Sequences
##################

#Get items involved in session
get_items<-function(basket){
  for(trans in basket){
    list <- strsplit(trans,split = ",")
    list <- c(sapply(list, function(x) gsub("([<{,}>])","",x)))
    return(list)
  }
}

#get_items(test_basket_1$items[1])

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
    antecedent = paste("<{",single,"}> =>",sep="")
    if(length(grep(antecedent, rules$rule,fixed=TRUE))>0){
      rules_fired<-c(rules_fired,grep(antecedent, rules$rule,fixed=TRUE))
    }
  }
  if(length(tuples) > 0) {
    for(i in 1:dim(tuples)[1]){
      tuple<-tuples[i,]
      antecedent = paste("<{",tuple[1],'},{',tuple[2],"}> =>",sep="")
      if(length(grep(antecedent, rules$rule,fixed=TRUE))>0){
        rules_fired<-c(rules_fired,grep(antecedent, rules$rule,fixed=TRUE))
      }
      antecedent = paste("<{",tuple[2],'},{',tuple[1],"}> =>",sep="")
      if(length(grep(antecedent, rules$rule,fixed=TRUE))>0){
        rules_fired<-c(rules_fired,grep(antecedent, rules$rule,fixed=TRUE))
      }
    } 
  }
  return(rules_fired)
}


#Get consequent itemsets list
get_consequents_list<-function(rules_fired,rules){
  relevant_rules<-rules$rule[rules_fired]
  consq<-vector()
  for(rule in relevant_rules){
    consq<-append(consq,sapply(strsplit(rule,'=>'),function(x) {gsub("[[:space:]]", "", gsub("([<{,}>])","",tail(x,1)))}))
  }
  return(unique(consq))
}

#Match predicted consequents to actual
match<-function(list,acts){
  actls<-vector()
  actls<-append(actls,sapply(strsplit(as.character(acts),split = ','),function(x) {gsub("[[:space:]]", "", gsub("([<{,}>])","",x))}))
  actls<-unique(actls)
  return(length(intersect(list,actls))/length(list))
  #return(actls)
}

#match(get_consequents_list(rules_fired("{214819577,214854300}",rule_frame),rule_frame),"{214819577,214854300}")
unlist_match<-function(list,items){
  return(length(intersect(list,unlist(items))))
}

#0. group items at session level

data_test_sep<-data_test_sep[data_test_sep$Category != '0',]
test_trans<-as(split(data_test_sep[,"Item ID"],data_test_sep[,"Session ID"]),"transactions")
test_basket<-as(test_trans,'data.frame')
rm(test_trans)


#1. delete 1 item sessions

test_basket<-test_basket[grep(pattern = ",",test_basket$items),]

#2. test transactions that contain the items involved in ruleset

library(dplyr)
items_list<-unique(unlist(as(lhs(rules),"list")))
test_basket_1<-mutate(test_basket,itemsets = as.vector(sapply(strsplit(as.character(items),split = ','),function(x) {gsub("[[:space:]]", "", gsub("([{,}])","",x))})))
test_basket_1$is_relevant<-lapply(test_basket_1$itemsets, function(x) unlist_match(items_list,x))
test_basket_1<-test_basket_1[test_basket_1$is_relevant > 0,]

#3. take a random subset of 1000 sessions and make predictions

set.seed(60)
sample_basket<-sample_n(test_basket_1,size = 1000)
sample_basket$itemsets<-NULL;sample_basket$is_relevant<-NULL
print(mean(sapply(sample_basket$items,function(x) match(get_consequents_list(rules_fired(x,seq_rules),seq_rules),x)),na.rm=TRUE))
