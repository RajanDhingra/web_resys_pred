# Web Resys Prediction

The objective of this project is to perform web usage mining on an ecommerce dataset called Recsys to identify events that would lead to purchases. 
This includes creating and testing association rules and sequential patterns in web usage data.

After pre-processing, the first step was to convert the data in to transactions format to extract association rules. 

Frequent itemsets were identified and the Apriori algorithm was used to extract association rules using minimum support and confidence. The items in these rules could not be tagged to click or buys, as the antecedents and consequents in the rule had only IDs of the items. 

Therefore, in the next iteration, the category was concatenated to the item to create a new field, that would represent if that item was clicked or purchased. 

Also, to align the rules with end objective, class association mining was performed to generate rules where the antecedents were click events and the consequents were buy events. 

The attributes required for sequence mining were Sequence ID, Event ID, Size and items. Sequential mining was done using CSPADE algorithm in R. Sequences for general events and click buy events were obtained by using different minimum support and minimum confidence values. Click-buy events were also mined in the rules using Regular expressions.

A novel testing algorithm was built for rule matching item combinations (in case of association rules) and sequence combinations (in case of sequence rules) up to size two antecedents. Hence, we utilize rules up to size 3 for making predictions. It can easily be scaled further to include antecedent sizes of 3 and above also. For testing, only general rules were used. This is because the test data had no rules that included buys and hence testing based on click buys generated rules would give extremely few matches.
