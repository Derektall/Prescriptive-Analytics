## I utilize the arules package in order to determine which specific products might be purchased together in certain circumstances

library(arules)

data(Groceries)

## I use Groceries data that comes with arules package

inspect(head(Groceries, 3))

frequentItems <- eclat (Groceries) ## Finding the most frequent items 

inspect(frequentItems)

itemFrequencyPlot(Groceries, topN=13, type="absolute", main="Frequently Bought Items", popCol="green")

rules <- apriori (Groceries, parameter = list(supp = .001, conf=.9))

rules_intervals <- sort(rules, by="confidence", decreasing=TRUE)

inspect(head(rules_intervals))

##If confidence is 1, then if the first item is bought the 2nd item is also bought 100% of the time
## i.e when rice and sugar are bought, whole milk is bought 100% of the time

rules_lift <- sort (rules, by="lift", decreasing=TRUE)

inspect(head(rules_lift))

## The highest lift means when liquor/wine is purchased it is 11.2 times more likely that bottled beer will
## be bought together alongside them when compared to purchases that were not related at all.

subsetRules <- which(colSums(is.subset(rules,rules)) > 1) ##Getting rid of the rules that are subset of
## larger rules
length(subsetRules)
rules <- rules[-subsetRules]

## Demonstrating how to find rules related to specific products

soda_rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="soda"), control = list (verbose=F))

soda_rules_intervals <- sort (soda_rules, by="confidence", decreasing=TRUE) 
inspect(head(soda_rules_intervals)) ##Getting rules of what products lead to buying soda

water_rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="rhs",lhs="bottled water"), control = list (verbose=F))

water_rules_intervals <- sort (water_rules, by="confidence", decreasing=TRUE) 
inspect(head(water_rules_intervals)) ##Finding rules of what products are bought alongside bottled water

