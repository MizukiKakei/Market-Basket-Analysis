######################################################################################
# Brand Preference Prediction                                                        #
#                                                                                    #
# Mizuki Kakei                                                                       #
#                                                                                    #
# Version 1.0                                                                        #     
# Date : 17.05.2019                                                                  #     
# We analysed the patterns of products bought together in the company Electronidex,  #
# which is called market basket analysis.                                            #
# In order to do so, we used apriori algorythm.                                      #
#                                                                                    #
######################################################################################

library(arules)
library(readr)
library(arulesViz)
library(caTools)
library(prabclus)
library(DEoptimR)
library(trimcluster)
library(caret)
library(ggplot2)
library(plotly)

# Reading data ####
CustomerData <- read.transactions("ElectronidexTransactions2017.csv",
                                  format = c("basket"),
                                  TRUE, sep =",")

# Excluding NA 
na.exclude(CustomerData)


# Visualization ####
# Setting Margin 
par(mar=c(3,0,0.5,1))

# Checking the frequency of the items
itemFrequencyPlot(CustomerData,
                  type = "absolute",
                  horiz= TRUE,
                  top = 20,
                  support = 0.2,
                  cex.names = 1.2,
                  tel.cex = 0.4)


# Apriori algorythm ####
rules <- apriori(CustomerData,
                 parameter = list(support=0.014,
                                  confidence=0.08,
                                  minlen = 2,
                                  maxlen = 2)
                )
# Sorting the rules
rulesByLift <- sort(rules, by = "lift")

# restricting the number of rules
subRules <- rulesByLift[quality(rulesByLift)$lift > 1 ]

# Visualization of the rules
plotly_arules(rulesByLift)

ItemRules <- subset(subRules[1:50],
                    parameter = list(minlen = 2, maxlen = 2)
                   )
plot(ItemRules,
     method = NULL,
     measure = "support", 
     shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL)
plot(ItemRules, method = "graph",
     measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL,
     gp_labels = gpar(cex=2))
plot(ItemRules, method = "grouped",
     measure = "support", shading = "lift",
     gp_labels = gpar(cex=2), 
     parameter = list(minlen = 2, maxlen = 2))