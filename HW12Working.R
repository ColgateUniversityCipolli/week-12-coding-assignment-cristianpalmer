########################################################################################################################

# HW 12 
library(tidyverse)
library(effectsize)

######################################################################################
# Step 1. 


# Part (a):  What values of t20 provide statistically discernible support for the alternative hypothesis?

# df = n-1
# Since alpha = 0.05, our p = 0.95

(t20.value <- qt(0.95, df = 20-1))



# Part (b):  What values of t30 provide statistically discernible support for the alternative hypothesis?

#df = n-1
# Since alpha = 0.05, our p = 0.95

(t30.value <- qt(0.95, df = 30-1))

#Part (c): 


###########################################

