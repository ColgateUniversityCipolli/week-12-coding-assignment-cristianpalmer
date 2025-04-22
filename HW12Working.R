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

#Part (c): Conduct a simulation study to assess the Type I error rate of this approach.

# install.packages("VGAM")
#?rlaplace()

library(VGAM)
a = 0
b = 4.0

for (i in 1:10000){
  simulations <- rlaplace(n = 30, location = a, scale = b)
# Outcome 1: Discernible support for the alternative at time 20 (Type I Error)
  t <- t.test(simulations[1:20],
              mu = 0,
              alternative = "greater")

  
  
# Outcome 2: Not discernible support for the alternative at time 20 and discernible support for the alternative at time 30 (Type I Error)
  
  
# Outcome 3: Not discernible support for the alternative at time 20 and time 30 (Not an error)
  
  
  
  
}

###########################################

