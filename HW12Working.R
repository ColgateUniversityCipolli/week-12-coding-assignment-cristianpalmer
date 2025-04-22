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
type1.20 <- 0 
type1.30 <- 0
no.error <- 0

for (i in 1:10000){
  simulations <- rlaplace(n = 30, location = a, scale = b)
  
  t <- t.test(simulations[1:20],
              mu = 0,
              alternative = "greater")
  
  # Outcome 1: Discernible support for the alternative at time 20 (Type I Error)
  if (t$p.value < 0.05){
    type1.20 <- type1.20 + 1
  } else {
    # Outcome 2: Not discernible support for the alternative at time 20 and discernible support for the alternative at time 30 (Type I Error)
    t2 <- t.test(simulations[1:30],
                 mu = 0,
                 alternative = "greater")
    
    if (t2$p.value < 0.05){
      type1.30 <- type1.30 + 1
    } else {
      # Outcome 3: Not discernible support for the alternative at time 20 and time 30 (Not an error)
      no.error <- no.error + 1
    }
  }
}  
errors.20 <- type1.20/10000
errors.30 <- type1.30/10000
no.errors <- no.error/10000
type1.error <- (type1.20 + type1.30)/10000

###########################################

