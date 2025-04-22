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

# Part (c): Conduct a simulation study to assess the Type I error rate of this approach.

# install.packages("VGAM")
# ?rlaplace()

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

######################################################################################
# Step 2. 

# sample size
n <- 15

# true mean for Beta(10,2)
mean.10.2 <- 10/(10+2)

# true mean for Beta(2,10)

mean.2.10 <- 2/(2+10)

# true mean for Beta(10,10)

mean.10.10 <- 10/(10+10)


# will loop over and add to

left_tail <- c(0,0,0)
right_tail <- c(0,0,0)
two_tail <- c(0,0,0)

# start loop
# ?rbeta

for (i in 1:10000){
  beta1 <- rbeta(n, 10, 2)
  beta2 <- rbeta(n, 2, 10)
  beta3 <- rbeta(n, 10, 10)

# part (a): What proportion of the time do we make an error of Type I for a left-tailed test?

  # Left-Tailed Test
  left.10.2 <- t.test(beta1,
                  mu = mean.10.2,
                  alternative = "less")
  left.2.10 <- t.test(beta2,
                  mu = mean.2.10,
                  alternative = "less")
  left.10.10 <- t.test(beta3,
                  mu = mean.10.10,
                  alternative = "less")
  
  # Keeping track of type 1 errors for each distribution
  
  left_tail[1] <- left_tail[1] + (left.10.2$p.value < 0.05)
  left_tail[2] <- left_tail[2] + (left.2.10$p.value < 0.05)
  left_tail[3] <- left_tail[3] + (left.10.10$p.value < 0.05)




# part (b): What proportion of the time do we make an error of Type I for a right-tailed test?

  # Left-Tailed Test
  right.10.2 <- t.test(beta1,
                      mu = mean.10.2,
                      alternative = "greater")
  right.2.10 <- t.test(beta2,
                      mu = mean.2.10,
                      alternative = "greater")
  right.10.10 <- t.test(beta3,
                       mu = mean.10.10,
                       alternative = "greater")
  
  # Keeping track of type 1 errors for each distribution
  
  right_tail[1] <- right_tail[1] + (right.10.2$p.value < 0.05)
  right_tail[2] <- right_tail[2] + (right.2.10$p.value < 0.05)
  right_tail[3] <- right_tail[3] + (right.10.10$p.value < 0.05)



# part (c): What proportion of the time do we make an error of Type I for a two-tailed test?

  # Left-Tailed Test
  two_tail.10.2 <- t.test(beta1,
                       mu = mean.10.2,
                       alternative = "two.sided")
  two_tail.2.10 <- t.test(beta2,
                       mu = mean.2.10,
                       alternative = "two.sided")
  two_tail.10.10 <- t.test(beta3,
                        mu = mean.10.10,
                        alternative = "two.sided")
  
  # Keeping track of type 1 errors for each distribution
  
  two_tail[1] <- two_tail[1] + (two_tail.10.2$p.value < 0.05)
  two_tail[2] <- two_tail[2] + (two_tail.2.10$p.value < 0.05)
  two_tail[3] <- two_tail[3] + (two_tail.10.10$p.value < 0.05)

}


# part (a): What proportion of the time do we make an error of Type I for a left-tailed test?
type1.error.left <- left_tail/10000

# part (b): What proportion of the time do we make an error of Type I for a right-tailed test?
type1.error.right <- right_tail/10000

# part (c): What proportion of the time do we make an error of Type I for a two-tailed test?
type1.error.two_sided <- two_tail/10000

# part (d): How does skewness of the underlying population distribution effect Type I error across the test types?
results <- tibble(
  Distribution = c("Beta(10,2)", "Beta(2,10)", "Beta(10,10)"),
  "Left-Tailed Test" = type1.error.left,
  "Right-Tailed Test" = type1.error.right,
  "Two-Tailed Test" = type1.error.two_sided
)
results

