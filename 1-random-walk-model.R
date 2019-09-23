# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# drift is the drift rate (default value is 0)
# sdrw is the variability in the drift rate (default value is 0.3)
# criterion is the threshold for a response (default value is 3)

random.walk.model <- function(samples, drift=0, sdrw=0.3, criterion=3) {
  output <- data.frame(matrix(ncol = 2, nrow = 0))
  
  for (i in seq(from = 1, to = samples)) {
    int_ev_sig <- 0
    rt <- 0
    while ((-criterion <= int_ev_sig) && (int_ev_sig <= criterion)) {
      new_ev <- rnorm(1, mean = drift, sd = sdrw)
      int_ev_sig <- int_ev_sig + new_ev
      rt <- rt + 1
    }
    correct = TRUE
    if (int_ev_sig < -criterion) {
      correct = FALSE
    }
    output <- rbind(output, data.frame(correct, rt))
  }
  output <- setNames(output, c("correct", "rt"))
  return(output)
}

# test the model ####

testy_test <- random.walk.model(10)

# if the model is working correctly, then the line below should generate a data frame with 
# 1000 samples and about half of the samples should be correct. the average rt will probably
# be around 112, but might vary from that by a bit.

initial.test <- random.walk.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####
# we can use dplyr to filter the data and visualize the correct and incorrect RT distributions

library(dplyr)

correct.data <- initial.test %>% filter(correct==TRUE)
incorrect.data <- initial.test %>% filter(correct==FALSE)

hist(correct.data$rt)
hist(incorrect.data$rt)
