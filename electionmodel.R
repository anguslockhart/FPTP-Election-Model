# proptional swing but flat for green, N iterations, uncorelated error terms, riding level stuff

bc2013 <- read.csv("~/Downloads/bc2013.csv")

csv <- bc2013[,-1]


pastresults <- c(44.14, 39.71, 8.13)
predresults <- c(39,39,18)

electmodel <- function(csv, predresults, pastresults, n=1000) {
  require(ggplot2)
  require(tidyverse)
  if (length(predresults)!=length(pastresults)) {
    print("Error. Number of predictions does not match number of results.")
    break}
  pred <- as.data.frame(matrix(0, ncol=length(predresults), nrow=length(csv[[1]])))
  result <- as.data.frame(matrix(0, ncol=length(predresults), nrow=n))
  for (i in 1:n){
    errors <- replicate(length(csv[[1]]), rnorm(length(predresults), 0, 7))
    for (h in 1:length(csv[[1]])) {
      errors[,h] <- (errors[,h]-mean(errors[,h]))
    }
    for (j in 1:length(csv[[1]])) {
      for (k in 1:length(predresults)) {
        pred[[k]][j] <- ((csv[[k]][j])*(predresults[k]/pastresults[k])+errors[k,j])
      }
    }
    for (o in 1:length(predresults)) {
      result[[o]][i] <- sum(max.col(pred)==o)
    }
  }
  wins <- max.col(result)
  winstab <- table(wins)
  means <- colMeans(as.matrix(result))
  result.for.plot <- gather(as.tibble(result), result[[1]],result[[length(predresults)]], key="party", value = "seats")
  plot <- ggplot(result.for.plot, aes(x=1:n, y=seats))+
    geom_point(colour=party)
  endproduct <- list(winstab, means, plot)
  return(endproduct)
} 

electmodel(csv, predresults, pastresults)
  #Have it recieve lists as arguments then loop through those lists.