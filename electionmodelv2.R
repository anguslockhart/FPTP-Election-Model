# proptional swing but flat for green, N iterations, uncorelated error terms, riding level stuff
library(tidyverse)


bc2013 <- read.csv("~/Downloads/bc2013.csv")
onresults <- read_csv("~/Dropbox/Coding Projects/Election Modeling/onresults.csv")
onresults <- onresults[,-1]

csv <- bc2013[,-1]


pastresults <- c(37.65, 35.45, 22.74)
predresults <- c(22,34,36)
predresults <- c(19.3,40.6,33.7)


electmodel <- function(csv, predresults, pastresults, n=1000, method="prop", colours=NULL) {
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
        if (method=="prop"){pred[[k]][j] <- ((csv[[k]][j])*(predresults[k]/pastresults[k])+errors[k,j])}
        if (method=="abs"){pred[[k]][j] <- ((csv[[k]][j])+(predresults[k]-pastresults[k])+errors[k,j])}
      }
    }
    for (o in 1:length(predresults)) {
      result[[o]][i] <- sum(max.col(pred)==o)
    }
  }
  wins <- max.col(result)
  winstab <- table(wins)
  means <- colMeans(as.matrix(result))
  result.for.plot <- gather(as.tibble(result), key="party", value = "seats")
  result.for.plot$trial <- rep(1:n,3)
  if (is.null(colours)) {result.for.plot$colour <- as.numeric(result.for.plot$party)}
  if (!is.null(colours)) {result.for.plot$colour <- rep(colours, each=n)}
  plot <- ggplot(result.for.plot, aes(x=party, y=seats, colour=colour))+
    geom_boxplot()+
    scale_color_identity()
  endproduct <- list(winstab, means, plot)
  return(endproduct)
} 

result <- electmodel(onresults, predresults, pastresults, method="prop", colours=c("red", "blue", "orange"))
result
#Have it recieve lists as arguments then loop through those lists.


plot <- ggplot(result.for.plot, aes(x=party, y=seats, colour=colour))+
  geom_boxplot()
endproduct <- list(winstab, means, plot)

