# inspired by this video https://www.youtube.com/watch?v=ovJcsL7vyrk
# by Veritasium

library(purrr)
library(ggplot2)
library(ggthemes)


logistic_map <- function(x_prev, r, i=0, reslist=NULL){
  i <- i+1
  x_next <- r * x_prev * (1- x_prev)
  if (i==1){
    reslist <- c(x_prev, x_next)
  } else {
    reslist <- c(reslist, x_next)
  }
  if (i < 1000){
    return(logistic_map(x_next, r, i=i, reslist=reslist))  
  } else{
    unique_states <- as.data.frame(unique(reslist[900:100]))
    names(unique_states) <- 'states'
    unique_states$rate <- r
    return(unique_states)
  }
  
}



maps <- map_dfr(seq(0, 5, by=0.05), function(x){logistic_map(0.9,x)})


ggplot(maps, aes(x=rate, y=states)) +
  geom_point(size=0.0001) +
  theme_minimal()


