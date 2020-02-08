# inspired by this video https://www.youtube.com/watch?v=ovJcsL7vyrk
# by Veritasium

library(purrr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scatterplot3d)
library(rgl)


logistic_map <- function(x_prev, r, i=0, reslist=NULL,
                         starting_position=NULL){
  i <- i+1
  x_next <- r * x_prev * (1- x_prev)
  if (i==1){
    starting_position <- x_prev
    reslist <- c(x_prev, x_next)
  } else {
    reslist <- c(reslist, x_next)
  }
  if (i < 1000){
    return(logistic_map(x_next, r, i=i, reslist=reslist,
                        starting_position=starting_position))  
  } else{
    unique_states <- as.data.frame(unique(reslist[900:1000]))
    names(unique_states) <- 'states'
    unique_states$rate <- r
    unique_states$starting_position <- starting_position
    return(unique_states)
  }
  
}


# altering rate
maps_rate <- map_dfr(seq(0, 5, by=0.05), function(x){logistic_map(0.9,x)})


points_rate <- ggplot(maps_rate, aes(x=rate, y=states)) +
  geom_point(size=10^-10) +
  theme_minimal()

lines_rate <- ggplot(maps_rate, aes(x=rate, y=states)) +
  geom_line(size=10^-10) +
  theme_minimal()


# altering starting position
maps_position <- map_dfr(seq(0, 1, by=0.01), function(x){logistic_map(x,2.9)})

points_position <- ggplot(maps_position, aes(x=starting_position, y=states)) +
  geom_point(size=10^-10) +
  theme_minimal()

lines_position <- ggplot(maps_position, aes(x=starting_position, y=states)) +
  geom_line(size=10^-10) +
  theme_minimal()


# altering both
maps_both <- map2_dfr(seq(0, 1, by=0.01),
                      seq(0, 5, by=0.05),
                      function(x,y){logistic_map(x,y)})




scatterplot3d(x=maps_both$starting_position, y=maps_both$rate, z=maps_both$states,
              highlight.3d=TRUE, col.axis="blue",
              col.grid="lightblue",pch=20)


scatterplot3d(x=maps_both$rate, y=maps_both$starting_position, z=maps_both$states,
              highlight.3d=TRUE, col.axis="blue",
              col.grid="lightblue",pch=20)


scatterplot3d(x=maps_both$rate, y=maps_both$states, z=maps_both$starting_position,
              highlight.3d=TRUE, col.axis="blue",
              col.grid="lightblue",pch=20)

options(rgl.printRglwidget=TRUE)
scatter3d(x=maps_both$rate, y=maps_both$states, z=maps_both$starting_position,
          surface = F)


