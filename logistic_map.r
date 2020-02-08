# inspired by this video https://www.youtube.com/watch?v=ovJcsL7vyrk
# by Veritasium

library(purrr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(plotly)


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
maps_rate <- map_dfr(seq(0, 5, by=0.05), function(x){logistic_map(0.6,x)})


points_rate <- ggplot(maps_rate, aes(x=rate, y=states)) +
  geom_point(size=10^-10) +
  theme_minimal()

lines_rate <- ggplot(maps_rate, aes(x=rate, y=states)) +
  geom_line(size=10^-10) +
  theme_minimal()


# altering starting position
maps_position <- map_dfr(seq(0, 1, by=0.01), function(x){logistic_map(x,2)})

points_position <- ggplot(maps_position, aes(x=starting_position, y=states)) +
  geom_point(size=10^-10) +
  theme_minimal()

lines_position <- ggplot(maps_position, aes(x=starting_position, y=states)) +
  geom_line(size=10^-10) +
  theme_minimal()


grid.arrange(points_position, lines_position,
             points_rate, lines_rate)


combinations <- expand.grid(seq(0, 1, by=0.01), seq(0, 5, by=0.05))



# altering both
maps_both <- map2_dfr(combinations$Var1,
                      combinations$Var2,
                      function(x,y){logistic_map(x,y)})



scatterplot3d(x=maps_both$starting_position, y=maps_both$rate, z=maps_both$states,
              highlight.3d=TRUE, col.axis="blue",
              col.grid="lightblue",pch=20)


plot_ly(maps_both, x=~rate, y=~starting_position,
        z=~states,
        type="scatter3d", mode="markers", size=10^-9)





