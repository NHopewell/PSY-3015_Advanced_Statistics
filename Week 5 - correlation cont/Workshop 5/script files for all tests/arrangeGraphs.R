#YOU CAN USE THE FOLLOWING CODE TO GRAPH THEM TOGETHER
library(grid)
library(gridExtra)
grid.arrange(graph1, 
             graph2,
             graph3,
             nrow=1, ncol = 3)  #NCOL * NROW MUST BE > THAN THE NUMBER OF GRAPHS