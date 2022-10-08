# assuming working directory with "shots_data.csv" has already been set
library(tidyverse)
data <- read_csv("shots_data.csv")
zones <- NULL
for (i in seq_along(data$team)){
  if(abs(data$x[i]) >= 22 & abs(data$y[i]) <= 7.8){
    zones <- c(zones, 1)
  }
  else if(abs(data$y[i]) > 7.8 & abs(data$y[i]) <= 8.95 & abs(data$x[i]) >= 22){
    zones <- c(zones, 2)
  }
  else if ((data$x[i]^2 + data$y[i]^2)^(1/2) >= 23.75){
    zones <-c(zones, 2)
  }
  else{
    zones <- c(zones, 3)
  }
}
data_zones <- data %>% add_column(Zone = zones)
a1 <- sum(data_zones$team == "Team A" & data_zones$Zone == 1)
a2 <- sum(data_zones$team == "Team A" & data_zones$Zone == 2)
a3 <- sum(data_zones$team == "Team A" & data_zones$Zone == 3)
b1 <- sum(data_zones$team == "Team B" & data_zones$Zone == 1)
b2 <- sum(data_zones$team == "Team B" & data_zones$Zone == 2)
b3 <- sum(data_zones$team == "Team B" & data_zones$Zone == 3)
total_a <- sum(data_zones$team == "Team A")
total_b <- sum(data_zones$team == "Team B")
a_made_3c <- sum((data_zones$team == "Team A") & (data_zones$fgmade == 1 & data_zones$Zone == 1))
a_made_3 <- sum((data_zones$team == "Team A") & (data_zones$fgmade == 1 & data_zones$Zone == 2))
a_made_2 <- sum((data_zones$team == "Team A") & (data_zones$fgmade == 1 & data_zones$Zone == 3))
b_made_3c <- sum((data_zones$team == "Team B") & (data_zones$fgmade == 1 & data_zones$Zone == 1))
b_made_3 <- sum((data_zones$team == "Team B") & (data_zones$fgmade == 1 & data_zones$Zone == 2))
b_made_2 <- sum((data_zones$team == "Team B") & (data_zones$fgmade == 1 & data_zones$Zone == 3))

# Zone percentages of team A
100 * (c(a1, a2, a3))/total_a 

# Zone percentages of team B
100 * (c(b1, b2, b3))/total_b

# eFG% of team A on corner 3s
(1.5 * a_made_3c) / a1

# eFG% of team A on non-corner 3s
(1.5 * a_made_3) / a2

# eFG% of team A on 2s
a_made_2 / a3

# eFG% of team B on corner 3s
(1.5 * b_made_3c) / b1

# eFG% of team B on non-corner 3s
(1.5 * b_made_3) / b2

# eFG% of team B on 2s
b_made_2 / b3
