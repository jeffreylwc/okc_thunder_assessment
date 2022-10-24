data <- read.csv(file = '/Downloads/shots_data.csv')

### Pythagorean theorem function to calculate the distance of the shot from the hoop
pythagorean <- function(x, y) {
  return(sqrt(x^2 + y^2))
}

### Function to calculate distance from the hoop
data['distance'] = mapply(pythagorean, x = data$x, y = data$y)

### function to calculate the effective fg percentage
effective_fg_percentage <- function(fgm, three_pt, fga) {
  return((fgm + (0.5 * three_pt))/fga)
}

### Team A
team_a_df <- data[data$team == 'Team A',]
### Team B
team_b_df <- data[data$team == 'Team B',]

### Corner 3's
c3_df <- data[data$y <= 7.8,]
c3_df <- subset(c3_df, x >= 22 | x <= -22)

### Team A corner 3's
team_a_c3 <- c3_df[c3_df$team == 'Team A',]
team_a_c3_attempted <- length(team_a_c3$fgmade)
team_a_c3_made <- sum(team_a_c3$fgmade)

### Team A Corner 3's distribution
team_a_c3_attempted/length(team_a_df$fgmade)
sprintf("Team A corner 3's shot distribution is %#.3f", team_a_c3_attempted/length(team_a_df$fgmade))
### Team A corner 3's efg
team_a_c3_effective_fg <- effective_fg_percentage(team_a_c3_made, team_a_c3_made, team_a_c3_attempted)
sprintf("Team A corner 3's efg is %#.3f", team_a_c3_effective_fg)

### Team B corner 3's
team_b_c3 <- c3_df[c3_df$team == 'Team B',]
team_b_c3_attempted <- length(team_b_c3$fgmade)
team_b_c3_made <- sum(team_b_c3$fgmade)

### Team B corner 3's distribution
team_b_c3_attempted/length(team_b_df$fgmade)
sprintf("Team B corner 3's shot distribution is %#.3f", team_b_c3_attempted/length(team_b_df$fgmade))
### Team B corner 3's efg
team_b_c3_effective_fg <- effective_fg_percentage(team_b_c3_made, team_b_c3_made, team_b_c3_attempted)
sprintf("Team B corner 3's efg is %#.3f", team_b_c3_effective_fg)

### Non-corner 3's
nc3_df <- subset(data, y > 7.8 & distance >= 23.75)

### Team A Non-corner 3's
team_a_nc3 <- nc3_df[nc3_df$team == 'Team A',]
team_a_nc3_attempted <- length(team_a_nc3$fgmade)
team_a_nc3_made <- sum(team_a_nc3$fgmade)

### Team A Non-corner 3's distribution
team_a_nc3_attempted / length(team_a_df$fgmade)
sprintf("Team A non-corner 3's shot distribution is %#.3f", team_a_nc3_attempted/length(team_a_df$fgmade))
### Team A Non-corner 3's efg
team_a_nc3_effective_fg <- effective_fg_percentage(team_a_nc3_made, team_a_nc3_made, team_a_nc3_attempted)
sprintf("Team A non-corner 3's efg is %#.3f", team_a_nc3_effective_fg)

### Team B Non-corner 3's
team_b_nc3 <- nc3_df[nc3_df$team == 'Team B',]
team_b_nc3_attempted <- length(team_b_nc3$fgmade)
team_b_nc3_made <- sum(team_b_nc3$fgmade)

### Team B Non-corner 3's distribution
team_b_nc3_attempted/length(team_b_df$fgmade)
sprintf("Team B non-corner 3's shot distribution is %#.3f", team_b_nc3_attempted/length(team_b_df$fgmade))
### Team B Non-corner 3's efg
team_b_nc3_effective_fg <- effective_fg_percentage(team_b_nc3_made, team_b_nc3_made, team_b_nc3_attempted)
sprintf("Team B non-corner 3's efg is %#.3f", team_b_nc3_effective_fg)

### 2PT
two_pt_arc <- subset(data, y > 7.8 & distance < 23.75)
two_pt_sideline <- data[data$y <= 7.8,]
two_pt <- subset(two_pt_sideline, x < 22 & x > -22)
two_pt_total <- rbind(two_pt, two_pt_arc)

### Team A 2pt
team_a_two_pt <- two_pt_total[two_pt_total$team == 'Team A',]
team_a_two_pt_attempted <- length(team_a_two_pt$fgmade)
team_a_two_pt_made <- sum(team_a_two_pt$fgmade)

### Team A 2pt distribution
team_a_two_pt_attempted/length(team_a_df$fgmade)
sprintf("Team A 2PT shot distribution is %#.3f", team_a_two_pt_attempted/length(team_a_df$fgmade))
### Team A 2pt efg
team_a_2pt_effective_fg <- effective_fg_percentage(team_a_two_pt_made, 0, team_a_two_pt_attempted)
sprintf("Team A 2PT efg is %#.3f", team_a_2pt_effective_fg)

### Team B 2pt
team_b_two_pt <- two_pt_total[two_pt_total$team == 'Team B',]
team_b_two_pt_attempted <- length(team_b_two_pt$fgmade)
team_b_two_pt_made <- sum(team_b_two_pt$fgmade)

### Team B 2pt distribution
team_b_two_pt_attempted/length(team_b_df$fgmade)
sprintf("Team B 2PT shot distribution is %#.3f", team_b_two_pt_attempted/length(team_b_df$fgmade))
### Team B 2pt efg
team_b_2pt_effective_fg <- effective_fg_percentage(team_b_two_pt_made, 0, team_b_two_pt_attempted)
sprintf("Team B 2PT efg is %#.3f", team_b_2pt_effective_fg)






