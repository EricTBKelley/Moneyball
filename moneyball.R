library(dplyr)
library(ggplot2)

batting <- read.csv('Batting.csv')
sal <- read.csv('Salaries.csv')

#creating Batting average, on base percentage, singles and slugging columns
batting$BA <- batting$H / batting$AB
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

#only data from 1985 forward
batting <- subset(batting, yearID >= 1985)

#merging batting and salary data
combo <- merge(batting, sal, by=c('playerID', 'yearID'))

##The A's lost 3 key players: Jason Giambi, Johnny Damon, and Rainer Gustavo
lost.players <- subset(combo, playerID %in% c('giambja01', 'damonjo01', 'saenzol01'))
lost.players <- lost.players %>% filter(yearID == 2001)
lost.players <- lost.players %>% select(playerID, H, X2B, X3B, HR, OBP, SLG, BA, AB)

#Finding replacement players with 3 constraints
# 1)The total combined salary of the three players can not exceed 15 million dollars.
# 2)Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
# 3)Their mean OBP had to equal to or greater than the mean OBP of the lost players

avail.players <- filter(combo, yearID == 2001)
ggplot(avail.players, aes(x=OBP, y=salary)) + geom_point() 

avail.players <- filter(avail.players, salary < 8000000, OBP>0)
mean(lost.players$AB) #489.66. I'll use 500

avail.players <- filter(avail.players, AB >= 500)

possible <- head(arrange(avail.players, desc(OBP)),10)

possible[2:4,]

#Todd Helton
#Lance Berkman
#Luis Gonzalez