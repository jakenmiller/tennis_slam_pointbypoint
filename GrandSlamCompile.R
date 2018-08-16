
#### Load libraries and set working directory ####

library(plyr)
library(dplyr)
library(chron)


setwd("C:/Users/jacobmiller21/Desktop/Papers")
# Forked tennis_slam_pointbypoint repo was cloned to directory below

DIR <- "C:/Users/jacobmiller21/Desktop/Papers/Tennis points"
tennisFiles <- list.files(path = DIR, all.files=TRUE, full.names = TRUE, recursive = FALSE)
tennisFiles <- tennisFiles[6:61]


#### Create and and merge primary data ####
matchesFiles <- tennisFiles[grepl("-points",tennisFiles)]
gamesFiles <- tennisFiles[grepl("-matches",tennisFiles)]
dfMatch <- data.frame()
dfGames <- data.frame()

for (i in 1:length(gamesFiles)) {
  temp <- read.csv(gamesFiles[i], header=TRUE)
  filespath <- unlist(strsplit(gamesFiles[i], split = "/"))
  temp$pointSource <- filespath[length(filespath)]
  dfGames <- rbind.fill(dfGames,temp)
}
rm(temp)
for (i in 1:length(matchesFiles)) {
  temp <- read.csv(matchesFiles[i], header=TRUE)
  filespath <- unlist(strsplit(matchesFiles[i], split = "/"))
  temp$matchSource <- filespath[length(filespath)]
  dfMatch <- rbind.fill(dfMatch,temp)
}
rm(temp)

df <- merge(dfMatch, dfGames, by = "match_id")
df$yearSlam <- paste(df$year, df$slam, sep = "-")
#df$na_count <- apply(df, 1, function(x) sum(is.na(x)))
#df$na_pct <- df$na_count/ncol(df)


#MissingbyYearSlam <- data.frame(df %>% group_by(yearSlam) %>% summarize_all(function(x) sum(is.na(x))))

#colNA <- data.frame(sapply(df, function(y) sum(is.na(y))))
#names(colNA) <- "missingDataCount"


#write.csv(df, "TennisGs2011-2017.csv")
#write.csv(MissingbyYearSlam, "missingByYearSlam.csv")
#write.csv(colNA, "missingByVariable.csv")

rm(dfGames, dfMatch, DIR, filespath, gamesFiles, i, matchesFiles, tennisFiles)


#### Label sex of matches ####

# Break out by data with event name (labeled male or femlae) and without event name
dfEn <- df[!is.na(df$event_name),]
dfNoEn <- df[is.na(df$event_name),]

# Create data frame with just players that do not have label of men or women's event
playerNoMFLabel <- data.frame(dfNoEn %>% group_by(player1) %>% summarize(num = length(player1)))
write.csv(playerNoMFLabel, "LackingMFLabel.csv")
rm(dfEn, dfNoEn, playerNoMFLabel)

# Sex -- Set Sex to U for unknown, which applies to all 2012 - 2017 data
# For 2011 set sex to M or F depending on the event
df$Sex <- "U"
df[!is.na(df$event_name) & df$event_name == "event_MS","Sex"] <- "M"
df[!is.na(df$event_name) & df$event_name == "Men's Singles","Sex"] <- "M"

df[!is.na(df$event_name) & df$event_name == "event_WS","Sex"] <- "F"
df[!is.na(df$event_name) & df$event_name == "Women's Singles","Sex"] <- "F"


#### Remove point number where point number is zero ####
df <- df[df$PointNumber != 0,]
df <- df[df$PointWinner !=0,]
rownames(df) <- NULL


#### Fix times ####

df$ElapsedTime <- as.character(df$ElapsedTime)
df[50307, "ElapsedTime"] <- paste0(substr(df[50307,"ElapsedTime"], 1, 5),59)
df[106917, "ElapsedTime"] <- paste0(substr(df[106917,"ElapsedTime"], 1, 5),59)
df[174637, "ElapsedTime"] <- paste0(substr(df[174637,"ElapsedTime"], 1, 5),59)
df[856485, "ElapsedTime"] <- paste0(substr(df[856485,"ElapsedTime"], 1, 5),59)

df[,"ElapsedTime"] <- chron(times=df$ElapsedTime)
# NAs are empty data rows
#which(is.na(test))
#df[which(is.na(test)),"ElapsedTime"]
df <- df[!is.na(df$ElapsedTime),]
rownames(df) <- NULL

#### Get game differential within set ####
df$P1GameDiff <- df$P1GamesWon - df$P2GamesWon

#### Get the set score of each point and set differential ####
## WARNING, HIGH COMPUTATION ##
## Would probably be much faster to multiply together logicals than perform
## multiple lookups simultaneously


df$P1SetWins <- NA
df[1,"P1SetWins"] <- 0
df$P2SetWins <- NA
df[1,"P2SetWins"] <- 0


for (i in 2:nrow(df)) {
  if (df[i,"match_id"] == df[i-1,"match_id"]) {
    if (df[i-1,"SetWinner"] == 1 & df[i-1,"SetNo"] != df[i,"SetNo"]) {
      df[i,"P1SetWins"] <- df[i-1,"P1SetWins"] + 1
      } else {
    df[i,"P1SetWins"] <- df[i-1,"P1SetWins"]
    }
  } else {
    df[i,"P1SetWins"] <- 0
  }
}

for (i in 2:nrow(df)) {
  if (df[i,"match_id"] == df[i-1,"match_id"]) {
    if (df[i-1,"SetWinner"] == 2 & df[i-1,"SetNo"] != df[i,"SetNo"]) {
      df[i,"P2SetWins"] <- df[i-1,"P2SetWins"] + 1
    } else {
      df[i,"P2SetWins"] <- df[i-1,"P2SetWins"]
    }
  } else {
    df[i,"P2SetWins"] <- 0
  }
}

df$P1SetDiff <- df$P1SetWins - df$P2SetWins

## load previously created csv
# write.csv(df, "CleanedTennisGS2011-2017.csv")
# setwd("C:/Users/jacobmiller21/Desktop/Papers")
# df <- read.csv("CleanedTennisGS2011-2017.csv")

# Add in the scores BEFORE the serve
temp <- as.character(tail(df$P1Score, n = 1))
temp1 <- as.character(head(df$P1Score, n = -1))
df$P1ScorePreServe <- as.factor(c(temp, temp1))
temp <- as.character(tail(df$P2Score, n = 1))
temp1 <- as.character(head(df$P2Score, n = -1))
df$P2ScorePreServe <- as.factor(c(temp, temp1))

# Adjust Game Difference to pre-serve
df$P1GameDiffPreServe <- df$P1GameDiff
df[df$SetWinner > 0, "P1GameDiffPreServe"] <- 0
# Note, there are some lines with a set winner that was not properly captured.
temp <- tail(df$P1GameDiffPreServe, n = 1)
temp1 <- head(df$P1GameDiffPreServe, n = -1)
df$P1GameDiffPreServe <- c(temp, temp1)

#Fix input errors. Game number should match below or above unless
#the set ended and wasn't captured.
from <- 2
to <- nrow(df) -1
for (i in from:to) {
  if ((df[i,'P1GameDiffPreServe'] != df[i-1,'P1GameDiffPreServe']) & (df[i,'P1GameDiffPreServe'] != df[i+1,'P1GameDiffPreServe'])) {
    df[i,'P1GameDiffPreServe'] <- 0
  }
}

df$FirstServe <- df$P1FirstSrvIn + df$P2FirstSrvIn
df$SecondServe <- df$P1SecondSrvIn + df$P2SecondSrvIn
df$DoubleFault <- df$P1DoubleFault + df$P2DoubleFault

# Identify US Open Tiebreaks
df$TieBreak <- 0
df[df$P1GamesWon == 6 & df$P2GamesWon == 6 & df$slam == "usopen","TieBreak"] <- 1
df[df$TieBreak == 1,2]
# Convert players into numeric ids for stata

#Score difference dataframe, convert into score differences
scoreDf <- data.frame("P1Score" = c(0,0,0,0,15,15,15,15,30,30,30,30,40,40,40,40,40,"AD"),
                      "P2Score" = c(0,15,30,40,0,15,30,40,0,15,30,40,0,15,30,40,"AD",40),
                      "P1ScoreDiff" = c(0,-1,-2,-3,1,0,-1,-2,2,1,0,-1,3,2,1,0,-1,1), stringsAsFactors = FALSE)
scoreDf$scorePaste <- paste0(scoreDf$P1Score, scoreDf$P2Score)
scoreDf <- scoreDf[,c(4,3)]

temp1 <- as.character(df$P1ScorePreServe)
temp2 <- as.character(df$P2ScorePreServe)
temp3 <- paste0(temp1, temp2)
scorePaste <- data.frame(scorePaste = temp3)
scorePaste <- join(scorePaste, scoreDf)
df$P1ScoreDiff <- scorePaste$P1ScoreDiff
rm(temp, temp1, temp2, temp3, from, i, to)

# These look like tiebreakers even though they aren't US open
df[is.na(df$P1ScoreDiff),'TieBreak'] <- 1

df[is.na(df$P1ScoreDiff),'P1ScoreDiff'] <- as.numeric(as.character(df[is.na(df$P1ScoreDiff),c('P1Score')])) - as.numeric(as.character(df[is.na(df$P1ScoreDiff),c('P2Score')]))
df[df$P1ScoreDiff == 15,'P1ScoreDiff'] <- 1
df[df$P1ScoreDiff == -15,'P1ScoreDiff'] <- -1

## Fix score from server's perspective and fix more of the NAs for serve numbers
df$ServerScoreDiff <- df$P1ScoreDiff
df$ServerScoreDiff <- df$P1ScoreDiff * ifelse(df$PointServer == 2, -1, 1)
df[df$ServeNumber == 1 & !is.na(df$ServeNumber), 'FirstServe'] <- 1
df[df$ServeNumber == 1 & !is.na(df$ServeNumber), 'SecondServe'] <- 0
df[df$ServeNumber == 2 & !is.na(df$ServeNumber), 'FirstServe'] <- 0
df[df$ServeNumber == 2 & !is.na(df$ServeNumber), 'SecondServe'] <- 1
df[df$ServeNumber == 0 & !is.na(df$ServeNumber), 'FirstServe'] <- 0
df[df$ServeNumber == 0 & !is.na(df$ServeNumber), 'SecondServe'] <- 0

## Fix game and set difference from server's persective

df$ServerGameDiffPreServe <- df$P1GameDiffPreServe * ifelse(df$PointServer == 2, -1, 1)

df$ServerSetDiff <- df$P1SetDiff * ifelse(df$PointServer == 2, -1, 1)


## Unfortunately there is no indication of serve number for about a 
# fourth of the Opens
df1 <- df[!is.na(df$SecondServe),]

write.csv(df, "SlamALLdata.csv")
write.csv(df1, "SlamServesData.csv")

# df1 <- read.csv("SlamServesData.csv")
