library(tidyverse)
library(readxl)
library(tidyverse)
library(ggimage)
library(gt)
library(ggthemes)
library(qs)
library(dplyr)

install.packages("nFactors")
install.packages("fmsb")

library(nFactors)
library(fmsb)

# read in mlb baseball savant data
mlb <- read_excel("C:/Users/student/Downloads/mlb_stats.xlsx")

# read in milb baseball savant data
milb <- read_excel("C:/Users/student/Downloads/minor_stats.xlsx")

# select columns to standardize for mlb and milb
mlb<-select(mlb,"Player","League","Contact","Power",
             "Luckiness",
             "Strikeout Prone", "Run Potential", "Patience", "Well-Rounded", "Efficiency")

milb<-select(milb,"Player","League","Contact","Power",
            "Luckiness",
            "Strikeout Prone", "Run Potential", "Patience", "Well-Rounded", "Efficiency")

# standardize mlb columns 3 through 10
mlb_standardized <- mlb
mlb_standardized[, 3:10] <- scale(mlb[, 3:10])

# transform to range [1, 100]
normalize <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  1 + 99 * (x - min_x) / (max_x - min_x)
}

mlb_standardized[, 3:10] <- apply(mlb_standardized[, 3:10], 2, normalize)

# standardize milb columns 3 through 10
milb_standardized <- milb
milb_standardized[, 3:10] <- scale(milb[, 3:10])

# transform to range [1, 100]
normalize <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  1 + 99 * (x - min_x) / (max_x - min_x)
}

milb_standardized[, 3:10] <- apply(milb_standardized[, 3:10], 2, normalize)

# combine mlb and milb data
df <- rbind(mlb_standardized, milb_standardized)

# get eigenvalues  
ev <- eigen(cor(df[,-c(1,2)]))  


ap <- parallel(subject=nrow(df[,-c(1,2)]),var=ncol(df[,-c(1,2)]),
               rep=100,cent=.05)

nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

#Look at number of factors
plotnScree(nS)

# perform factor analysis
res = factanal(df[,-c(1,2)],4,scores = "regression")

# view results
res

# look at loadings
loadings(res,sort=TRUE)

# look at uniqueness
res$uniquenesses

# extract factor scores and combine with Name and League
df_scores <- cbind.data.frame(Name = df[, "Player"], res$scores, League = df[, "League"])

# check example of similar players
Similarity <- 1-(rowSums(abs(sweep(df_scores[,2:5],2,unlist(df_scores[df_scores["Player"]=="Duran, Jarren",2:5]))))/
                   max(rowSums(abs(sweep(df_scores[,2:5],2,unlist(df_scores[df_scores["Player"]=="Duran, Jarren",2:5]))))))

Similarity <- cbind.data.frame(Player = df_scores[,1],Similarity)

SimilarityResult <- cbind(df_scores, Similarity)

# get rid of columns we do not need
SimilarityResult <- SimilarityResult[, -c(2, 3, 4, 5, 7)]

# order by similarity
SimilarityResult<-SimilarityResult[with(SimilarityResult, order(-Similarity)), ]

# order by league
SimilarityResult <- SimilarityResult[SimilarityResult$League == "MLB", ]

# look at most similar players
SimilarityResult[1:10,]

# define the names you want to filter for
names_to_filter <- c("Mayo, Coby MLB #19 / #BAL #3", "Devers, Rafael")

# clean up player name
df <- df %>%
  mutate(Player = case_when(
    Player == "Mayo, Coby MLB #19 / #BAL #3" ~ "Coby Mayo",
    Player == "Devers, Rafael" ~ "Rafael Devers",
    TRUE ~ Player
  ))

# define the names you want to filter for
names_to_filter <- c("Coby Mayo", "Rafael Devers")

# filter the dataframe
comparison <- df[df$Player %in% names_to_filter, ]
