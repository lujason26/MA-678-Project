library(dplyr)
library(ggplot2)
library(arm)
library(data.table)

setwd("C:/Users/lujas/Desktop/MA 678")
matches <- read.csv("matches2020.csv")
stats <- read.csv("champion_stats.csv")
stats[stats$id=="Qiyana","attack"] <- 8 # this value was 0, definitely wrong

# meta was aggressive laners and supportive ADC's alongside CC supports

######
# cleaning

# separating by side with corresponding result
blue_side <- subset(matches, select = c("league","blueteam","bluetop","bluejungle","bluemid","blueadc","bluesupport","result"))
blue_side$side <- "blue"
red_side <- subset(matches, select = c("league","redteam","redtop","redjungle","redmid","redadc","redsupport","result"))
red_side$result <- ifelse(red_side$result==1,0,1)
red_side$side <- "red"

# all together
colnames(blue_side) <- c("league","team","top","jungle","mid","adc","support","result","side")
colnames(red_side) <- c("league","team","top","jungle","mid","adc","support","result","side")
all_games <- rbind(blue_side,red_side)
all_games$team <- as.factor(all_games$team)

###
# Cut down the teams to the major regions only 
all_games <- subset(all_games, league=="LPL" | league=="LCS" | league=="LCK" | league=="LEC")

###
# make 'NA' with champion play count <= 10 for EACH region (since champ being in the game affects everyone)
count_graphics_by_region <- function(all_games, colname) {
  merged = paste(all_games[,which(colnames(all_games) == colname)], all_games$league, sep="_")
  counts = table(merged)
  merged_frame = data.frame(counts)
  sorted_merged_frame = merged_frame %>% arrange(desc(Freq))
  split_frame <- data.frame(do.call('rbind', strsplit(as.character(sorted_merged_frame$merged),'_',fixed=TRUE)))
  sorted_merged_frame$champ = split_frame$X1
  sorted_merged_frame$region = split_frame$X2
  sorted_merged_frame
}
# View(count_graphics_by_region(all_games, "support"))

replace_champ_with_na <- function(all_games, colname) {  # this one is each region, see if under 10 and NA them
  play_freq <- count_graphics_by_region(all_games, colname)
  rare_champs <- play_freq[play_freq$Freq <= 10,]
  rare_champs
  new_df <- all_games
  
  for (i in 1:nrow(rare_champs)) {
    curr_champ = rare_champs[i, 3]
    curr_region = rare_champs[i, 4]
    for (j in 1:nrow(all_games)) {
      if ((all_games[j, which(colnames(all_games) == colname)] == curr_champ) & (all_games[j, 1] == curr_region)) {
        new_df[j, which(colnames(all_games) == colname)] <- NA
      }
    }
  }
  # print(rare_champs)
  new_df # saves as an ENTIRE new data frame, not just column
}
# new_data <- replace_champ_with_na(all_games,"top")
# View(count_graphics(new_data$top))

all_games$top <- replace_champ_with_na(all_games,"top")$top
all_games$jungle <- replace_champ_with_na(all_games,"jungle")$jungle
all_games$mid <- replace_champ_with_na(all_games,"mid")$mid
all_games$adc <- replace_champ_with_na(all_games,"adc")$adc
all_games$support <- replace_champ_with_na(all_games,"support")$support

###### creates average of team stats #####
get_champ_stats <- function(name) {                                  # links champion name to their stats
  champ <- stats[which(stats$id==name),]
}

get_sum <- function(top, jungle, mid, adc, support, index) {
  sum = 0
  if (nrow(top) > 0) {sum = sum + top[index]}
  if (nrow(jungle) > 0) {sum = sum + jungle[index]}
  if (nrow(mid) > 0) {sum = sum + mid[index]}
  if (nrow(adc) > 0) {sum = sum + adc[index]}
  if (nrow(support) > 0) {sum = sum + support[index]}
  return(sum)
}

get_num_non_zero <- function(top, jungle, mid, adc, support) {        # this indexes non 0's to divide by
  return(as.numeric(nrow(top) > 0) + as.numeric(nrow(jungle) > 0) + 
           as.numeric(nrow(mid) > 0) + as.numeric(nrow(adc) > 0) + 
           as.numeric(nrow(support) > 0))
}

get_average <- function(top, jungle, mid, adc, support, index) {
  return (get_sum(top, jungle, mid, adc, support, index) / 
            get_num_non_zero(top, jungle, mid, adc, support))
}

for(i in 1:length(all_games$team)) {
  top <- get_champ_stats(all_games$top[i])
  jungle <- get_champ_stats(all_games$jungle[i])
  mid <- get_champ_stats(all_games$mid[i])
  adc <- get_champ_stats(all_games$adc[i])
  support <- get_champ_stats(all_games$support[i])
  
  ave_attack <- get_average(top,jungle,mid,adc,support,4)
  ave_defense <- get_average(top,jungle,mid,adc,support,5)
  ave_magic <- get_average(top,jungle,mid,adc,support,6)
  ave_difficulty <- get_average(top,jungle,mid,adc,support,7)
  
  all_games$ave_attack[i] <- as.numeric(ave_attack)
  all_games$ave_defense[i] <- as.numeric(ave_defense)
  all_games$ave_magic[i] <- as.numeric(ave_magic)
  all_games$ave_difficulty[i] <- as.numeric(ave_difficulty)
}
# View(all_games)

###### creates Win Rates #####
regions <- c("LPL","LCK","LCS","LEC")

## win rate
win_rate <- function(name,column_name) {
  #all_games <- unique(all_games[,c(1,5)])
  wr_table <- data.frame()
  for(region in regions){
    temp <- all_games[all_games$league == region,]

    champ <- temp[temp[,which(colnames(temp)==column_name)]==name,] #gets all rows whose champ name is in the column
    champ <- champ[complete.cases(champ),] # it keeps every NA, so we're removing those here
      
    sum <- sum(champ$result)
    length <- length(champ$result)
    wr <- mean(champ$result)

    wr_table <- rbind(wr_table, c(sum,length,wr,region))
  
  }
  names <- c("Games Won","Total Games","Win Rate","Region")
  setnames(wr_table,names)
  wr_table
}
win_rate("Aatrox","top") #spits out win rate for each region

# calculate win rate for each champion in SPECIFIC role                           (ex. Ornn mid and Ornn top are 2 different rows)
wr_top <- data.frame()
wr_mid <- data.frame()
wr_jungle <- data.frame()
wr_adc <- data.frame()
wr_support <- data.frame()
for(i in 1:length(all_games$team)) {        # takes a while (60 s)
  top <- win_rate(all_games$top[i],"top")
  top <- cbind(all_games$top[i],top)
  wr_top <- rbind(wr_top,top)
  
  mid <- win_rate(all_games$mid[i],"mid")
  mid <- cbind(all_games$mid[i],mid)
  wr_mid <- rbind(wr_mid,mid)

  jungle <- win_rate(all_games$jungle[i],"jungle")
  jungle <- cbind(all_games$jungle[i],jungle)
  wr_jungle <- rbind(wr_jungle,jungle)

  adc <- win_rate(all_games$adc[i],"adc")
  adc <- cbind(all_games$adc[i],adc)
  wr_adc <- rbind(wr_adc,adc)

  support <- win_rate(all_games$support[i],"support")
  support <- cbind(all_games$support[i],support)
  wr_support <- rbind(wr_support,support)

}
names <- c("Champion", "Games Won", "Total Games","Win Rate","Region")
wr_top <- unique(wr_top)
  setnames(wr_top,names)
wr_jungle <- unique(wr_jungle)
  setnames(wr_jungle,names)
wr_mid <- unique(wr_mid)
  setnames(wr_mid,names)
wr_adc <- unique(wr_adc)
  setnames(wr_adc,names)
wr_support <- unique(wr_support)
  setnames(wr_support,names)
wr_all <- data.frame()

# win rate in general (across all roles, not grouped into one champion per region)
wr_all <- rbind(wr_top,wr_jungle,wr_mid,wr_adc,wr_support)
wr_all <- wr_all[complete.cases(wr_all),]

# Cumulative win rate for each champion (NOT role SPECIFIC) by region             (ex. Ornn mid and top are now combined)
merge_wr_region <- function(wr_all) {         #takes like 30 s
  new_df <- unique(wr_all[,c(1,5)])
  final_df <- data.frame()
  for (i in 1:nrow(new_df)) {
    champ <- new_df[i, 1]
    region <- new_df[i, 2]
    
    games_won_cntr = 0
    total_games_cntr = 0
    
    for (j in 1:nrow(wr_all)) {
      row <- wr_all[j,]
      if (row[1] == champ & row[5] == region) {
        games_won_cntr <- games_won_cntr + as.integer(row[2])
        total_games_cntr <- total_games_cntr + as.integer(row[3])
      }
    }
    
    wr <- games_won_cntr / total_games_cntr
    final_df <- rbind(final_df, c(champ, games_won_cntr, total_games_cntr, wr, region))
  }
  names <- c("Champion", "Games Won", "Total Games","Win Rate","Region")
  setnames(final_df,names)
  final_df
}
wr_all_by_region <- merge_wr_region(wr_all) # gives overall champion win rate by region

# Cumulative win rate for each champion (not role specific) (not by region)       (ex. All Ornn's from every region are combined in one observation)
merge_wr_all_general <- function(wr_all) {         #takes like 30 s
  new_df <- unique(wr_all[,c(1,5)])
  final_df <- data.frame()
  for (i in 1:nrow(new_df)) {
    champ <- new_df[i, 1]
    
    games_won_cntr = 0
    total_games_cntr = 0
    
    for (j in 1:nrow(wr_all)) {
      row <- wr_all[j,]
      if (row[1] == champ) {
        games_won_cntr <- games_won_cntr + as.integer(row[2])
        total_games_cntr <- total_games_cntr + as.integer(row[3])
      }
    }
    
    wr <- games_won_cntr / total_games_cntr
    final_df <- rbind(final_df, c(champ, games_won_cntr, total_games_cntr, wr))
  }
  names <- c("Champion", "Games Won", "Total Games","Win Rate")
  setnames(final_df,names)
  final_df
}
wr_overall <- merge_wr_all_general(wr_all)
wr_overall <- unique(wr_overall)

### Now I should have 3 datasets, one w/r by region (wr_all_by_region)
###                             , one w/r overall (wr_overall)
###                             , and all_games with NA's for champions under 10 freq. in each region (the NEW "all_games")


###### EDA #####

## EDA champ play rate in general (top 15)
##     then their win rates in same order as champ freq played

wr_overall$`Games Won` <- as.numeric(wr_overall$`Games Won`)
wr_overall$`Total Games` <- as.numeric(wr_overall$`Total Games`)
wr_overall$`Win Rate` <- as.numeric(wr_overall$`Win Rate`)
# write.csv(wr_all_by_region, "C:/Users/lujas/Desktop/MA 678/wr_all_by_region.csv")
# write.csv(wr_overall, "C:/Users/lujas/Desktop/MA 678/wr_overall.csv")

plot_1 <- top_n(wr_overall, n=15, `Total Games`) %>%
  ggplot(., aes(x=reorder(Champion,-`Total Games`), y=`Total Games`, fill=Champion)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Most Played Champions") +
  theme(legend.position="none")
plot_1

# wr_overall %>% arrange(-`Total Games`) %>% top_n(`Total Games`, n=15)

plot_2 <- wr_overall %>% arrange(-`Total Games`) %>% top_n(`Total Games`, n=15) %>%
  ggplot(., aes(x=reorder(Champion,-`Total Games`), y=`Win Rate`, fill=Champion)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = 0.5,color="darkorchid", size=3) +
  xlab("Most Played Champions Win Rates") +
  theme(legend.position="none")
plot_2

## 4 Regions play rate champions see if they are similar (so 4 plots )
LCS <- subset(wr_all_by_region, Region=="LCS")
LPL <- subset(wr_all_by_region, Region=="LPL")
LCK <- subset(wr_all_by_region, Region=="LCK")
LEC <- subset(wr_all_by_region, Region=="LEC")

LCS_champs <- top_n(LCS, n=15, Total.Games) %>%
  ggplot(., aes(x=reorder(Champion,-Total.Games), y=Total.Games, fill=Champion)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Most Played Champions") +
  theme(legend.position="none")
LCS_champs

LPL_champs <- top_n(LPL, n=15, Total.Games) %>%
  ggplot(., aes(x=reorder(Champion,-Total.Games), y=Total.Games, fill=Champion)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Most Played Champions") +
  theme(legend.position="none")
LPL_champs

LCK_champs <- top_n(LCK, n=15, Total.Games) %>%
  ggplot(., aes(x=reorder(Champion,-Total.Games), y=Total.Games, fill=Champion)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Most Played Champions") +
  theme(legend.position="none")
LCK_champs

LEC_champs <- top_n(LEC, n=15, Total.Games) %>%
  ggplot(., aes(x=reorder(Champion,-Total.Games), y=Total.Games, fill=Champion)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Most Played Champions") +
  theme(legend.position="none")
LEC_champs

# boxplot of champion stats (just use stats csv)
boxplot(stats$attack, stats$defense, stats$magic, stats$difficulty, names=c("Attack","Defense","Magic","Difficulty"))


######
# model
# write.csv(all_games, "C:/Users/lujas/Desktop/MA 678/final_data.csv")

model_stats <- glmer(result ~ ave_attack + ave_defense + ave_magic + ave_difficulty + (1|team), 
                     family=binomial(link="logit"), data=all_games)
summary(model_stats)
ranef(model_stats)

predictors <- fixef(model_stats)
ci <- confint(model_stats)

# combining into one table
ci <- ci[-1,]
model_table <- data.frame(predictor=rownames(ci[,0]),
                          coefficient=predictors,
                          lci=ci[,1],
                          uci=ci[,2])

ggplot(model_table[-1,],aes(y=predictor,x=coefficient,xmin=lci,xmax=uci)) + theme_bw() +
  geom_point() + geom_text(aes(label=round(coefficient,digits=3)), hjust=0.5,vjust=-1, digits=3) + 
  geom_errorbarh(height=0) + geom_vline(xintercept = 0, color="purple")


# binned residuals
# plot(c(0,1), c(-1,1), xlab="Estimated Pr (win)", ylab="Observed - estimated",
#      type="n", main="Residual plot", mgp=c(2,.5,0))
# abline(0,0, col="gray", lwd=.5)
# points(fitted(model_stats), all_games$result - fitted(model_stats), pch=20, cex=.2)
binnedplot(fitted(model_stats), resid(model_stats))

######
# plug in champions for output, as well as optional team / side
# write function to take in champs and put stat numbers in model
newdata = data.frame(top='Sion',jungle='Gragas',mid='Kled',adc='Varus',support='Thresh') #can't put a champion not already there
predict(model_0, newdata=newdata,type="response") ## outputs probability
