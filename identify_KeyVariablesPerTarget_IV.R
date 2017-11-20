#install.packages("Devtools")
library(devtools)
#install_github("riv","tomasgreif")
library(woe)

ds_FULL <- readRDS("/Users/ndeena/Downloads/loan_data_ch1.rds", refhook = NULL)
View(ds_FULL)

df <- ds_FULL
df <- rename(df, target = loan_status)
#function 1 - overall stats
remove(stats)
stats <- data_summary(df)
stats

#function 2 
remove(stats_mean)
df$groupBy <- df$target
stats_mean <- data_groupBy_summary(df)
names(stats_mean) <- c("low_mean","high_mean","CoeffOfVariance_target","Variable")
#special case
stats_mean$ratio_good_bad_mean <- ifelse(stats_mean$high_mean!=0,stats_mean$low_mean/stats_mean$high_mean,NA)
stats <- dplyr::full_join(stats,stats_mean,by=c("Variable"="Variable"))

remove(stats_mean)
df$groupBy <- df$grade
stats_mean <- data_groupBy_summary(df)
stats <- dplyr::full_join(stats,stats_mean,by=c("Variable"="Variable"))

#function 3 <- select top variables
stats <- stats %>% dplyr::mutate_all(funs(type.convert(as.character(.))))
stats$keyVariable <- ifelse(  ((stats$perc_missing+stats$perc_zero) < 0.8
                              & stats$CoeffOfVariance_target > 0.05)
                              | stats$ratio_good_bad_mean > 1.5 
                              | stats$ratio_good_bad_mean < 0.5
                                ,"Yes","No"
                           )

stats$Variable1 <- stats$Variable

#######################
#Calculate Information Value
stats_iv <- iv.mult(df,"target",TRUE)
iv.plot.summary(stats_iv)

stats <- dplyr::full_join(stats,stats_iv,by=c("Variable"="Variable"))
remove(stats_iv)
keyVariableList <- select(filter(stats,keyVariable=="Yes" | Strength=="Strong"),Variable)
keyVariableList <- as.array(keyVariableList$Variable)

options(digits=2)
stats_woe <- iv.mult(df,"target",vars=keyVariableList)

iv.plot.woe(iv.mult(df,"target",vars=keyVariableList,summary=FALSE))

stats_woe_ds <-stats_woe[1][[1]]
if("sql" %in% colnames(stats_woe_ds))
{
  stats_woe_ds <- select(stats_woe_ds,-sql)
}
for (i in 2:length(stats_woe))
{
  if("sql" %in% colnames(stats_woe[i][[1]]))
  {
    stats_woe[i][[1]] <- select(stats_woe[i][[1]],-sql)
  }
  stats_woe_ds <- rbind(stats_woe_ds,stats_woe[i][[1]])
}
totalPop <- nrow(df)
stats_woe_ds$groupPerc <- (stats_woe_ds$outcome_0+stats_woe_ds$outcome_1)/totalPop
stats_woe_ds <- dplyr::left_join(stats_woe_ds
                                 ,select(stats,Variable,InformationValue,CoeffOfVariance_target,ratio_good_bad_mean)
                                 ,by=c("variable"="Variable"))
remove(stats_woe)
write.csv(stats,row.names=F)
write.csv(stats_woe_ds,row.names=F)
