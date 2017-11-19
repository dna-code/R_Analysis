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
stats <- dplyr::full_join(stats,stats_mean,by=c("Variable"="Variable"))

remove(stats_mean)
df$groupBy <- df$grade
stats_mean <- data_groupBy_summary(df)
stats <- dplyr::full_join(stats,stats_mean,by=c("Variable"="Variable"))

#function 3 <- select top variables
stats <- stats %>% dplyr::mutate_all(funs(type.convert(as.character(.))))
stats$keyVariable <- ifelse(  (stats$perc_missing+stats$perc_zero) < 0.8
                              & stats$CoeffOfVariance.x > 0.05
                                ,"Yes","No")
stats$Variable1 <- stats$Variable

#######################
#Calculate Information Value
stats_iv <- iv.mult(df,"target",TRUE)
iv.plot.summary(stats_iv)

stats <- dplyr::full_join(stats,stats_iv,by=c("Variable"="Variable"))

keyVariableList <- select(filter(stats,keyVariable=="Yes" | Strength=="Strong"),Variable)
keyVariableList <- as.array(keyVariableList$Variable)

options(digits=2)
stats_woe <- iv.mult(df,"target",vars=keyVariableList)
stats_woe1<- as.data.frame(stats_woe)

iv.plot.woe(iv.mult(df,"target",vars=keyVariableList,summary=FALSE))


#### OLD CODE
stats_groupBy <- df %>% 
  group_by(groupBy) %>%   ##group by if needed
  summarise_all(funs(count=n()
                      ,mean(.,na.rm=TRUE)
                      ,median(., na.rm=TRUE)
                      ,std_dev = sd(.,na.rm=T)
  ))


stats_groupBy <- df %>% 
  group_by(groupBy) %>%   ##group by if needed
  summarise_each(funs(min(., na.rm=TRUE),mean(., na.rm=TRUE)
                      ,median(., na.rm=TRUE)
                      ,max(., na.rm=TRUE)
                      ,count=n()
                      ,q_05 = quantile(.,probs = 0.05,na.rm=T)
                      ,q_25 = quantile(.,probs = 0.25,na.rm=T)
                      ,q_50 = quantile(.,probs = 0.5,na.rm=T)
                      ,q_75 = quantile(.,probs = 0.75,na.rm=T)
                      ,q_95 = quantile(.,probs = 0.95,na.rm=T)  
                      ,count_missing = sum(is.na(.), na.rm = TRUE)
                      ,count_zeros   = sum(abs(.) <= 0.000001, na.rm=TRUE)
                      ,count_neg_spl   =sum(.<0 & .>-10 , na.rm=TRUE)
                      ,count_999_spl   =sum(.==999 , na.rm=TRUE)
                      ,num_unique  = length(unique(.[!is.na(.)], na.rm = TRUE))
                      ,std_dev = sd(.,na.rm=T)
  ))

stats$count <- as.numeric(as.character(stats$count))
stats$perc_missing <- as.numeric(as.character(stats$count_missing))/stats$count
stats$perc_zero <- as.numeric(as.character(stats$count_zeros))/stats$count
stats$perc_neg_spl <- as.numeric(as.character(stats$count_neg_spl))/stats$count
stats$perc_999_spl <- as.numeric(as.character(stats$count_999_spl))/stats$count


