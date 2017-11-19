install.packages("robertzk/s3mpi")
install.packages("https://github.com/robertzk")

devtools::install_github('avantcredit/datainsights', subdir='R-package')
install.packages("robertzk")
devtools::install_github(c('robertzk/mungebits2'))
devtools::install_github('robertzk/allthepackages')
devtools::install_github("robertzk/s3mpi")
devtools::install_github("christiantillich/AnaliTools")

devtools::install_github("avant/rubystore")
devtools::install_github("avant")
install.packages("avant/rubystore")
library(rubystore)
library(avant/rubystore)

devtools::install_github("gbm-developers/gbm3",ref="3410a969090d3d2b2d48f3594bbd251a2287b843")
devtools::install_github("gbm-developers/gbm3")

###clean up 
rm(list = ls()[grep("^tmp", ls())])
rm(list = ls()[grep("^ds", ls())])
rm(list = ls()[grep("^df", ls())])
rm(list = ls()[grep("^dd", ls())])
rm(list = ls()[grep("^stats", ls())])
rm(list = ls()[grep("^leads", ls())])
rm(list = ls()[grep("^loan", ls())])
rm(list = ls()[grep("^good", ls())])
rm(list = ls()[grep("^bk_", ls())])
rm(list = ls()[grep("^tu", ls())])
rm(list = ls()[grep("^default", ls())])
rm(list = ls()[grep("^iris", ls())])
rm(list = ls()[grep("^all", ls())])
rm(list = ls()[grep("^train", ls())])


#############TEST DATA
a <-  c(1:5)
b <- c("Cat", "Dog", "Rabbit", "Cat", "Dog")
c <- c("Dog", "Rabbit", "Cat", "Dog", "Dog")
d <- c("Rabbit", "Cat", "Dog", "Dog", "Rabbit")
e <- c("Cat", "Dog", "Dog", "Rabbit", "Cat")
f <- c("Cat", "Dog", "Dog", "Rabbit", "Cat")

df <- data.frame(a,b,c,d,e,f)
remove(a)
remove(b)
remove(c)
remove(d)
remove(e)


######### CODE EXAMPLES

ds$model_decline_score_tiers_current <- 
  unlist(sapply(ds$model_decline_score, function(score) {
    if (score < 0.050) "b1" 
    else if (score >= 0.050 & score < 0.055) "b2"
    else if (score >= 0.055 & score < 0.060) "b3"
    else if (score >= 0.060 & score < 0.065) "b4"
    else if (score >= 0.065 & score < 0.070) "b5"
    else if (score >= 0.070 & score < 0.075) "c1"
    else if (score >= 0.075 & score < 0.080) "c2"
    else if (score >= 0.080 & score < 0.090) "c3"
    else if (score >= 0.090 & score < 0.100) "d1"
    else if (score >= 0.100 & score < 0.110) "d2"
    else if (score >= 0.110 & score < 0.120) "d3"
    else "decline"
  }))

#dont use this - not matching what is in production - credit decisions 
ds$model_decline_score_tiers_current <- 
  unlist(sapply(ds$model_decline_score, function(score) {
    if (score < 0.050) "a" 
    else if (score >= 0.050 & score < 0.070) "b" 
    else if (score >= 0.070 & score < 0.090) "c"
    else if (score >= 0.090 & score < 0.120) "d"
    else "decline"
  }))

binFICO <- c(0,350, 450, 550, 580, 620, 660, 720, 780, 850,1000)
ALPay$FICOBin <- cut(ALPay$fico_score, breaks = binFICO)


binBreaks = c(0,10,50,75,100,110)
labelNames = c(-1,0,1,2,3)
x <- stats::rnorm(100) #random numbers, different every time
c <- cut(x,breaks=binBreaks,dig.lab=2
         ,labels=labelNames)
summary(c)

#Example code/snippets
## rename all vars
colnames(ds_pre) <- paste0("FT_",colnames(ds_pre))




q <- paste0("SELECT id from loans limit 10")
dsDBTMP <- dbGetQuery(con, q)

q <- paste0("SELECT customer_id,max(id) as loan_id, max(tzcorrect(created_at)) as 
            max_created_at from loans where customer_id 
            in (",paste("",as.character(ds$customer_id),"",collapse=", ",sep=""),") group by 1")
dsDateCheck <- dbGetQuery(con, q)
summary(dsDateCheck)

q <- paste0("
            select min(cms.created_at),max(cms.created_at)
            from
            credit_model_scores cms
            inner join modeling_processor_logs mpl on mpl.cache_id = cms.cache_id
            inner join model_bridges mb on mb.model_id = cms.credit_model_id
            inner join credit_decisions cd on cd.id = cms.scorable_id
            where mb.version = 'default/en-US/4.3.0 '
            and cd.approved
            and cms.id = (select max(id) from credit_model_scores
            where customer_id = cms.customer_id and credit_model_id = mb.model_id)
            and cms.customer_id in (",paste("",as.character(ds$customer_id),"",collapse=", ",sep=""),")") 


######################## XML Examples


xml.text <- 
  '<?xml version="1.0" encoding="utf-8"?>
<posts>  
<row Id="1" PostTypeId="1" AcceptedAnswerId="17" CreationDate="2010-07-26T19:14:18.907" Score="6"/>
<row Id="2" PostTypeId="1" AcceptedAnswerId="17" CreationDate="2010-07-26T19:14:18.907" Score="6"/>
<row Id="3" PostTypeId="1" AcceptedAnswerId="17" CreationDate="2010-07-26T19:14:18.907" Score="6"/>
<row Id="4" PostTypeId="1" AcceptedAnswerId="17" CreationDate="2010-07-26T19:14:18.907" Score="6"/>
</posts>'

library(XML)
xml <- xmlParse(xml.text)
result <- as.data.frame(t(xmlSApply(xml["/posts/row"],xmlAttrs)),
                        stringsAsFactors=FALSE)


xml.text <- '
<inquiry>
<type>IN11</type>
<length>084</length>
<subscriber_prefix>33PC</subscriber_prefix>
<industry_code>BC</industry_code>
<member_code>02699824</member_code>
<subscriber_name>CAP ONE</subscriber_name>
<ecoa_designator>I</ecoa_designator>
<inquiry_date>20160216</inquiry_date>
</inquiry>
<inquiry>
<type>IN11</type>
<length>084</length>
<subscriber_prefix>33PC</subscriber_prefix>
<industry_code>BC</industry_code>
<member_code>02699824</member_code>
<subscriber_name>CAP ONE</subscriber_name>
<ecoa_designator>I</ecoa_designator>
<inquiry_date>20160216</inquiry_date>
</inquiry>
'

xml <- xmlParse(xml.text)
xml
xml1<- xmlToDataFrame(xml)

result <- as.data.frame(t(xmlSApply(xml["/posts/row"],xmlAttrs)),
                        stringsAsFactors=FALSE)

xml1
ds_ic<- xmlToDataFrame(getNodeSet(xml, "//industry_code"))


result <- as.data.frame(t(xmlSApply(xmlToDataFrame(getNodeSet(xml, "//industry_code")),xmlAttrs)),
                        stringsAsFactors=FALSE)

#### Connecting to Data Lake from R Studio


##remove comma from string 
var1 <- as.data.frame(c("50,0", "72,0", "960,0", "1.920,0", "50,0", "50,0", "960,0"))
names(var1) <- c("x")
var1$x2<-as.factor(gsub(',','',var1$x))


#time diffs
ds$p1_created_at <- as.POSIXct(ds$p1_created_at, format = "%Y-%m-%d %H:%M:%S") 
ds$p2_created_at <- as.POSIXct(ds$p2_created_at, format = "%Y-%m-%d %H:%M:%S") 
ds$diff_created_at_hr <- round(as.numeric(ds$p2_created_at - ds$p1_created_at, units = "hours"),0)
ds$diff_created_at_days <- round(as.numeric(ds$p2_created_at - ds$p1_created_at, units = "days"),0)












##ANALYSIS of different rules
#instantiate with the dataset used for analysis, with default indicator
ds_R<- ds_forR
ds_R$defaultIndicator <- ds_R$F3Def30Flag

#build rule - check the rating variable 
ds_R$newRule <- ifelse(ds_R$ld_fico_8>600,"GT600","LE600")
ds_R$newRule <- ifelse(ds_R$diff_credit_limit_amount>100,"diff_cla_gt_100","diff_cla_le_100")

ds_R$newRulewRating <- paste0(ds_R$newRating,sep="_",ds_R$newRule) 

#run analysis -- NO CHANGE BELOW
ds_analysis_2 <- select(ds_R,newRating,defaultIndicator)%>%
  group_by(
    newRating
  ) %>%
  summarise_each(funs(
    baseline_default = mean(.,na.rm=TRUE)
    ,baseline_volume = n()/nrow(ds)
  )) %>% 
  as.data.frame(.)

ds_analysis_2
remove(ds_analysis_1)
ds_analysis_1 <- select(ds_R,newRulewRating,newRating,defaultIndicator,ld_fico_8)%>%
  group_by(
    newRulewRating,newRating
  ) %>%
  summarise_each(funs(
    mean = mean(.,na.rm=TRUE)
  )) %>% 
  as.data.frame(.)
remove(ds_analysis)
ds_analysis <- select(ds_R,newRulewRating,newRating,1)%>%
  group_by(
    newRulewRating,newRating
  ) %>%
  summarise_each(funs(
    loanCount = n()
    ,loanPerc = n()/nrow(ds)
  )) %>% 
  as.data.frame(.)
ds_analysis <- left_join(ds_analysis,ds_analysis_1,by=c("newRulewRating"="newRulewRating","newRating"="newRating"))
ds_analysis$defaultIndicator_mean <- round(ds_analysis$defaultIndicator_mean*100,2)
ds_analysis$loanPerc <- round(ds_analysis$loanPerc*100,2)
ds_analysis <- left_join(ds_analysis,ds_analysis_2,by = c("newRating"="newRating"))
ds_analysis$baseline_default <- round(ds_analysis$baseline_default*100,2)
ds_analysis$baseline_volume <- round(ds_analysis$baseline_volume*100,2)
ds_analysis$delta_default_bps <- round((ds_analysis$baseline_default-ds_analysis$defaultIndicator_mean)*100,2)
#ds_analysis$delta_volume_bps <- round((ds_analysis$baseline_volume-ds_analysis$loanPerc)*100,2)
ds_analysis <- select(ds_analysis,newRulewRating,loanCount,loanPerc,baseline_volume,defaultIndicator_mean,delta_default_bps,baseline_default)
ds_analysis
write.csv(ds_analysis,row.names=F)



###########EXAMPLE - Do group by quantile ranges.
clientID <- round(runif(200,min=2000, max=3000),0)
orders <- round(runif(200,min=1, max=50),0)

df <- df <- data.frame(cbind(clientID,orders))

#function to break them into quintiles
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(df$orders, probs = seq(0, 1, by = 0.20)))
      #,labels=c("0-20","20-40","40-60","60-80","80-100")
  )
}

#Add the quintile to the dataframe
df$Quintile <- sapply(df$orders, ApplyQuintiles)
tmpStats <- df %>% group_by(Quintile) %>%
  summarise_each(funs(
    baseline_default = mean(.,na.rm=TRUE)
    ,baseline_volume = n()/nrow(df)
  )) %>% 
  as.data.frame(.)
tmpStats


####ungroup

original_data <- s3mpi::s3read('tu_acct_mgmt/2017_06/data_20170623', 's3://avant-data-insights/') %>%
  dplyr::select(loan_id, customer_id) %>%
  dplyr::group_by(customer_id) %>%
  dplyr::filter(loan_id == max(loan_id)) %>% 
  ungroup


ds <- ds %>%
 dplyr::group_by(customer_id) %>%
  dplyr::filter(loan_id == max(loan_id)) %>% 
  ungroup

