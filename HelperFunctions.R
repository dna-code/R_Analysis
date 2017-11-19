
library(dplyr)
#library(xlsx)
#install.packages('rjson')
#library(rjson)
#library(googlesheets)
library(reshape2)
#library(OneR)
#library(s3mpi)
library(readr)

#install.packages("Devtools")
library(devtools)
#install_github("riv","tomasgreif")
library(woe)

string_pad <- function(x, width = 8, pad = ' ') {
  x <- as.character(x)
  stringr::str_pad(x, width = width, side = 'right', pad = pad)
}

nonzero <- function(x) sum(x != 0,na.rm=T)
zero <- function(x) sum(x == 0,na.rm=T)
is_na <- function(x) sum(is.na(x) == T)

na.zero <- function (x) {  #replace na with zero
  x[is.na(x)] <- 0
  return(x)
}
one.zero <- function (x) {   
  x[(abs(x)) <= 0.00000000001] <- 0.00000000001
  return(x)
}
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}


normalize_na <- function(data_df) {
  #' Convert all known NA strings to NA in a data frame
  convert_column <- function(col){
    na_strings <- c("NA", "", "{ND}")
    if ( suppressWarnings(all(is.na(as.numeric(col)))) ) {
      col[col %in% na_strings] <- NA
      col 
    }
    else { 
      as.numeric(col) 
    }
  }
  
  lapply(data_df, convert_column) %>% as.data.frame  
}


force_return_NA <- function(f, ...) {
  #' @param  f A function returning a single summary value of its input
  #' @return if evaluated result is +/-Inf, return NA, otherwise return the evaluated result   
  res <- suppressWarnings(f(...)) 
  if (is.infinite(res)) NA 
  else res
}


column_summary <- function(col) {
  #' Generate statistical summary for a single column
  # Numeric columns
  if (class(col) %in% c("integer", "numeric", "double")) {
    tolerance  <- 1e-8 
    
    c(
      type        = class(col),
      count         = length(col),
      min         = round(force_return_NA(min, col, na.rm = TRUE), 2),
      a_median      = round(median(col, na.rm = TRUE), 4),
      b_mean        = round(mean(col, na.rm = TRUE), 6),
      sd          = round(sd(col, na.rm = TRUE), 2),
      max         = round(force_return_NA(max, col, na.rm = TRUE), 2),
      #count_missing = sum(is.na(col), na.rm = TRUE),
      perc_missing =  round(mean(is.na(col), na.rm = TRUE),4),
      #count_zero   = sum(abs(col) <= tolerance, na.rm=TRUE),
      perc_zero =  round(mean(abs(col) <= tolerance, na.rm = TRUE),4),
      #count_neg_spl   =sum(col<0 & col>-10 , na.rm=TRUE),
      perc_neg_spl = round(mean(col<0, na.rm=TRUE),4),
      #count_999_spl   =sum(col==999 , na.rm=TRUE),
      perc_999_spl = round(mean(col==999, na.rm=TRUE),4),
      num_unique  = length(unique(col[!is.na(col)], na.rm = TRUE)),
      quantiles   = round(quantile(col, probs = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99), na.rm = TRUE), 2)
    )
  }
  # Character columns
  else {
    nchars <- nchar(as.character(col), allowNA = TRUE)
    is_na  <- is.na(col)
    c(
      type        = class(col),
      count         = length(col),
      min         = force_return_NA(min, nchars, na.rm = TRUE),
      a_median      = as.integer(median(nchars, na.rm = TRUE)),
      b_mean        = as.integer(mean(nchars, na.rm = TRUE)),
      sd          = as.integer(sd(nchars, na.rm = TRUE)),
      max         = force_return_NA(max, nchars, na.rm = TRUE),
      #count_missing = sum(is_na),
      perc_missing = round(mean(is_na),4),
      #count_zero   = NA,
      perc_zero = NA,
      perc_neg_spl   =NA,
      perc_999_spl   =NA,
      num_unique  = length(unique(col[!is_na], na.rm = TRUE)),
      quantiles   = round(quantile(nchars, probs = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99), na.rm = TRUE), 2)
    )
  }
}




column_summary_simple <- function(col) {
  #' Generate statistical summary for a single column
  # Numeric columns
  if (class(col) %in% c("integer", "numeric", "double")) {
    tolerance  <- 1e-8 
    c(
      mean        = round(mean(col, na.rm = TRUE), 2),
      num         = length(col)
    )
  }
  # Character columns
  else {
    nchars <- nchar(as.character(col), allowNA = TRUE)
    is_na  <- is.na(col)
    c(
      num_missing = sum(is_na, na.rm = TRUE),
      num_unique  = length(unique(col[!is_na], na.rm = TRUE))
      )
  }
}


categorical_distribution <- function(col, col_name) {
  #' Generate frequency counts for each level in a categorical(character) column
  #' @param col A single column of data
  #' @param col_name A string, the name of the column
  distr     <- summary(as.factor(col))
  output_df <- data.frame(Variable = col_name, levels = names(distr), freq = distr)
}


data_summary <- function(data_df) {
  #' Generate variable summary (columnwise) for a dataset formatted as data frame
  summary_df <- lapply(data_df, column_summary) %>% 
    as.data.frame %>% 
    t %>% 
    as.data.frame
  summary_df %<>% data.frame(Variable=row.names(.), .) %>% dplyr::arrange(type) 
}

data_groupBy_summary <- function(df){
  
  stats_mean <- df %>%
    group_by(groupBy) %>%
    summarise_all(funs(mean(., na.rm = TRUE)))
  
  stats_mean <- as.data.frame(stats_mean)
  stats_mean$groupBy = paste0(stats_mean$groupBy,"_mean")
  #tmp<- as.data.frame(lapply(stats_mean, function(x) (mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)))
  tmp<- as.data.frame(lapply(stats_mean, function(x) (sd(x, na.rm = TRUE)/mean(x,na.rm = TRUE))))
  
  tmp$groupBy<-"CoeffOfVariance"
  stats_mean<- rbind(stats_mean,tmp)
  remove(tmp)
  stats_mean
  
  stats_mean <- t(stats_mean)
  colnames(stats_mean) = stats_mean[1, ] # the first row will be the header
  stats_mean = stats_mean[-1, ] 
  stats_mean <- as.data.frame(stats_mean)
  stats_mean$Variable <- row.names(stats_mean)
  stats_mean <- stats_mean %>% dplyr::mutate_all(funs(type.convert(as.character(.))))
  return(stats_mean)
  
}

categorical_summary <- function(char_vars, data_df){
  #' Generate categorical variable summary(columnwise) for a dataset formatted as data frame
  #' @param char_vars A vector of strings
  #' @param data_df A data frame with observations as row entries
  char_distr_df    <- lapply(char_vars, function(var_name) {categorical_distribution(data_df[[var_name]], var_name)}) 
  char_distr_df    <- Reduce(rbind, char_distr_df)
}


summary_to_xlsx <- function(summary_list, summary_names, file_name, dir="~") {
  #' Create xlsx file with differnt sheets given a list of summaries formated as data frames.
  #'
  #' @param summary_list    A list of data frames containing different summaries
  #' @param summary_names   A vector of strings corresponding to the different summaries, used to name the different sheets
  #' @param file_name       A string for the name of the xlsx file
  #' @param dir             A string for where the xlsx file is going to be stored, default is $HOME directory

  summary_book   <- xlsx::createWorkbook(type="xlsx")
  summary_sheets <- lapply(summary_names, function(sname) xlsx::createSheet(wb=summary_book, sheetName=sname) )
  
  mapply(function (s, ss) {xlsx::addDataFrame(x=s, sheet=ss); xlsx::autoSizeColumn(ss,1:NCOL(s))}, summary_list, summary_sheets)
  
  file_path <- normalizePath(file.path(dir, paste0(file_name, ".xlsx", sep="")))
  xlsx::saveWorkbook(summary_book, file_path)
}



substrLeft <- function(x, n){
  substr(x, 1, nchar(x)-n)
}
rename_cols <- function(df, old_names, new_names) {
  stopifnot(identical(length(old_names), length(new_names)))
  names_idx <- match(old_names, names(df))
  missing_lv  <- is.na(names_idx)
  names_idx <- names_idx[!missing_lv]
  new_names <- new_names[!missing_lv]
  names(df)[names_idx] <- new_names
  df
}
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      mean= mean(is.na(x)),
      median=median(is.na(x)),
      min=min(is.na(x)),
      max=max(is.na(x)),
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d)
}

baseUniStats <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss1=sum(is.na(x)), 
      n1=length(x), 
      propmiss1=sum(is.na(x))/length(x),
      min1=min(x,na.rm=TRUE),
      mean1= mean(x,na.rm=TRUE),
      median1=median(x,na.rm=TRUE),
      max1=max(x,na.rm=TRUE),
      numUnique1 = length(unique(x))
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d)
}


rename_cols <- function(df, old_names, new_names) {
stopifnot(identical(length(old_names), length(new_names)))
names_idx <- match(old_names, names(df))
missing_lv  <- is.na(names_idx)
names_idx <- names_idx[!missing_lv]
new_names <- new_names[!missing_lv]
names(df)[names_idx] <- new_names
df
}



summary2 <- function(df) {
  df <- df[, vapply(df, is.numeric, logical(1))]
  # TODO: DRY / Functional
  summary_funs <- function(x) {
    c(
      min         = min(x, na.rm = TRUE),
      mean        = mean(x, na.rm = TRUE),
      median      = median(x, na.rm = TRUE),
      sd          = sd(x, na.rm = TRUE),
      max         = max(x, na.rm = TRUE),
      per_missing = mean(is.na(x), na.rm = T),
      num_missing = mean(is.na(x), na.rm = T))
  }   
  
  lapply(df, summary_funs) %>% 
    as.data.frame %>%
    t %>%
    as.data.frame
}

firstLookup <- function(data, lookup, by, select = setdiff(colnames(lookup), by)) {
  # Merges data to lookup using first row per unique combination in by.
  unique.lookup <- lookup[!duplicated(lookup[, by]), ]
  res <- merge(data, unique.lookup[, c(by, select)], by = by, all.x = T)
  return (res)
}
movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}
moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}




get_rubympi_path <- function() { "s3://PATH/" }
rubyread <- function(name = NULL, .path = get_rubympi_path()) {
  if (is.null(name)) name <- grab_latest_file_in_s3_dir(path)
  x.serialized <- tempfile()
  s3.cmd <- paste("s3cmd get", s3_path(.path, name), x.serialized)
  res <- system(s3.cmd, intern = TRUE)
  ans <- rjson::fromJSON(paste(readLines(x.serialized), collapse = "\n"))
  unlink(x.serialized)
  ans
}

s3path <- function() {
  ## The default S3 prefix, for example, `s3://yourbucket/yourprefix/`.
  ## You should set this in everyone's `~/.Rprofile` if
  ## you are using s3mpi to collaborate in a data science team.
  path <- getOption("s3mpi.path")
  
  if (is.null(path)) {
    stop("s3mpi package: Please set your s3 path using ",
         "options(s3mpi.path = 's3://your_bucket/your/path/'). ",
         "This is where all of your uploaded R objects will be stored.")
  }
  
  path
}
s3_path <- function(path, name) {
  paste('"', path, name, '"', sep = '')
}
#df <- rubyread_avantminer('deena/scrubbed_raw/batch/401')


rubyread <- function(name = NULL, .path = get_rubympi_path()) {
  if (is.null(name)) name <- grab_latest_file_in_s3_dir(path)
  x.serialized <- tempfile()
  s3.cmd <- paste("s3cmd get", s3_path(.path, name), x.serialized)
  res <- system(s3.cmd, intern = TRUE)
  ans <- rjson::fromJSON(paste(readLines(x.serialized), collapse = "\n"))
  unlink(x.serialized)
  ans
}

tree.bin <- function(var){ 
  mdl <- rpart( 
    paste('dpd60_12 ~',var) 
    , data=df 
    , control = rpart.control(cp = 0.001, maxdepth=4) 
  )  
  
  if(nrow(mdl$frame) > 1){ 
    tbl <- mdl$frame %>%  
      mutate(label = labels(mdl)) %>%  
      select(var,label) %>% 
      filter(label != 'root') 
    
    
    vals <- tbl$label %>%  
      gsub('.+[<>=]+','',.) %>%  
      as.numeric %>% unique %>% sort %>% 
      c(min(df[[var]],na.rm=T),.,max(df[[var]],na.rm=T)) 
    
    return(df[[var]] %>% cut(vals, include.lowest=T)) 
  }else{return(NA)} 
}


tree.bin <- function(var,target){ 
  mdl <- rpart( 
    paste('dpd60_12 ~',var) 
    , data=df 
    , control = rpart.control(cp = 0.001, maxdepth=4) 
  )  
  
  if(nrow(mdl$frame) > 1){ 
    tbl <- mdl$frame %>%  
      mutate(label = labels(mdl)) %>%  
      select(var,label) %>% 
      filter(label != 'root') 
    
    
    vals <- tbl$label %>%  
      gsub('.+[<>=]+','',.) %>%  
      as.numeric %>% unique %>% sort %>% 
      c(min(df$agg204,na.rm=T),.,max(df$agg204,na.rm=T)) 
    
    return(df[[var]] %>% cut(vals, include.lowest=T)) 
  }else{return(NA)} 
}



tblFun <- function(x){
  tbl1 <- as.data.frame(prop.table(table(x)))
  tbl2 <- as.data.frame((table(x)))
  tbl2 <- rename(tbl2,Count=Freq)
  tbl <- inner_join(tbl1,tbl2)
  remove(tbl1)
  remove(tbl2)
  tbl <- rename(tbl,Category=x)
  #  tbl <- cbind(varName = deparse(substitute(x)),tbl)
  #tbl <- cbind(varName2 = (substitute(x)),tbl)
  tbl
}

subset_colclasses <- function(DF, colclasses="numeric") {
  DF[,sapply(DF, function(vec, test) class(vec) %in% test, test=colclasses)]
}
#str(subset_colclasses(df, c("factor", "integer")))

getDataSetFromSQL <- function(ds,sql){
dsLoanIds <- dplyr::select(ds,loan_id)
strLoanIds <- paste(as.character(dsLoanIds$loan_id),"",collapse=",",sep="")
remove(dsLoanIds)
}

calc_newRating_2D <- function(score,FICO) {
  #prime as of 2017-07-14 policy
  if (score <= 0.05  & FICO>=700)  "A6"
  else if (score <= 0.055 & FICO>=690)  "B1"
  else if (score <= 0.06  & FICO>=690)  "B2"
  else if (score <= 0.065 & FICO>=680)  "B3"
  else if (score <= 0.07  & FICO>=680)  "B4"
  else if (score <= 0.08  & FICO>=680)  "B5"
  else if (score <= 0.1   & FICO>=680)  "CA"
  #near prime as of 2017-07-14 policy
  else if (score <=0.08   & FICO >=660) "C1"
  else if(score <=0.09 & FICO >=650)  "C3"
  else if(score <=0.1 & FICO >=630)  "C5"
  else if(score <=0.12 & FICO >=600)  "D3"
  else if(score <=0.14 & FICO >=600)  "D4"
  else if(score <=0.12 & FICO >=580)  "D5"
  else "NEP"
}
calc_newRating_1D <- function(score,FICO) {
  #prime as of 2017-07-14 policy
  if (score <= 0.1   & FICO>=680)  "A/B/CA"
  #near prime as of 2017-07-14 policy
  else if (score <=0.08   & FICO >=660) "C"
  else if(score <=0.09 & FICO >=650)  "C"
  else if(score <=0.1 & FICO >=630)  "C"
  else if(score <=0.12 & FICO >=600)  "D"
  else if(score <=0.14 & FICO >=600)  "D"
  else if(score <=0.12 & FICO >=580)  "D"
  else "NEP"
}
calc_newRating_1D_v2 <- function(score,FICO) {
  if (score <= 0.05  & FICO>=700)  "A/B"
  else if (score <= 0.055 & FICO>=690)  "A/B"
  else if (score <= 0.06  & FICO>=690)  "A/B"
  else if (score <= 0.065 & FICO>=680)  "A/B"
  else if (score <= 0.07  & FICO>=680)  "A/B"
  else if (score <= 0.08  & FICO>=680)  "A/B"
  else if (score <= 0.1   & FICO>=680)  "CA"
  #near prime as of 2017-07-14 policy
  else if (score <=0.08   & FICO >=660) "C"
  else if(score <=0.09 & FICO >=650)  "C"
  else if(score <=0.1 & FICO >=630)  "C"
  else if(score <=0.12 & FICO >=600)  "D"
  else if(score <=0.14 & FICO >=600)  "D"
  else if(score <=0.12 & FICO >=580)  "D"
  else "NEP"
}
#ds$newRating_2D <- mapply(calc_newRating_2D,ds$final_s430_score,ds$BR_fico_score)

calc_newAPR <- function(rating){
  #prime as of 2017-07-14 policy
  if(rating == "A6") 0.0995
  else if(rating == "B1") 0.1195
  else if(rating == "B2") 0.1395
  else if(rating == "B3") 0.1595
  else if(rating == "B4") 0.1795
  else if(rating == "B5") 0.1995
  else if(rating == "CA") 0.2195
  #near prime as of 2017-07-14 policy
  else if(rating == "C1") 0.2195
  else if(rating == "C2") 0.2395
  else if(rating == "C3") 0.2595
  else if(rating == "C4") 0.2795
  else if(rating == "C5") 0.2995
  else if(rating == "D1") 0.3195
  else if(rating == "D2") 0.3395
  else if(rating == "D3") 0.3595
  else if(rating == "D4") 0.3595
  else if(rating == "D5") 0.3595
  else NA
}
#ds$calcAPR <- mapply(calc_newAPR,ds$newRating_2D)

#ds$newRating_2D <- mapply(calc_newRating_2D,ds$final_s430_score,ds$BR_fico_score)


ds$calcAPR <- mapply(calc_newAPR,ds$newRating_2D)
summary(ds$calcAPR)


simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}
#library(pROC)
#plot(roc(test_set$bad_widget, glm_response_scores, direction="<"),
#     col="yellow", lwd=3, main="The turtle finds its way")
#tmpROC <- simple_roc(ds_a$F10Def60Flag,ds_a$prime_v1_score)



monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); 
lt$year*12 + lt$mon } 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
mondf(as.Date("2008-01-01"), Sys.Date())

datenb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); 
lt$year*12 + lt$mon } 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
mondf(as.Date("2008-01-01"), Sys.Date())

