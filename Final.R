rm(list=ls())
options("scipen"=999, "digits"=4)

# import, check classes (already manually find/replaced -- for NA in excel prior to this import)
d.in <- read.csv("C:\\Users\\Josh Gaunt\\Documents\\R\\Germany Workflows SSI Survey-Cleaned\\Germany Workflows SSI Survey-Cleaned - 20160203142449-SurveyExport cleaned.csv", stringsAsFactors = FALSE)
lapply(d.in, 'class')
summary(d.in)

# drop fluff from task response names and shorten variable names
t.names <- names(d.in[,1:27])
t.names <- gsub(".Did.you.do.you.do.the.following.on.the.Internet.in.the.last.week.", "", t.names, perl = TRUE)
t.names <- append(t.names, c('GOOD_EVERYDAY', 'NO_WEB_TOMORR', 'PRIVACY', 'NO_DO_WEB', 'START_YEAR', 
                             'BRWSR_MAIN', 'BRWSR_MAINo', 'BRWSR_OTHER',  'BRWSR_OTHERo', 'BRWSR_MOBILE_MAIN', 'BRWSR_MOBILE_MAINo', 'BRWSR_MOBILE_OTHER', 'BRWSR_MOBILE_OTHERo',
                             'GENDER', 'AGE', 'COMM', 'INCOME', 'EDU'))
names(d.in) <- t.names

# rename nonFF browsers to Other to collapse data
d.in[d.in$BRWSR_MAIN!='Mozilla Firefox',33] <- 'Other'

# manually set datatypes (sigh)
for(i in  1:27) {d.in[,i] <- as.integer(d.in[,i])}
for(i in 28:45) {d.in[,i] <-     factor(d.in[,i])}

f.SurveyTaskByV <- function(V) {
  d.final <- data.frame()
  # run ChiSq on table of 1/0 responses by task, expanded over the column passed as arg
  for(i in 2:27) {
    result <- chisq.test(table(d.in[,V], d.in[,i]))
    d.final <- rbind(d.final, cbind(i, obs= result$observed, exp= result$expected, ChiSq= result$statistic, p= result$p.value, resid= result$stdres))
  }
  # copy to clipboard to paste into excel, quick/dirty
  write.table(d.final, 'clipboard', sep = "\t")
}

### DEMOS VS. TASK (AGE)
f.SurveyTaskByV(42)

### DEMOS VS. TASK (BROWSER)
f.SurveyTaskByV(33)

# iterate by question with this function to table by age x browser instead of either by 1yes/0no (counts 1s only)
f.ThisChiSq <- function(d.in) {
  result <- chisq.test(table(d.in[,42], d.in[,33]))
  return(cbind(i, obs= result$observed, exp= result$expected, ChiSq= result$statistic, p= result$p.value, resid= result$stdres))
}
d.out <- data.frame()
for(i in 2:27) {
  d.out <- rbind(d.out, f.ThisChiSq(d.in[d.in[,i]==1,]))
}
write.table(d.out, 'clipboard', sep = "\t")