rm(list=ls())
options("scipen"=999, "digits"=4)

# import, check classes (already manually find/replaced -- for NA in excel prior to this import)
d.in <- read.csv("C:\\Users\\Josh Gaunt\\Documents\\R\\Germany Workflows SSI Survey-Cleaned\\Germany Workflows SSI Survey-Cleaned - 20160203142449-SurveyExport cleaned.csv", stringsAsFactors = FALSE)
lapply(d.in, 'class')
summary(d.in)

# drop fluff from task response names
t.names <- names(d.in[,1:27])
t.names <- gsub(".Did.you.do.you.do.the.following.on.the.Internet.in.the.last.week.", "", t.names, perl = TRUE)
t.names <- append(t.names, c('GOOD_EVERYDAY', 'NO_WEB_TOMORR', 'PRIVACY', 'NO_DO_WEB', 'START_YEAR', 
                             'BRWSR_MAIN', 'BRWSR_MAINo', 'BRWSR_OTHER',  'BRWSR_OTHERo', 'BRWSR_MOBILE_MAIN', 'BRWSR_MOBILE_MAINo', 'BRWSR_MOBILE_OTHER', 'BRWSR_MOBILE_OTHERo',
                             'GENDER', 'AGE', 'COMM', 'INCOME', 'EDU'))
names(d.in) <- t.names

# rename nonFF browsers to Other to collapse data (sample drops from 1019 to 793)
d.in <- d.in[d.in$BRWSR_MAIN %in% c('Mozilla Firefox','Google Chrome'),]

# manually set datatypes (sigh)
for(i in 1:45) {d.in[,i] <- factor(d.in[,i])}

# drop those "other" cols; they're usually empty and unless they put FF/GC there by mistake I'm not interested here
# also, divide the data on desktop vs. mobile
d.deskmain   <- d.in[c(1:32,33,41:45)]
d.deskother  <- d.in[c(1:32,35,41:45)]
d.mobmain    <- d.in[c(1:32,37,41:45)]
d.mobother   <- d.in[c(1:32,39,41:45)]

#library(partykit) wow, this wouldn't even run
library(party)
# look at desktop first
m.deskmain  <- ctree(BRWSR_MAIN  ~ ., d.deskmain)
#m.deskother <- ctree(BRWSR_OTHER ~ ., d.deskother)
#m.mobmain   <- ctree(BRWSR_MOBILE_MAIN  ~ ., d.mobmain)
#m.mobother  <- ctree(BRWSR_MOBILE_OTHER ~ ., d.mobother)
# ^these couldn't run to completion, or I didn't give them the 100 yrs they needed

plot(m.deskmain)