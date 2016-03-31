rm(list=ls())
options("scipen"=999, "digits"=4)

# gsheets import fails
require(gsheet)
d.in2 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1pGAMfqfVx7jJIqc6MauZq1R05CgKNS83PDYLw3_DN4c/edit?ts=56e8881c#gid=1371587120")

# csv import
d.in <- read.csv("C:\\Users\\Josh Gaunt\\Documents\\R\\Germany Workflows SSI Survey-Cleaned\\Germany Workflows SSI Survey-Cleaned - 20160203142449-SurveyExport cleaned.csv")
lapply(d.in, 'class')
summary(d.in)

# computes proportions for each task, not that useful in hindsight
d.out <- data.frame()
for(i in 2:27) {
  total <- sum(d.in[,i], na.rm = TRUE)
  ff <- sum(d.in[d.in$Which.web.browser.do.you.use.for.most.of.your.Internet.activities.on.a.laptop.or.desktop.computer.=='Mozilla Firefox',i], na.rm = TRUE)
  gc <- sum(d.in[d.in$Which.web.browser.do.you.use.for.most.of.your.Internet.activities.on.a.laptop.or.desktop.computer.=='Google Chrome',i], na.rm = TRUE)
  ie <- sum(d.in[d.in$Which.web.browser.do.you.use.for.most.of.your.Internet.activities.on.a.laptop.or.desktop.computer.=='Internet Explorer',i], na.rm = TRUE)
  sa <- sum(d.in[d.in$Which.web.browser.do.you.use.for.most.of.your.Internet.activities.on.a.laptop.or.desktop.computer.=='Safari',i], na.rm = TRUE)
  ed <- sum(d.in[d.in$Which.web.browser.do.you.use.for.most.of.your.Internet.activities.on.a.laptop.or.desktop.computer.=='Edge',i], na.rm = TRUE)
  op <- sum(d.in[d.in$Which.web.browser.do.you.use.for.most.of.your.Internet.activities.on.a.laptop.or.desktop.computer.=='Opera',i], na.rm = TRUE)
  d.out <- rbind(d.out, cbind(i, total, ff, gc, ie, sa, ed, op))
}

# run ChiSq on table of 1/0 responses by task, expanded over browser
d.final <- data.frame()
for(i in 2:27) {
  result <- chisq.test(table(d.in[,33], d.in[,i]))
  d.final <- rbind(d.final, cbind(i, result$statistic, result$p.value))
}

### BROWSER VS. AGE (to clipboard) [did this one here to get it before collapsing other browsers in Basic.r script]
write.table(table(d.in[,42], d.in[,33]), 'clipboard', sep = "\t")