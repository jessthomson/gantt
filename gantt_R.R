


#if not installed
#install.packages("TimeProjection")
#install.packages("dplyr")
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("RColorBrewer")


#change your start date here (this is valid currently only for full time accelerated entry students)


phdcalendar <- function (cname="PhD project timeline" ,start.dt="2021-07-26")
{
  library(TimeProjection)
  library(dplyr)   
  library(plyr)
  library(ggplot2)
  library(RColorBrewer)
  
  start.date <- as.Date(start.dt,"%Y-%m-%d")
  
  end.date <-  seq(start.date, length=2, by="+3 year")[2] -1
  
  phd.length=  as.numeric(end.date-start.date) +1
  
  phd <- data.frame(dates=seq(start.date, end.date,"day"), item=" ", stringsAsFactors =FALSE)
  
  #calendarHeat(phd$dates, phd$item, varname="PhD Oliver Twist")
  
  phd$item[ which(phd$dates==start.date %m+% months(6))] <- "Introductory Seminar"
  phd$item[ which(phd$dates==start.date %m+% months(12))] <- "Confirmation of Candidature/Project Proposal"
  phd$item[ which(phd$dates==start.date %m+% months(4))] <- "Draft Project Proposal"
  phd$item[ which(phd$dates==start.date %m+% months(3))] <- "Animal Ethics application/submission"
  phd$item[ which(phd$dates==start.date %m+% months(11))] <- "Research Integrity Training"
  phd$item[ which(phd$dates==start.date %m+% months(32))] <- "Pre-submission Seminar"
  phd$item[ which(phd$dates==start.date %m+% months(24))] <- "Work in Progress Seminar"
  phd$item[ which(phd$dates==start.date %m+% months(30))] <- "Identify Reviewers"
  phd$item[ which(phd$dates==start.date %m+% months(33))] <- "Intention to Submit Form"
  
  #phd$item[ which(phd$dates==start.date %m+% months(12))] <- "Confirmation of Candidature"
  
  
  
  #phd$item[ which(phd$dates==(start.date+1) %m+% months(6))] <- "Research Plan Review"
  phd$item[ which( substr(phd$dates,6,15)=="03-31")] <- "Research Plan Review"
  
  phd$item[ which( substr(phd$dates,6,15)=="09-30")] <- "Annual Progress Report"
  
  phd$item[1] <- "Begin candidature"
  phd$item[(nrow(phd))] <- "Submit thesis"
  
  phd$item[c(18,19,39,40,67,68,88,89)] <- "Course Work"
  
  
  cols <- c("grey", "pink","green", "red", "lightblue","brown", "blue",  "orange", "yellow",
            "purple", "cyan", "forestgreen", "orchid3", "deeppink","coral" )# (in regards to items)
  
  
  
  
  tp = projectDate(phd$dates, drop = F)
  tp$values = phd$item
  tp$week = as.numeric(format(phd$dates, "%W"))
  tp$mweek = floor((day(phd$dates)- wday(phd$dates)) /7) 
  
  offset<- 0
  firstm <- month(phd$dates[1])
  firsty <- year(phd$dates[1])
  for (i in 1:nrow(tp))
  {
    
    
    if (tp$mday[i]==1 )  offset <- -tp$mweek[i]+1
    if (firstm==month(phd$dates[i]) & firsty == year(phd$dates[i]) ) offset <-    1
    tp$mweek[i]<-  tp$mweek[i] + offset
    
  }
  
  tp = ddply(tp, .(year, month), transform, monthweek = mweek)# 1 + 
  #week - min(week))
  
  tp$month <-as.factor(tp$month)
  levels(tp$month) <- month.abb[1:12]
  
  
  
  ggplot(tp, aes(monthweek, weekday, fill = values)) + geom_tile(colour = "white") +        
    facet_grid(year ~ month) + scale_fill_manual(name="",values=cols[1:(length(cols))]) + xlab("Week of Month") + ggtitle(paste("PhD candidature of",cname, "- from: ",start.date, " to: " ,end.date))
  
  
} 


phdcalendar("Jess Thomson", "2021-07-26
             ")



