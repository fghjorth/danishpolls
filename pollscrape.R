### script based on code by Erik Gahner Larsen, @erikgahner

library(XML)

#pull in data
url <- "http://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Danish_general_election"
polls <- readHTMLTable(url)

#clean up
polls <- polls[[1]]
polls <- polls[-1,1:10]
names(polls) <- c("Institut", "date","Venstre","Socialdemokraterne","DF","Radikale","SF","Enhedslisten","LA","Konservative")

#get full date
breakpoints<-c(which(is.na(polls$date)),nrow(polls)-1)
yearsindata<-c(2014,2013,2012,2011)
polls$fulldate<-NA
for (i in 4:1){
  polls$fulldate[0:breakpoints[i]]<-paste(polls$date[0:breakpoints[i]],", ",yearsindata[i],sep="")
}
polls$fulldate[nrow(polls)]<-polls$date[nrow(polls)]
polls<-polls[!is.na(polls$date),]
polls$fulldate[nrow(polls)]<-as.character(polls$date[nrow(polls)])
require(lubridate)
polls$fulldate<-mdy(tolower(polls$fulldate),locale="English")

#clean up house names
polls$Institut <- gsub("\\[|1|2|3|4|5|6|\\]", "", polls$Institut)
polls$Institut <- gsub("DR", "Epinion", polls$Institut)

#calculate blocs
names(polls)

polls$redbloc<-as.numeric(as.character((polls[,4])))+as.numeric(as.character((polls[,6])))+as.numeric(as.character((polls[,7])))+as.numeric(as.character((polls[,8])))

polls$bluebloc<-as.numeric(as.character((polls[,3])))+as.numeric(as.character((polls[,5])))+as.numeric(as.character((polls[,9])))+as.numeric(as.character((polls[,10])))

polls$blocdiff<-polls$bluebloc-polls$redbloc

#plot
require(ggplot2)
ggplot(polls,aes(x=fulldate,y=blocdiff)) +
  geom_point(alpha=.8) +
  geom_smooth(method="loess",span=.2,level=.90) +
  geom_smooth(method="loess",span=.2,level=.95) +
  geom_smooth(method="loess",span=.2,level=.99,color="black") +
  xlab("") +
  ylab("Blue bloc advantage, percentage points") +
  geom_hline(yintercept=0,linetype=2) +
  theme_bw()

#house effects?
anova(lm(blocdiff~factor(Institut),data=polls))
houseeffects<-as.data.frame(summary(lm(blocdiff~factor(Institut),data=polls))$coefficients[2:8,1:2])
names(houseeffects)<-c("est","se")
houseeffects$house<-as.character(levels(as.factor(polls$Institut))[2:8])
bdmean<-mean(polls$blocdiff,na.rm=T)

t95<-1.96
ggplot(houseeffects,aes(x=est-bdmean,y=reorder(house,est))) +
  geom_point() +
  geom_errorbarh(aes(xmin=est-t95*se-bdmean,xmax=est+t95*se-bdmean,height=0)) +
  geom_vline(xintercept=0,linetype=2) +
  theme_bw() +
  xlab("") +
  ylab("")

