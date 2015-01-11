
library(RCurl)

#read in data
polls<-read.csv("https://raw.github.com/fghjorth/danish-polls/master/all-polls.csv")

#clean up
polls <- polls[,1:11]
names(polls) <- c("house", "date","Venstre","Socialdemokraterne","DF","Radikale","SF","Enhedslisten","LA","Konservative","Kristendemokraterne")
polls$house<-gsub("RambÃ¸ll","Ramboll",polls$house)

#add date
require(lubridate)
polls$fulldate<-mdy(polls$date)

#calculate blocs
names(polls)
polls$redbloc<-as.numeric(as.character((polls[,4])))+as.numeric(as.character((polls[,6])))+as.numeric(as.character((polls[,7])))+as.numeric(as.character((polls[,8])))
polls$bluebloc<-as.numeric(as.character((polls[,3])))+as.numeric(as.character((polls[,5])))+as.numeric(as.character((polls[,9])))+as.numeric(as.character((polls[,10])))
polls$blocdiff<-polls$bluebloc-polls$redbloc

#house effects?
anova(lm(blocdiff~factor(house),data=polls))
houseeffects<-as.data.frame(summary(lm(blocdiff~factor(house),data=polls))$coefficients[2:9,1:2])
names(houseeffects)<-c("est","se")
houseeffects$house<-as.character(levels(as.factor(polls$house))[2:9])
bdmean<-mean(polls$blocdiff)

t95<-1.96
ggplot(houseeffects,aes(x=est-bdmean,y=reorder(house,est))) +
  geom_point() +
  geom_errorbarh(aes(xmin=est-t95*se-bdmean,xmax=est+t95*se-bdmean,height=0)) +
  geom_vline(xintercept=0,linetype=2) +
  theme_bw() +
  xlab("") +
  ylab("")

#create house effect-corrected estimate of bloc difference
polls$housecorrblocdiff<-NA
for (i in 1:nrow(polls)){
  if(polls$house[i] %in% houseeffects$house){
    correction<-houseeffects$est[houseeffects$house==polls$house[i]]-bdmean
    polls$housecorrblocdiff[i]<-(polls$blocdiff[i]-correction)
  }
}


#plot
require(ggplot2)
require(scales)

ggplot(polls,aes(x=fulldate,y=housecorrblocdiff,colour=housecorrblocdiff)) +
  geom_point(alpha=1) +
  geom_smooth(method="loess",span=.2,level=.90) +
  geom_smooth(method="loess",span=.2,level=.95) +
  geom_smooth(method="loess",span=.2,level=.99,color="black") +
  xlab("") +
  ylab("Fordel til blå blok, pct.-point") +
  geom_hline(yintercept=0,linetype=2) +
  scale_colour_gradient2(low="red",high="blue",mid="dark gray",guide=F) +
  theme(legend.position="none") +
  theme_bw() 



#########################
# obsolete stuff
#########################

#linear trend past year?
polls$dayssinceelec<-as.numeric(difftime(as.Date(polls$fulldate),as.Date("2011-09-15"),unit="days"))
linfcast<-coef(lm(blocdiff~dayssinceelec,data=subset(polls,as.Date(fulldate)>"2013-10-07")))
fcast<-data.frame(date=ymd(c("2013-10-07","2014-10-07","2027-06-01")))
fcast$daysin<-as.numeric(difftime(as.Date(fcast$date),as.Date(fcast$date[1]),unit="days"))
fcast$predblocdiff<-linfcast[1]+fcast$daysin*linfcast[2]
fcast
#  geom_line(data=fcast,aes(x=date,y=predblocdiff),color="red",linetype="dotted") +
#  geom_line(data=subset(fcast,daysin<366),aes(x=date,y=predblocdiff),color="red",linetype="solid") +
#  xlim(ymd("2011-09-15"),ymd("2021-11-01")) +

#########################
# code used for scraping from the wiki
#########################

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
