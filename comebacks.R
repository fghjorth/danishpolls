#get data on comebacks
require(dplyr)
require(googlesheets)
sheetkey<-"1jHCvQWZ_KOAZEOg1nzS72ZePgL_OYYdp5qpWNXJqGm4"
ck <- register_ss(sheetkey) %>%
  get_via_csv()

#create date vars
require(lubridate)
ck$date<-dmy(paste(ck$dd,ck$mm,ck$yy))
ck$days2elec<-NA

ck$days2elec<-difftime(ck$date,ymd("1998-03-11"),units="days")
ck$days2elec[ck$valg=="fv11"]<-difftime(ck$date[ck$valg=="fv11"],ymd("2011-09-15"),units="days")
ck$days2elec[ck$valg=="fv15"]<-difftime(ck$date[ck$valg=="fv15"],ymd("2015-06-18"),units="days")

ck$valg<-factor(ck$valg,labels=c("Løkke i '11","Helle i '15","Nyrup i '98"))

ggplot(ck,aes(x=as.numeric(days2elec),y=blocshare,group=valg,fill=valg,color=valg)) +
  geom_point(size=3) +
  geom_smooth() +
  geom_hline(yintercept=50,linetype="dashed") +
  theme_minimal() +
  xlab("Dage til valget") +
  ylab("Blok-opbakning (pct.)") +
  scale_fill_manual(values=c("blue", "dark gray", "red")) +
  scale_color_manual(values=c("blue", "black", "red"))

setwd("~/GitHub/danishpolls")
ggsave("comebackkidplot.pdf",width=11,height=6)
ggsave("comebackkidplot.png",width=11,height=6)
