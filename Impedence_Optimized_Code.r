library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(data.table)
cbg_zip_correspondence<-setDT(read_excel("ZIPCODE_BLOCK_GROUP_CORRESPONDENCE.xlsx"))
cbg_charging_station<-setDT(read_csv("CBG_ChargingStation.csv"))
zip_code_travel_time<-setDT(read_csv("NA_OD_0-3hours.csv"))


cbg_charging_station <- cbg_charging_station %>%
  mutate_at(c(2:6), as.numeric)
setDT(cbg_charging_station)

cols<-c('GEOID10','D1D','Ac_Unpr','NUMPOINTS')
cbg_charging_station=cbg_charging_station[,..cols]
cbg_zip=merge.data.table(cbg_charging_station,cbg_zip_correspondence[,c('GEOID10','ZCTA5CE20')],by ='GEOID10',all.x = T )
cbg_zip<-cbg_zip%>%drop_na(ZCTA5CE20)
cbg_zip<-cbg_zip[nchar(cbg_zip$ZCTA5CE20)%in%c(4,5),]
#rm(cbg_charging_station)
zip_code_travel_time$OZCTA=as.numeric(zip_code_travel_time$OZCTA)
zip_code_travel_time$DZCTA=as.numeric(zip_code_travel_time$DZCTA)
head(zip_code_travel_time)
head(cbg_zip)
rm(cbg_charging_station,cbg_zip_correspondence)

#how many unique zip codes we pass in each go is manually decided based on when we get a out of memory error
#we used nine partitions in our case and each of them had ~3k unique zip codes 

cbg_zip_travel_time=merge.data.table(cbg_zip[ZCTA5CE20%in%unique(cbg_zip$ZCTA5CE20)[23001:26068]],zip_code_travel_time[OZCTA%in%unique(cbg_zip$ZCTA5CE20)[23001:26068]],by.x="ZCTA5CE20",by.y = "OZCTA",all.x=T  ,allow.cartesian = T)
cbg_zip_travel_time=merge.data.table(cbg_zip_travel_time,cbg_zip[,c('GEOID10','ZCTA5CE20','NUMPOINTS')],by.x='DZCTA',by.y ='ZCTA5CE20',all.x = T,allow.cartesian = T)
cbg_zip_travel_time<-cbg_zip_travel_time%>%drop_na(GEOID10.y)
cbg_zip_travel_time[GEOID10.x==GEOID10.y & D1D<0.5,time:=0.5*sqrt(0.0015625*Ac_Unpr)*60/35]
cbg_zip_travel_time[GEOID10.x==GEOID10.y & D1D>6,time:=0.5*sqrt(0.0015625*Ac_Unpr)*60/15]
cbg_zip_travel_time[GEOID10.x==GEOID10.y & D1D>=0.5 & D1D<=6,time:=0.5*sqrt(0.0015625*Ac_Unpr)*60/25]

cbg_zip_travel_time[,time:=ifelse(is.na(time),EstTime ,time)]
cbg_zip_travel_time[,impedence.o.d :=ifelse(is.na(time),0 ,1*((time)^(-0.300))*(exp(-0.070*time)))]
final_impedence<-cbg_zip_travel_time%>%group_by(GEOID10.x)%>%summarise(alt.fuel.accessibility=sum(NUMPOINTS.y*impedence.o.d))
#write.csv(cbg_zip_travel_time,"cbg_zip_travel_time_test1.csv")
write.csv(final_impedence,"final_impedence_test9.csv")
