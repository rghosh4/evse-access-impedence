library(readxl)
library(dplyr)
library(readr)
cbg_zip_correspondence<-read_excel("ZIPCODE_BLOCK_GROUP_CORRESPONDENCE.xlsx")
cbg_charging_station<-read_csv("CBG_ChargingStation.csv")
zip_code_travel_time<-read_csv("NA_OD_0-3hours.csv")

library(dplyr)
cbg_charging_station <- cbg_charging_station %>% 
  mutate_at(c(2:6), as.numeric)

cbg_charging_station<-cbg_charging_station%>%left_join(cbg_zip_correspondence)
rm(cbg_zip_correspondence)
cbg_i<-NULL
cbg_i_zip<-NULL
cbj_j<-NULL
cbg_j_zip<-NULL
time_i_j<-NULL
i_replications<-NULL
for (i in cbg_charging_station$GEOID10[2]){
          #print(i)
          zip<-NULL
          cbg1<-NULL
          z<-0
          zip<-cbg_charging_station$ZCTA5CE20[cbg_charging_station$GEOID10==i]
          l<-zip_code_travel_time%>%filter(OZCTA==zip)%>%distinct(DZCTA)
          cbg1<-cbg_charging_station%>%select(GEOID10,ZCTA5CE20,D1D,Ac_Unpr)%>%mutate(Ac_Unpr=0.0015625*Ac_Unpr)%>%filter(ZCTA5CE20%in%as.numeric(l$DZCTA))
          i_replications<-c(i_replications,nrow(cbg1))
          cbg_i<-c(cbg_i,i)
          cbg_i_zip<-c(cbg_i_zip,zip)
          #print(zip)
          for (j in cbg1$GEOID10){
                  #print(j)
                  cbj_j<-c(cbj_j,j)
                  zip_j<-cbg1$ZCTA5CE20[cbg1$GEOID10==j]
                  cbg_j_zip<-c(cbg_j_zip,zip_j)
                  time<-NULL
                  if (i==j){
                            if (cbg1$D1D[cbg1$GEOID10==j]<0.5){
                                   time=0.5*sqrt(cbg1$Ac_Unpr[cbg1$GEOID10==j])*60/35
                            }
                           else if (cbg1$D1D[cbg1$GEOID10==j]>6){
                                   time=0.5*sqrt(cbg1$Ac_Unpr[cbg1$GEOID10==j])*60/15}
                    
                            else {
                                   time =0.5*sqrt(cbg1$Ac_Unpr[cbg1$GEOID10==j])*60/25
                           }
                         #time_i_j<-c(time_i_j,time)
                    }
                    
                           
             else { 
                                
                                  time=zip_code_travel_time%>%filter(OZCTA==zip,DZCTA==zip_j)%>%pull(EstTime)
                                
                           
             }
                 
                  #print(time)
                  time_i_j<-c(time_i_j,time)
                  
                  }
          
}


cbg_i_all<-rep(cbg_i,i_replications)
cbg_i_zip_all<-rep(cbg_i_zip,i_replications)
final_df<-bind_cols(cbg_i_all,cbg_i_zip_all,cbj_j,cbg_j_zip,time_i_j,.name_repair = "unique")%>%
  rename(cbg.origin = ...1,zip.origin=...2,cbg.destination=...3,zip.destination=...4,travel.time=...5)
final_df<-final_df%>%left_join(select(cbg_charging_station,GEOID10,NUMPOINTS),by = c("cbg.destination" = "GEOID10"))%>%rename(alt.fuel.stations = NUMPOINTS) 
final_df<-final_df%>%mutate(impedence.o.d = ifelse(is.na(travel.time),0,1*((travel.time)^(-0.300))*(exp(-0.070*travel.time))))
final_impedence<-final_df%>%group_by(cbg.origin)%>%summarise(alt.fuel.accessibility=sum(alt.fuel.stations*impedence.o.d))
               






