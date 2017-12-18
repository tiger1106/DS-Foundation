library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)


Forestry_Service_Requests <- read_csv("C:/Users/Naksh/Desktop/R Project/Forestry_Service_Requests.csv")
Forestry_Tree_Points <- read_csv("C:/Users/Naksh/Desktop/R Project/Forestry_Tree_Points.csv")
Forestry_Work_Orders <- read_csv("C:/Users/Naksh/Desktop/R Project/Forestry_Work_Orders.csv")
Forestry_Inspections <- read_csv("C:/Users/Naksh/Desktop/R Project/Forestry_Inspections.csv")
fsr_df<-Forestry_Service_Requests
fis_df<-Forestry_Inspections
ftp_df<-Forestry_Tree_Points
fwo_df<-Forestry_Work_Orders


# Community Dataset Wrangling

Community_District_household <- read_csv("C:/Users/Naksh/Downloads/NYC Demographic data/Community District household.csv")
Community_District_Area <- read_csv("C:/Users/Naksh/Downloads/NYC Demographic data/Community District Area.csv")

areaCD_df<- Community_District_Area

areaCD_df 

community_demo_df<-Community_District_household

community_demo_df<-community_demo_df %>% rename(CommunityBoard=`COMMUNITY DISTRICTS`)

areaCD_df<-areaCD_df %>% rename(CommunityBoard=BoroCD)

community_demo_df<-inner_join(community_demo_df,areaCD_df)

community_demo_df<-community_demo_df %>% select(`Borough cd`,CommunityBoard,Growth,Household,`Population 18 Years and over`,White,`Black/ African American`,Asian,`Native Hawaiian and Other Pacific Islander`,`Hispanic Origin (or other race)`,`Female 18 Over`,`Male 18 over`,Shape_Area,Shape_Leng)

community_demo_df<-community_demo_df %>% rename(BoroughCode=`Borough cd`)

ComplaintbyCB_df<-data.frame(fsr_df$CommunityBoard,fsr_df$ComplaintType)

ComplaintbyCB_df<-ComplaintbyCB_df %>% group_by(fsr_df.CommunityBoard,fsr_df.ComplaintType) %>% summarise(cnt=n())

ComplaintbyCB_df<-spread(ComplaintbyCB_df,fsr_df.ComplaintType,cnt)

ComplaintbyCB_df<-ComplaintbyCB_df%>% rename(CommunityBoard=fsr_df.CommunityBoard)

community_demo_Complaintytype_df<-inner_join(community_demo_df,ComplaintbyCB_df,by=c("CommunityBoard","CommunityBoard"))

community_demo_Complaintytype_df<-community_demo_Complaintytype_df %>% mutate(totalRequest=`Damaged Tree`+`Dead Tree`+`Dead/Dying Tree`+`Illegal Tree Damage`+`Overgrown Tree/Branches`+`Root/Sewer/Sidewalk Condition`+ `New Tree Request`)

Comm_ResponseTime_df<-fsr_df%>% group_by(CommunityBoard) %>% summarize(mean(ResponseTime,na.rm=TRUE),median(ResponseTime,na.rm=TRUE))

community_demo_Complaintytype_df<-inner_join(community_demo_Complaintytype_df,Comm_ResponseTime_df)







#Convert Date from Char to Date format

fsr_df<-fsr_df %>% mutate(myCreatedDate=as.Date(CreatedDate,"%m/%d/%Y"))
fsr_df<-fsr_df %>% mutate(myUpdatedDate=as.Date(UpdatedDate,"%m/%d/%Y"))
fsr_df<-fsr_df %>% mutate(tempcreatedate=myCreatedDate)%>% separate(tempcreatedate,c("Cyear","CMonth","CDate"),sep="-")
fsr_df<-fsr_df %>% mutate(tempupdatedate=myUpdatedDate)%>% separate(tempupdatedate,c("Uyear","UMonth","UDate"),sep="-")
fsr_df$CreatedMonth<-factor(fsr_df$CMonth)
levels(fsr_df$CreatedMonth) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
fsr_df$UpdatedMonth<-factor(fsr_df$UMonth)
levels(fsr_df$UpdatedMonth) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#Add TimeDiff

fsr_df<-fsr_df %>% mutate(ResponseTime=myUpdatedDate-myCreatedDate) 
fsr_df$ResponseTime<- as.numeric(fsr_df$ResponseTime)

fsr_df<-fsr_df %>% select(ComplaintNumber,SRCategory,SRType,SRPriority,SRSource,SRResolution,BoroughCode,CommunityBoard,TaxClass,GlobalID,ClosedDate,UpdatedDate,ComplaintType,Latitude,Longitude,myCreatedDate,myUpdatedDate,Cyear,CMonth,Uyear,UMonth,UDate,CreatedMonth,UpdatedDate,ResponseTime)

# full Join with inspection and Tree point

fis_df<-Forestry_Inspections
ftp_df<-Forestry_Tree_Points


fis_df<-fis_df %>% filter(!is.na(GlobalID)) # 239305

fis_df<-fis_df %>% rename(InClosedDate=ClosedDate)
fis_df<-fis_df%>% rename(InCreatedDate=CreatedDate)
fis_df<-fis_df %>% rename(InUpdatedDate=UpdatedDate)

fis_df<-fis_df %>% select(InspectionType,InspectionDate,InspectionStatus,InspectionTPCondition,InspectionTPStructure,TreePointDBH,TreePointGlobalID,PlantingSpaceGlobalID,ServiceRequestGlobalID,GlobalID,InClosedDate,InCreatedDate,InUpdatedDate,SWTotalTrees,SWTreeConditionRating,SWRatingTotal,SWDamageRating)

fis_df<-fis_df %>% rename(InGlobalId=GlobalID)

ftp_df<- ftp_df %>% select(DBH,GlobalID,GenusSpecies,TPStructure,TPCondition,StumpDiameter)

# join inspection and treepoint using Global ID in tree point with Treepoint global Id in inspection dataset

ftp_df<- ftp_df %>% rename(TreePointGlobalID=GlobalID)
t_df<-full_join(fis_df,ftp_df,by=c("TreePointGlobalID","TreePointGlobalID")) 

fistp_df<-t_df%>% filter(!is.na(InspectionDate))


# join inspection and Wo

fwo_df<-Forestry_Work_Orders

# filter  relevant attributes from Forestry Work order

fwo_df<-fwo_df %>% select(OBJECTID,WOType,WOStatus,Borough,CommunityBoard,WOCategory,WOEquipment,WOWoodRemains,WOContract,WOWireConflict,TreePointGlobalID,InspectionGlobalID,ClosedDate,CreatedDate,WOEntity,UpdatedDate)

# Rename attibutes for join
fwo_df<-fwo_df %>% rename(WOCreatedDate=CreatedDate)
fwo_df<-fwo_df %>% rename(WOClosedDate=ClosedDate)
fwo_df<-fwo_df %>% rename(WOUpdatedDate=UpdatedDate)
fwo_df<-fwo_df %>% rename(WOOBJECTID=OBJECTID)
fwo_df<-fwo_df %>% rename(InGlobalId=InspectionGlobalID)

# join on InspectionGlobal Id  in WO with Global ID of Inspection dataset

t_df<-full_join(fistp_df,fwo_df,by=c("InGlobalId","InGlobalId"))
fistpwo_df<-t_df %>% filter(!is.na(InspectionDate))


# join inspection, TP , WO with SR

# Rename  Global Id in Service dataset to Service Request GLobal Id 

fsr_df <- fsr_df %>% rename(ServiceRequestGlobalID=GlobalID)

# full join Service Request dataset with joint Inspection, Treepoint and WO data set

t_df<-full_join(fsr_df,fistpwo_df,by=c("ServiceRequestGlobalID","ServiceRequestGlobalID"))

fsrfistpwo_df<-t_df%>% filter(!is.na(ComplaintNumber))

fsrfistpwo_df %>% filter(!is.na(InspectionStatus))  # 63108

fsrfistpwo_df %>% filter(!is.na(WOType))  # 40300 


# Frequent Cases

# Frequent Tree points which have multiple inpection, service requests from Joint DataSet fsrfistpwo

FreqTpt_df<-fsrfistpwo_df %>% group_by(TreePointGlobalID.x) %>% summarise(cnt=n()) %>% arrange(desc(cnt)) %>% filter(!is.na(TreePointGlobalID.x)) 

# Rename Frequent Treepoint Count

FreqTpt_df <- FreqTpt_df%>%rename(CntTPT=cnt)



# Full join to add Frequent Treepoint  Cnt 

fsrfistpwo_df<-full_join(fsrfistpwo_df,FreqTpt_df,by=c("TreePointGlobalID.x","TreePointGlobalID.x"))

# frequent locations which have multiple service requests

# Merge longitude and latitude to get unique location key For FSR

t_df<-fsr_df %>% select(ServiceRequestGlobalID,ComplaintNumber,Longitude,Latitude)

t_df<-unite(t_df,"SRlocation",Longitude,Latitude,remove = FALSE)

FreqLoc_df<-t_df %>% group_by(SRlocation) %>% summarise(SRlocCnt=n()) %>% filter(SRlocation!="NA_NA")   # 61684 location points

t_df<-full_join(t_df,FreqLoc_df,by=c("SRlocation","SRlocation"))

fsrfistpwo_df<-full_join(fsrfistpwo_df,t_df,by=c("ServiceRequestGlobalID","ServiceRequestGlobalID"))

fsrfistpwo_df %>% group_by(SRlocation) %>% summarise(cnt=n()) %>% arrange(desc(cnt))   # 61685 Frequent location points
 
fsrfistpwo_df %>% group_by(CntTPT) %>% summarise(cnt=n()) %>% arrange(desc(cnt))


#Frequent Species

FreqSpecies_df<-fsrfistpwo_df %>% select(ServiceRequestGlobalID,TreePointGlobalID.x,GenusSpecies)
  
t_df<-FreqSpecies_df%>% group_by(GenusSpecies) %>% summarise(SpeciesCnt=n()) %>% arrange(desc(SpeciesCnt)) %>% filter(!is.na(GenusSpecies))   # 199 Frequent species

FreqSpecies_df<-full_join(FreqSpecies_df,t_df)

FreqSpecies_df %>% filter(!is.na(GenusSpecies) & SpeciesCnt>1000) %>% group_by(GenusSpecies) %>% summarise(cnt=n()) %>% arrange(desc(cnt))




#Merge with Joint datasets

fsrfistpwo_df<-full_join(fsrfistpwo_df,t_df)



# Explore relationship between TP cnt and location cnt

fsrfistpwo_df %>% ggplot(aes(x=SRlocCnt,y=CntTPT,col=BoroughCode))+geom_point()  # Broklyn has more presence


# identify location of Species 

fsrfistpwo_df %>% filter(SpeciesCnt>1500)%>%ggplot(aes(x=Longitude.x,y=Latitude.x,col=factor(SpeciesCnt),fill=GenusSpecies))+geom_point()

# London Planetree has more requests and Count is 10071 and Norway Maple has 7500

# Explore Request Month created  on these species

fsrfistpwo_df %>% filter(SpeciesCnt>7500) %>% ggplot(aes(x=CMonth,fill=GenusSpecies))+geom_bar()

# Explore Proportion of Species on time scale

fsrfistpwo_df %>% filter(SpeciesCnt>7500) %>% ggplot(aes(x=CMonth,fill=GenusSpecies))+geom_bar(position = "fill")

# Explore BoroughCode for these species
fsrfistpwo_df %>% filter(SpeciesCnt>1500) %>% ggplot(aes(x=GenusSpecies,fill=BoroughCode))+geom_bar(position = "fill") 




# Explore Complaint Type on these tress


fsrfistpwo_df %>% filter(ComplaintType!="New Tree Request" & SpeciesCnt>1500 & GenusSpecies!="Unknown - Unknown") %>% ggplot(aes(x=GenusSpecies,fill=ComplaintType))+geom_bar()

fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" ) %>% ggplot(aes(x=GenusSpecies,fill=ComplaintType))+geom_bar()  

fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" ) %>% ggplot(aes(x=GenusSpecies,fill=ComplaintType))+geom_bar(position = "fill") + ylab("Proportion") 

# NorWay Maple has most contribution in Damaged , Dead or Dying Tree and very less contribution in overgrown or Root or Sear Problem

# Thornless Honeylocust has more contribution in Illegal Tree Damage, Overgrown and SW complaints.

# London Plantree has almost Same contribution in all Complaints

# Pin Oak and Tilla Cordata has more contribution in overgrown and SW problems

# Damaged Tree has most cases more than 9000  

# Explore Work order category for these trees

fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" & GenusSpecies!="Unknown - Unknown" & !is.na(WOType) & WOType!="Misc. Work" ) %>% ggplot(aes(x=GenusSpecies,fill=WOType))+geom_bar()
fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" & GenusSpecies!="Unknown - Unknown" & !is.na(WOType) & WOType!="Misc. Work" ) %>% ggplot(aes(x=GenusSpecies,fill=WOType))+geom_bar(position = "fill") + ylab("Proportion")

fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" & GenusSpecies!="Unknown - Unknown" & !is.na(WOType) & WOType!="Misc. Work" & !is.na(WOEquipment) & WOEquipment!="Chipper") %>% ggplot(aes(x=WOEquipment,fill=GenusSpecies))+geom_bar()  
fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" & GenusSpecies!="Unknown - Unknown" & !is.na(WOType) & WOType!="Misc. Work" & !is.na(WOEquipment) & WOEquipment!="Chipper") %>% ggplot(aes(x=GenusSpecies,fill=WOEntity))+geom_bar()  

fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" & GenusSpecies!="Unknown - Unknown" & !is.na(WOType)) %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=GenusSpecies))+geom_point()  

fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" & GenusSpecies!="Unknown - Unknown" & !is.na(WOType)) %>% ggplot(aes(x=BoroughCode,fill=GenusSpecies))+geom_bar()

fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" & GenusSpecies!="Unknown - Unknown" & !is.na(WOType) & GenusSpecies=="") %>% ggplot(aes(x=BoroughCode,fill=GenusSpecies))+geom_bar()



# Explore Diameter of these trees:

fsrfistpwo_df %>% filter(ComplaintType!="New Tree Request" & DBH<250) %>% ggplot(aes(x=DBH))+geom_histogram(binwidth = 1)

fsrfistpwo_df %>% filter(ComplaintType!="New Tree Request" & DBH<50) %>% ggplot(aes(x=DBH))+geom_histogram(binwidth = 1)

# Explore Proportion of Species

fsrfistpwo_df %>% filter(ComplaintType!="New Tree Request" & DBH<50 & SpeciesCnt>1500 & GenusSpecies !="Unknown - Unknown") %>% ggplot(aes(x=DBH,fill=GenusSpecies))+geom_histogram(aes(y = ..density..),binwidth = 1)

# Most of request for Norway Maple is for DBH<22 While London plantree most cases when DBH are more than 20

# Pin Oak has more cases at DBH more than 20 while Pyrus and Tilia as well as Callery Pear  has most cases at less than 20 DBH

# Thornless Honeylocust has more cases in 10-20 DBH

# Explore Density of DBH 

fsrfistpwo_df %>% filter(ComplaintType!="New Tree Request" & DBH<50 & SpeciesCnt>1500 & GenusSpecies!="Unknown - Unknown") %>% ggplot(aes(x=DBH,col=GenusSpecies))+geom_density()

ftp_df %>% filter(DBH<50 & GenusSpecies!="Unknown - Unknown" ) %>% ggplot(aes(x=DBH,col=GenusSpecies))+geom_density()

t_df<-ftp_df %>% group_by(GenusSpecies) %>% summarise(Tcnt=n()) %>% arrange(desc(Tcnt)) 

ftp_df<-full_join(ftp_df,t_df)

ftp_df %>% filter(DBH<50 & GenusSpecies!="Unknown - Unknown" & Tcnt> 22000 ) %>% ggplot(aes(x=DBH,col=GenusSpecies))+geom_density()


# There is Normal Curve for each species at specific DBH in these complaints


# Join Community Demo with SR 



fsrfistpwo_df<-fsrfistpwo_df %>% rename(CommunityBoard=CommunityBoard.x)

fsrfistpwodemo_df<-full_join(fsrfistpwo_df,community_demo_Complaintytype_df,by=c("CommunityBoard","CommunityBoard"))


fsrfistpwodemo_df<-fsrfistpwodemo_df %>% rename(BoroughCode=BoroughCode.x)


# Calculate Time for Inspection and time for WO

# Convert Inspection date into Date format.

fsrfistpwodemo_df<-fsrfistpwodemo_df %>% mutate(myInCreatedDate=as.Date(InCreatedDate,"%m/%d/%Y"))
fsrfistpwodemo_df<- fsrfistpwodemo_df %>% mutate(myInUpdatedDate=as.Date(InUpdatedDate,"%m/%d/%Y"))
fsrfistpwodemo_df<- fsrfistpwodemo_df %>% mutate(tempcreatedate=myInCreatedDate)%>% separate(tempcreatedate,c("InCyear","InCMonth","InCDate"),sep="-")
fsrfistpwodemo_df<- fsrfistpwodemo_df %>% mutate(tempupdatedate=myInUpdatedDate)%>% separate(tempupdatedate,c("InUyear","InUMonth","InUDate"),sep="-")
fsrfistpwodemo_df<-fsrfistpwodemo_df %>% mutate(myWoCreatedDate=as.Date(WOCreatedDate,"%m/%d/%Y"))
fsrfistpwodemo_df<- fsrfistpwodemo_df %>% mutate(myWoUpdatedDate=as.Date(WOUpdatedDate,"%m/%d/%Y"))
fsrfistpwodemo_df<- fsrfistpwodemo_df %>% mutate(tempcreatedate=myWoCreatedDate)%>% separate(tempcreatedate,c("WoCyear","WoCMonth","WoCDate"),sep="-")
fsrfistpwodemo_df<- fsrfistpwodemo_df %>% mutate(tempupdatedate=myWoUpdatedDate)%>% separate(tempupdatedate,c("WoUyear","WoUMonth","WoUDate"),sep="-")

#Add TimeDiff between service request creation and inspection Creation 

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(InDelayTime=myInCreatedDate-myCreatedDate)

fsrfistpwodemo_df$InDelayTime<- as.numeric(fsrfistpwodemo_df$InDelayTime)

# Analysis of inspection delay

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & InDelayTime<200) %>% ggplot(aes(x=InDelayTime,fill=ComplaintType))+geom_bar()

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & InDelayTime<200) %>% ggplot(aes(x=InDelayTime,fill=SRType))+geom_bar()

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & InDelayTime<200) %>% ggplot(aes(x=InDelayTime,fill=BoroughCode))+geom_bar()

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & InDelayTime<200 & ComplaintType!="New Tree Request") %>% ggplot(aes(x=InDelayTime,fill=CreatedMonth))+geom_bar()

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & InDelayTime<200 & ComplaintType!="New Tree Request") %>% ggplot(aes(x=InDelayTime,fill=SRSource))+geom_bar()

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & InDelayTime<200 & ComplaintType!="New Tree Request") %>% ggplot(aes(x=BoroughCode,y=InDelayTime))+geom_boxplot() + facet_wrap(~ComplaintType)

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & ResponseTime>0 & InDelayTime>0 &ComplaintType!="New Tree Request" & !is.na(WOType)) %>% ggplot(aes(x=ResponseTime,y=InDelayTime,col=BoroughCode))+geom_point()

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & ResponseTime>0 & InDelayTime>0 & ComplaintType!="New Tree Request" & !is.na(WOType)) %>% ggplot(aes(x=log(InDelayTime),col=CreatedMonth))+geom_density()


fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & !is.na(WOType) & ComplaintType=="Damaged Tree" ) %>% ggplot(aes(x=BoroughCode,y=InDelayTime))+geom_boxplot()+facet_wrap(~SRType)

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & !is.na(WOType) & ComplaintType=="Dead Tree" ) %>% ggplot(aes(x=BoroughCode,y=InDelayTime))+geom_boxplot()+facet_wrap(~SRType)

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & !is.na(WOType) & ComplaintType=="Root/Sewer/Sidewalk Condition" ) %>% ggplot(aes(x=BoroughCode,y=InDelayTime))+geom_boxplot()+facet_wrap(~SRType)

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & !is.na(WOType) & ComplaintType=="Dead/Dying Tree" ) %>% ggplot(aes(x=BoroughCode,y=InDelayTime))+geom_boxplot()+facet_wrap(~SRType)

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & !is.na(WOType) & ComplaintType=="Overgrown Tree/Branches" ) %>% ggplot(aes(x=BoroughCode,y=InDelayTime))+geom_boxplot()+facet_wrap(~SRType)

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & InDelayTime>0 & !is.na(WOType) & ComplaintType=="Illegal Tree Damage" ) %>% ggplot(aes(x=BoroughCode,y=InDelayTime))+geom_boxplot()+facet_wrap(~SRType)



# Calculate and Add Inspection time

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(InspTime=myInUpdatedDate-myInCreatedDate)

fsrfistpwodemo_df$InspTime<- as.numeric(fsrfistpwodemo_df$InspTime)

#fsrfistpwodemo_df %>% ggplot(aes(x=InspTime,y=ResponseTime))+geom_point()

fsrfistpwodemo_df %>% ggplot(aes(x=InspTime,fill=ComplaintType))+geom_histogram()

fsrfistpwodemo_df %>% ggplot(aes(x=InspTime,fill=InspectionType))+geom_histogram()

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & InspTime>0 & !is.na(WOType) & ComplaintType=="Overgrown Tree/Branches" )%>%ggplot(aes(x=BoroughCode,y=InspTime))+geom_boxplot()+facet_wrap(~WOType)
fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & InspTime>0 & !is.na(WOType) & ComplaintType=="Damaged Tree" )%>%ggplot(aes(x=BoroughCode,y=InspTime))+geom_boxplot()+facet_wrap(~InspectionType)
fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & InspTime>0  & !is.na(WOType) & ComplaintType=="Root/Sewer/Sidewalk Condition" )%>%ggplot(aes(x=BoroughCode,y=InspTime))+geom_boxplot()+facet_wrap(~InspectionType)
fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & InspTime>0  & !is.na(WOType) & ComplaintType=="Overgrown Tree/Branches" )%>%ggplot(aes(x=BoroughCode,y=InspTime))+geom_boxplot()+facet_wrap(~InspectionType)
fsrfistpwodemo_df %>% filter(InspectionStatus=="Closed" & InspTime>0  & !is.na(WOType) & ComplaintType=="Root/Sewer/Sidewalk Condition" )%>%ggplot(aes(x=BoroughCode,y=InspTime))+geom_boxplot()+facet_wrap(~InspectionType)
fsrfistpwodemo_df %>% filter(InspectionStatus=="Closed" & InspTime>0  & !is.na(WOType) & ComplaintType=="Damaged Tree" )%>%ggplot(aes(x=BoroughCode,y=InspTime))+geom_boxplot()+facet_wrap(~InspectionType)
fsrfistpwodemo_df %>% filter(InspectionStatus=="Closed" & InspTime>0  & !is.na(WOType) & ComplaintType=="Damaged Tree" )%>%ggplot(aes(x=log(InspTime),col=SRType))+geom_density()
fsrfistpwodemo_df %>% filter(InspectionStatus=="Closed" & InspTime>0  & !is.na(WOType) & ComplaintType=="Damaged Tree" )%>%ggplot(aes(x=log(InspTime),col=InspectionType))+geom_density()
fsrfistpwodemo_df %>% filter(InspectionStatus=="Closed" & InspTime>0  & !is.na(WOType) & ComplaintType=="Damaged Tree" )%>%ggplot(aes(x=log(InspTime),col=InspectionTPCondition))+geom_density()
fsrfistpwodemo_df %>% filter(InspectionStatus=="Closed" & InspTime>0  & !is.na(WOType) & ComplaintType=="Damaged Tree" & !is.na(InspectionTPStructure))%>%ggplot(aes(x=log(InspTime),col=InspectionTPStructure))+geom_density() +facet_wrap(~InspectionType)
fsrfistpwodemo_df %>% filter(InspectionStatus=="Closed" & InspTime>0  & !is.na(WOType) & !is.na(InspectionTPStructure) & !is.na(DBH))%>%ggplot(aes(x=log(InspTime),col=factor(DBH)))+geom_density()

# Not much relationship with DBH

fsrfistpwodemo_df %>% filter(InspectionStatus=="Closed" & InspTime>0 & DBH<250  & !is.na(WOType) & !is.na(InspectionTPStructure) & !is.na(DBH))%>%ggplot(aes(x=DBH,y=log(InspTime),col=SRType))+geom_point()


#Add TimeDiff between service request creation and inspection Closure

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(InCloseTime=myInUpdatedDate-myCreatedDate)

fsrfistpwodemo_df$InCloseTime<- as.numeric(fsrfistpwodemo_df$InCloseTime)

#Add TimeDiff between Inspection updated date and WO Creation

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(WoDelayTime=myWoCreatedDate-myInUpdatedDate)

fsrfistpwodemo_df$WoDelayTime <- as.numeric(fsrfistpwodemo_df$WoDelayTime)

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(InWoDelayTime=myWoCreatedDate-myInCreatedDate)

fsrfistpwodemo_df$InWoDelayTime <- as.numeric(fsrfistpwodemo_df$InWoDelayTime)

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & InDelayTime>0 & !is.na(WOType) & ComplaintType=="Overgrown Tree/Branches" )%>%ggplot(aes(x=BoroughCode,y=InWoDelayTime))+geom_boxplot()+facet_wrap(~WOType)
fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & InDelayTime>0 & !is.na(WOType) & ComplaintType=="Damaged Tree" )%>%ggplot(aes(x=BoroughCode,y=InWoDelayTime))+geom_boxplot()+facet_wrap(~WOType)
fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & !is.na(WOType) & ComplaintType=="Root/Sewer/Sidewalk Condition" )%>%ggplot(aes(x=BoroughCode,y=InWoDelayTime))+geom_boxplot()+facet_wrap(~WOType)
fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & InDelayTime>0 & !is.na(WOType) & ComplaintType=="Illegal Tree Damage" )%>%ggplot(aes(x=BoroughCode,y=InWoDelayTime))+geom_boxplot() + facet_wrap(~WOType)

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & WoDelayTime>0 & !is.na(WOType) & ComplaintType=="Overgrown Tree/Branches" )%>%ggplot(aes(x=SRType,y=InWoDelayTime))+geom_boxplot()

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus)  & !is.na(WOType) & ComplaintType=="Overgrown Tree/Branches" )%>%ggplot(aes(x=WOType,y=InWoDelayTime))+geom_boxplot()




# Calculate Total WO Completion time

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(WoCloseTime=myWoUpdatedDate-myWoCreatedDate)

fsrfistpwodemo_df$WoCloseTime <- as.numeric(fsrfistpwodemo_df$WoCloseTime)

fsrfistpwodemo_df %>% filter(!is.na(ComplaintType)) %>%ggplot(aes(x=WoCloseTime,fill=ComplaintType))+geom_histogram(position = "fill")

fsrfistpwodemo_df %>% filter(!is.na(ComplaintType) & WoCloseTime>0 & !is.na(WOType)) %>%ggplot(aes(x=WoCloseTime,fill=SRType))+geom_histogram(position = "fill")

# Relationship between WOType and SR Type

fsrfistpwodemo_df %>% filter(!is.na(ComplaintType) & WoCloseTime>0 & !is.na(WOType) & ComplaintType=="Overgrown Tree/Branches" & WoCloseTime>0 & !is.na(WOType)) %>%ggplot(aes(x=SRType,fill=WOType))+geom_bar()

fsrfistpwodemo_df %>% filter(!is.na(ComplaintType) & WoCloseTime>0 & !is.na(WOType) & ComplaintType=="Root/Sewer/Sidewalk Condition" & WoCloseTime>0 & !is.na(WOType)) %>%ggplot(aes(x=SRType,fill=WOType))+geom_bar() + facet_wrap(~BoroughCode)

fsrfistpwodemo_df %>% filter(!is.na(ComplaintType) & WoCloseTime>0 & !is.na(WOType) & ComplaintType=="Root/Sewer/Sidewalk Condition" & WoCloseTime>0 & !is.na(WOType)) %>%ggplot(aes(x=SRType,fill=WOType))+geom_bar(position = "fill") + facet_wrap(~BoroughCode)


fsrfistpwodemo_df %>% filter(WoCloseTime>0 & !is.na(WOType) ) %>% ggplot(aes(x=log(WoCloseTime),col=ComplaintType))+ geom_density()


fsrfistpwodemo_df %>% filter(WoCloseTime>0 & !is.na(WOType) & ComplaintType=="Damaged Tree" ) %>% ggplot(aes(x=WoCloseTime,fill=WOType))+ geom_histogram(position = "fill")

fsrfistpwodemo_df %>% filter(WoCloseTime>0 & !is.na(WOType) & ComplaintType== "Overgrown Tree/Branches" ) %>% ggplot(aes(x=WoCloseTime,fill=WOType))+ geom_histogram(position = "fill")

fsrfistpwodemo_df %>% filter(WoCloseTime>0 & WoCloseTime<500 & !is.na(WOType) & ComplaintType== "Root/Sewer/Sidewalk Condition"  ) %>% ggplot(aes(y=WoCloseTime,x=WOType))+ geom_boxplot() + facet_wrap(~BoroughCode)

fsrfistpwodemo_df %>% filter(WoCloseTime>0 & WoCloseTime<500 & !is.na(WOType) & ComplaintType== "Damaged Tree"  ) %>% ggplot(aes(x=WoCloseTime,fill=WOType))+ geom_histogram() + facet_wrap(~BoroughCode)

fsrfistpwodemo_df %>% filter(WoCloseTime>0 & WoCloseTime<500 & !is.na(WOType) & ComplaintType== "Overgrown Tree/Branches"  ) %>% ggplot(aes(x=WoCloseTime,fill=WOType))+ geom_histogram() + facet_wrap(~BoroughCode)

fsrfistpwodemo_df %>% filter(WoCloseTime>0 & WoCloseTime<500 & !is.na(WOType) & SRType== "Trees and Sidewalks"  ) %>% ggplot(aes(y=WoCloseTime,x=BoroughCode))+ geom_boxplot() + facet_wrap(~WOType)

fsrfistpwodemo_df %>% ggplot(aes(x=WoCloseTime,fill=BoroughCode))+geom_histogram()

fsrfistpwodemo_df %>% filter(SpeciesCnt>2000 & GenusSpecies!="Unknown - Unknown") %>%ggplot(aes(x=WoCloseTime,fill=GenusSpecies))+geom_histogram(binwidth = 20)

fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRlocCnt<7) %>%ggplot(aes(x=WoCloseTime,fill=factor(SRlocCnt)))+geom_histogram(position = "fill")

fsrfistpwodemo_df %>% filter(CntTPT>1) %>%ggplot(aes(x=WoCloseTime,fill=factor(CntTPT)))+geom_histogram()


#Add TimeDiff between Service Creation and WO Update

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(SRWocloseTime=myWoUpdatedDate-myCreatedDate)

fsrfistpwodemo_df$SRWocloseTime <- as.numeric(fsrfistpwodemo_df$SRWocloseTime)

#SRClosure TIme and Created Month

fsrfistpwodemo_df %>% filter(SRWocloseTime>0 & !is.na(ComplaintType)) %>% ggplot(aes(x=log(SRWocloseTime),col=BoroughCode))+geom_density()+ facet_wrap(~ComplaintType)

fsrfistpwodemo_df %>% filter(SRWocloseTime>200 & SRWocloseTime<600 &  !is.na(ComplaintType) ) %>% ggplot(aes(x=SRWocloseTime,fill=CreatedMonth))+geom_histogram(aes(y = ..density..), binwidth = 10,position = "fill")

fsrfistpwodemo_df %>% filter(SRWocloseTime>0 &  !is.na(SRType) ) %>% ggplot(aes(x=SRWocloseTime,fill=WOType))+geom_histogram(aes(y = ..density..), binwidth = 10,position = "fill")



# Explore Response time, Inspection Delay time, Inspection close time, Wo Delay time and Overall Closure time

fsrfistpwodemo_df%>% filter(ResponseTime>0)%>%ggplot(aes(x=ResponseTime))+geom_density()

fsrfistpwodemo_df%>% filter(InDelayTime>0)%>%ggplot(aes(x=InDelayTime))+geom_density()

fsrfistpwodemo_df%>% filter(WoDelayTime>0)%>%ggplot(aes(x=WoDelayTime))+geom_density()

fsrfistpwodemo_df%>% filter(SRWocloseTime>0)%>%ggplot(aes(x=SRWocloseTime))+geom_density()

fsrfistpwodemo_df%>% filter(SRWocloseTime>0)%>%ggplot(aes(x=SRWocloseTime,y=ResponseTime))+geom_point()

fsrfistpwodemo_df%>% filter(InDelayTime>0)%>%ggplot(aes(x=InDelayTime,y=ResponseTime))+geom_point()

fsrfistpwodemo_df%>% filter(WoDelayTime>0)%>%ggplot(aes(x=WoDelayTime,y=ResponseTime))+geom_point()

fsrfistpwodemo_df%>% filter(InCloseTime>0)%>%ggplot(aes(x=InCloseTime,y=ResponseTime))+geom_point()

# Inspection Delay time is contributing most to Response time. It means service closure is considered when inpection is updated.

 # Explore relationship between SRWOClosure time on Borough, Community Board and Species

# Is Repeating call affecting Time?




fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRlocCnt<14 &SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=factor(SRlocCnt)))+geom_density()

fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRlocCnt<14 &SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=factor(SRlocCnt)))+geom_density()

fsrfistpwodemo_df %>% filter(SRlocCnt>1  & SRWocloseTime>0 ) %>% ggplot(aes(x=ComplaintType,fill=SRType))+geom_bar()

fsrfistpwodemo_df %>% filter(SRlocCnt>1  & SRWocloseTime>0 ) %>% ggplot(aes(x=ComplaintType,fill=BoroughCode))+geom_bar()

fsrfistpwodemo_df %>% filter(SRlocCnt>1  & SRWocloseTime>0 ) %>% ggplot(aes(x=ComplaintType,fill=BoroughCode))+geom_bar()

fsrfistpwodemo_df %>% filter(SRlocCnt>1  & SRWocloseTime>0 ) %>% ggplot(aes(x=ComplaintType,fill=WOType))+geom_bar()


fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=BoroughCode))+geom_density()

fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=ComplaintType))+geom_density()

# Multiple request on Same location and complaint type Root/Sewar/Sidewalk has more closure time

fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=ComplaintType))+geom_density()

# Explore Frequent SR on same Tree point affecting Closure

fsrfistpwodemo_df %>% filter(CntTPT>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=ComplaintType))+geom_density()

fsrfistpwodemo_df %>% filter(CntTPT>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=SRCategory))+geom_density()

# Multiple request on Same TreePoint and complaint type Root/Sewar/Sidewalk, Dead Tree and OverGrown Tree Branches has more closure time

# Explore WO Type on SRWoclosetime

fsrfistpwodemo_df %>% filter(CntTPT>1 & SRWocloseTime>100 & SRCategory!="Plant Tree") %>% ggplot(aes(x=SRWocloseTime,col=SRResolution))+geom_density()

fsrfistpwodemo_df %>% filter(CntTPT>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=WOType))+geom_density()

# Limb down has most cases under 400 days.

# Explore Tree Diameter Affect Time

fsrfistpwodemo_df %>% filter(CntTPT>1 & SRWocloseTime>0 & SpeciesCnt>2000 & DBH<60) %>% ggplot(aes(x=DBH,y=SRWocloseTime,col=SpeciesCnt))+geom_point()+geom_smooth()

fsrfistpwodemo_df %>% filter(CntTPT>2 & SRWocloseTime>0& SRlocCnt>5& DBH<45) %>% ggplot(aes(x=DBH,y=SRWocloseTime,col=SRlocCnt))+geom_point()+geom_smooth()


#More than Diameter Species is affecting Response Time

fsrfistpwodemo_df %>% filter(CntTPT>2 & SRWocloseTime>0& SRlocCnt>5& DBH<45) %>% ggplot(aes(x=DBH,y=SRWocloseTime,col=SRCategory))+geom_point()


# Explore Time by Borough and Community Board

fsrfistpwodemo_df%>%ggplot(aes(x=factor(SRCategory),y= ResponseTime))+geom_boxplot()+ facet_wrap(~BoroughCode)  # Box Plot for SR Resoution

fsrfistpwodemo_df%>%ggplot(aes(x=factor(SRCategory),y= SRWocloseTime))+geom_boxplot()+ facet_wrap(~BoroughCode)  # Box Plot for SR Resoution

fsrfistpwodemo_df%>% filter(SRWocloseTime>0&!is.na(ComplaintType))%>%ggplot(aes(x=factor(ComplaintType),y= SRWocloseTime))+geom_boxplot()+ facet_wrap(~BoroughCode)  # Box Plot for SR Resoution

# Explore which Borough Has more repating calls

fsrfistpwodemo_df %>% ggplot(aes(x=SRlocCnt,col=BoroughCode)) + geom_density()

# ReMove Outlier Loc count > 10

fsrfistpwodemo_df %>% filter(SRlocCnt>1  & SRlocCnt<10 & !is.na(BoroughCode ))%>%ggplot(aes(x=SRlocCnt,col=BoroughCode)) + geom_density()

# Queens and Brooklyn has most repeated calls

# Explore What is Reason for these calls : Complaint type

fsrfistpwodemo_df %>% filter(SRlocCnt<10 &SRlocCnt>2 & !is.na(ComplaintType))%>%ggplot(aes(x=SRlocCnt,col=ComplaintType)) + geom_density()

# Repeated Calls RootSewar and overgrown Branches

# Explore SR Resolution 

fsrfistpwodemo_df %>% filter(SRlocCnt<10 &SRlocCnt>2 & !is.na(ComplaintType))%>%ggplot(aes(x=SRlocCnt,col=SRResolution)) + geom_density()

# Explore WO Category for these cases

fsrfistpwodemo_df %>% filter(SRlocCnt<10 &SRlocCnt>2 & !is.na(ComplaintType) & !is.na(WOCategory)& WOCategory !="Claim" & WOCategory!="Misc Work" & WOCategory != "Pest and Disease")%>%ggplot(aes(x=SRlocCnt,col=WOCategory)) + geom_density()

# Tree Sidewalk Repair and Tree Removal has most cases

#Explore Time Required for full closure

fsrfistpwodemo_df %>% filter(SRlocCnt<10 &SRlocCnt>2 & !is.na(ComplaintType) & !is.na(WOCategory)& SRWocloseTime>0)%>%ggplot(aes(x=SRWocloseTime,fill=WOCategory)) + geom_bar()

# WO Category Tree Sidewalk Repair and Tree Removal take has spread to max Time

fsrfistpwodemo_df %>% filter(SRlocCnt<10 &SRlocCnt>2 & !is.na(ComplaintType) & !is.na(WOCategory)& SRWocloseTime>0)%>%ggplot(aes(x=SRWocloseTime,fill=WOCategory)) + geom_bar()


# Explore SR Source and Work Completion


fsrfistpwodemo_df%>%ggplot(aes(x=WOCategory,fill=SRSource)) + geom_bar()  # WO category is mostly NA

# Explore SR Source and SRResolution

fsrfistpwodemo_df%>%ggplot(aes(x=SRCategory,fill=SRSource)) + geom_bar()  # Two Main Source of Request 311 and Public Website
fsrfistpwodemo_df%>%ggplot(aes(x=ComplaintType,fill=SRSource)) + geom_bar()  # Two Main Source of Request 311 and Public Website


fsrfistpwodemo_df%>%ggplot(aes(x=ResponseTime,fill=SRSource)) + geom_histogram()  #  Public Website cases has been resolved ealier

fsrfistpwodemo_df%>%ggplot(aes(x=factor(ComplaintType),y= SRWocloseTime))+geom_boxplot()+ facet_wrap(~SRSource) 

fsrfistpwodemo_df%>%ggplot(aes(x=factor(ComplaintType),y= SRWocloseTime))+geom_boxplot()+ facet_wrap(~BoroughCode) 

fsrfistpwodemo_df%>% filter(!is.na(WOType) & SRlocCnt>1 & CntTPT>1) %>% ggplot(aes(x=factor(BoroughCode),y= SRWocloseTime))+geom_boxplot()+ facet_wrap(~ComplaintType) 

fsrfistpwodemo_df%>%ggplot(aes(x=factor(ComplaintType),y= ResponseTime))+geom_boxplot()+ facet_wrap(~SRSource) 

fsrfistpwodemo_df%>%ggplot(aes(x=factor(ComplaintType),y= InDelayTime))+geom_boxplot()+ facet_wrap(~SRSource) 

fsrfistpwodemo_df%>%ggplot(aes(x=factor(ComplaintType),y= WoDelayTime))+geom_boxplot()+ facet_wrap(~SRSource) 

# 311 and Public Website has almost same response Time

# SR source by borough


fsrfistpwodemo_df%>%ggplot(aes(x=BoroughCode,fill=SRSource)) + geom_bar()  # Two Main Source of Request 311 and Public Website

fsrfistpwodemo_df%>%ggplot(aes(x=BoroughCode,fill=SRSource)) + geom_bar(position ="fill")  # Two Main Source of Request 311 and Public Website

# Manhattan has around 40% of total Cases from Website. Others has around 25 to 30%

# Explore Repeated Calls from Same location

fsrfistpwodemo_df%>% filter(SRlocCnt<10 &SRlocCnt>2 & !is.na(ComplaintType) & !is.na(WOCategory)& SRWocloseTime>0&CntTPT>1) %>% ggplot(aes(x=BoroughCode,fill=SRSource))+geom_bar() 

# Brooklyn has most repeated calls and Public website request are almost more than Total requests where WO is created

fsrfistpwodemo_df%>% filter(SRlocCnt>2 & !is.na(ComplaintType) &  SRWocloseTime > 0 & CntTPT>1) %>% ggplot(aes(x=BoroughCode,fill=SRSource))+geom_bar() 

fsrfistpwodemo_df%>% filter(SRlocCnt>2) %>% ggplot(aes(x=BoroughCode,fill=SRSource))+geom_bar() 

fsrfistpwodemo_df%>% filter(CntTPT>2 & BoroughCode=="Brooklyn") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=factor(CntTPT)))+geom_point() 

# 3 times service request has been raised for repeated Tree in Brooklyn Most

fsrfistpwodemo_df%>% filter(CntTPT>2 & BoroughCode=="Queens") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=factor(CntTPT)))+geom_point() 

# 3 times service request has been raised for repeated Tree in Queens Most

fsrfistpwodemo_df%>% filter(CntTPT>2 & BoroughCode=="Manhattan") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=factor(CntTPT)))+geom_point() 

# 3 times service request has been raised for repeated Tree in Manhattan Most

fsrfistpwodemo_df%>% filter(CntTPT>2 & BoroughCode=="Bronx") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=factor(CntTPT)))+geom_point() 

# 3 times service request has been raised for repeated location in Bronx Most




# Explore Workorder Completion time

fsrfistpwodemo_df %>% filter(!is.na(WOType)) %>% ggplot(aes(x=WoUMonth,fill=WOType))+geom_bar()

# October has most updated Work order

fsrfistpwodemo_df %>% filter(!is.na(WOType)) %>% ggplot(aes(x=WoCMonth,fill=WOType))+geom_bar()

# July, Aug and Sep are most Work order created


fsrfistpwodemo_df %>% filter(!is.na(WOType) & ResponseTime>0 & SRWocloseTime>0 & WoDelayTime>0 & WoCloseTime>0 & Cyear=="2015" & WoUyear =="2016") %>% ggplot(aes(x=WoUMonth,fill=CMonth))+geom_bar()

fsrfistpwodemo_df %>% filter(!is.na(WOType) & ResponseTime>0 & SRWocloseTime>0 & WoDelayTime>0 & WoCloseTime>0) %>% ggplot(aes(x=CMonth,fill=WoUMonth))+geom_bar()

# distribution of various time

fsrfistpwodemo_df %>% filter(!is.na(WOType) & ResponseTime>0 & SRWocloseTime>0 ) %>% ggplot(aes(x=factor(SRCategory),y= WoCloseTime))+geom_boxplot()+ facet_wrap(~BoroughCode)

fsrfistpwodemo_df %>% filter(!is.na(WOType) & ResponseTime>0 & SRWocloseTime>0 ) %>% ggplot(aes(x=factor(SRResolution),y= WoCloseTime))+geom_boxplot()+ facet_wrap(~BoroughCode)

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & ResponseTime>0 & SRWocloseTime>0 ) %>% ggplot(aes(x=factor(ComplaintType),y= InspTime))+geom_boxplot()+ facet_wrap(~BoroughCode)

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & ResponseTime>0 & SRWocloseTime>0) %>% ggplot(aes(x=factor(WOType),y= InspTime))+geom_boxplot()+ facet_wrap(~BoroughCode)

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & ResponseTime>0 & SRWocloseTime>0 & !is.na(WOCategory)) %>% ggplot(aes(x=factor(BoroughCode),y= WoCloseTime))+geom_boxplot()+ facet_wrap(~WOCategory)

# Same WO Category is taking different time in boroughs

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus) & ResponseTime>0 & SRWocloseTime>0 & !is.na(WOCategory) & SpeciesCnt>2000) %>% ggplot(aes(x=WOCategory,y= WoCloseTime))+geom_boxplot()+ facet_wrap(~GenusSpecies)


# Analysis on Community

community_demo_Complaintytype_df %>% ggplot(aes(x=`Female 18 Over`,y= totalRequest)) +geom_point()

community_demo_Complaintytype_df %>% ggplot(aes(x=`Male 18 over`,y= totalRequest)) +geom_point()

community_demo_Complaintytype_df %>% ggplot(aes(x=White,y= totalRequest)) +geom_point()

community_demo_Complaintytype_df %>% ggplot(aes(x=Asian,y= totalRequest)) +geom_point()

community_demo_Complaintytype_df %>% ggplot(aes(x=`Population 18 Years and over`,y= totalRequest,col=BoroughCode)) +geom_point(size=5)

community_demo_Complaintytype_df %>% ggplot(aes(x=Household,y= totalRequest,col=BoroughCode)) +geom_point(size=5)


# Modeling


fsrfistpwodemo_df$SRCategory = as.factor(fsrfistpwodemo_df$SRCategory)
fsrfistpwodemo_df$SRType = as.factor(fsrfistpwodemo_df$SRType)
fsrfistpwodemo_df$WOType = as.factor(fsrfistpwodemo_df$WOType)
fsrfistpwodemo_df$ComplaintType = as.factor(fsrfistpwodemo_df$ComplaintType)

contrasts(fsrfistpwodemo_df$SRCategory)

coef(summary(lm(SRWocloseTime ~ C(SRCategory, base=1),   data=fsrfistpwodemo_df)))
summary(lm(SRWocloseTime ~ C(SRCategory, base=1),   data=fsrfistpwodemo_df))

fsrfistpwodemo_df$BoroughCode.x = as.factor(fsrfistpwodemo_df$BoroughCode.x)
summary(lm(ResponseTime ~ C(BoroughCode.x, base=1),   data=fsrfistpwodemo_df)) #5.6

summary(lm(ResponseTime ~ BoroughCode.x+SRCategory,   data=fsrfistpwodemo_df))  # 20.2   

#Adding DBH reduces R sq to 17.57


summary(lm(ResponseTime ~ SRCategory,   data=fsrfistpwodemo_df))  # 15.40

summary(lm(ResponseTime ~ BoroughCode.x,   data=fsrfistpwodemo_df)) # 5.6

summary(lm(SRWocloseTime ~ DBH,   data=fsrfistpwodemo_df)) # .3

summary(lm(ResponseTime ~ InspectionTPStructure,   data=fsrfistpwodemo_df))  # .3

summary(lm(ResponseTime ~ SRType,   data=fsrfistpwodemo_df))  # 17.3  Almost most of them are non significant

summary(lm(ResponseTime ~ WOType,   data=fsrfistpwodemo_df)) # 27.35  Few are one star significant

summary(lm(ResponseTime ~ ComplaintType,   data=fsrfistpwodemo_df))  # 16.7  New Tree request is not significant

summary(lm(ResponseTime ~ CreatedMonth,   data=fsrfistpwodemo_df))  # 2.4

summary(lm(ResponseTime ~ GenusSpecies,   data=fsrfistpwodemo_df)) # 1.5

summary(lm(ResponseTime ~ CommunityBoard,   data=fsrfistpwodemo_df)) .3

summary(lm(SRWocloseTime ~ InspTime,   data=fsrfistpwodemo_df))  #  64.3

summary(lm(ResponseTime ~ InDelayTime,   data=fsrfistpwodemo_df))  # 50.5

summary(lm(ResponseTime ~ Household,   data=fsrfistpwodemo_df)) # no dependency

summary(lm(ResponseTime ~ CntTPT,   data=fsrfistpwodemo_df)) # less depedency

summary(lm(ResponseTime ~ SRlocCnt,   data=fsrfistpwodemo_df)) # no depedency

summary(lm(ResponseTime ~ TaxClass,   data=fsrfistpwodemo_df)) # less depedency

summary(lm(ResponseTime ~ Longitude.x+Latitude.x,   data=fsrfistpwodemo_df)) # less depedency

summary(lm(ResponseTime ~ SRSource,   data=fsrfistpwodemo_df)) # less depedency

summary(lm(ResponseTime ~ SRPriority,   data=fsrfistpwodemo_df)) # less depedency

summary(lm(ResponseTime ~ Cyear,   data=fsrfistpwodemo_df)) # 9.0

summary(lm(ResponseTime ~ UMonth,   data=fsrfistpwodemo_df)) # 13.62

summary(lm(ResponseTime ~ WOCategory,   data=fsrfistpwodemo_df)) # 27.5  Only Tree Planting is significant

summary(lm(ResponseTime ~ WOContract,   data=fsrfistpwodemo_df)) # 34.93


mod.resolution<-summary(lm(ResponseTime ~ SRResolution,   data=fsrfistpwodemo_df)) # no depedency




str(fsrfistpwodemo_df)
fsrfistpwodemo_df$SRCategory <- as.factor(fsrfistpwodemo_df$SRCategory)
fsrfistpwodemo_df$SRType <- as.factor(fsrfistpwodemo_df$SRType)
fsrfistpwodemo_df$SRPriority <- as.factor(fsrfistpwodemo_df$SRPriority)
fsrfistpwodemo_df$SRSource <- as.factor(fsrfistpwodemo_df$SRSource)
fsrfistpwodemo_df$SRResolution <- as.factor(fsrfistpwodemo_df$SRResolution)
fsrfistpwodemo_df$BoroughCode<- as.factor(fsrfistpwodemo_df$BoroughCode)
fsrfistpwodemo_df$ComplaintType<-as.factor(fsrfistpwodemo_df$ComplaintType)
fsrfistpwodemo_df$InspectionType<-as.factor(fsrfistpwodemo_df$InspectionType)
fsrfistpwodemo_df$InspectionTPStructure<-as.factor(fsrfistpwodemo_df$InspectionTPStructure)
fsrfistpwodemo_df$InspectionStatus<-as.factor(fsrfistpwodemo_df$InspectionStatus)
fsrfistpwodemo_df$InspectionTPCondition<-as.factor(fsrfistpwodemo_df$InspectionTPCondition)
fsrfistpwodemo_df$WOCategory<-as.factor(fsrfistpwodemo_df$WOCategory)
fsrfistpwodemo_df$WOStatus<- as.factor(fsrfistpwodemo_df$WOStatus)
fsrfistpwodemo_df$WOType<-as.factor(fsrfistpwodemo_df$WOType)
fsrfistpwodemo_df$TPCondition<-as.factor(fsrfistpwodemo_df$TPCondition)
fsrfistpwodemo_df$TPStructure<-as.factor(fsrfistpwodemo_df$TPStructure)

#install.packages('DEoptimR') 
#install.packages("caret", dependencies = TRUE)

#install.packages("caret",repos = "http://cran.r-project.org", dependencies = c("Depends", "Imports", "Suggests"))

#install.packages("RcppRoll")

#install.packages("ddalpha")

#install.packages("dimRed")

#install.packages("gower")

library("caret")



fsrforMOD_df<-fsrfistpwodemo_df %>% select(ComplaintNumber.x,SRCategory,SRType,SRPriority,SRSource,SRResolution,BoroughCode,ResponseTime,Latitude.x,Longitude.x,Cyear,CMonth,Uyear,UMonth,CreatedMonth,InspectionType,InspectionTPStructure,InspectionTPCondition,TreePointDBH,TPStructure,WOStatus,WOCategory,InDelayTime,SRWocloseTime)
str(fsrforMOD_df)

dmy<-dummyVars("~SRCategory",data = fsrforMOD_df)
trsf_SRCat<-data.frame(predict(dmy,newdata = fsrforMOD_df))

fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_SRCat)
str(fsrforMOD_df)

dmy<-dummyVars("~BoroughCode",data = fsrforMOD_df)
trsf_BoroughCode<-data.frame(predict(dmy,newdata = fsrforMOD_df))
fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_BoroughCode)

dmy<-dummyVars("~SRType",data = fsrforMOD_df)
trsf_SRType<-data.frame(predict(dmy,newdata = fsrforMOD_df))
fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_SRType)

dmy<-dummyVars("~SRResolution",data = fsrforMOD_df)
trsf_SRResolution<-data.frame(predict(dmy,newdata = fsrforMOD_df))
fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_SRResolution)

dmy<-dummyVars("~SRPriority",data = fsrforMOD_df)
trsf_SRPriority<-data.frame(predict(dmy,newdata = fsrforMOD_df))
fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_SRPriority)

dmy<-dummyVars("~SRSource",data = fsrforMOD_df)
trsf_SRSource<-data.frame(predict(dmy,newdata = fsrforMOD_df))
fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_SRSource)

dmy<-dummyVars("~CreatedMonth",data = fsrforMOD_df)
trsf_CreatedMonth<-data.frame(predict(dmy,newdata = fsrforMOD_df))
fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_CreatedMonth)

fsrforMOD_df$Cyear<-as.numeric(fsrforMOD_df$Cyear)


fsrforMOD_df$InspectionTPStructure<-as.factor(fsrforMOD_df$InspectionTPStructure)

dmy<-dummyVars("~InspectionTPStructure",data = fsrforMOD_df)
trsf_InspectionTPStructure<-data.frame(predict(dmy,newdata = fsrforMOD_df))
fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_InspectionTPStructure)

dmy<-dummyVars("~TPStructure",data = fsrforMOD_df)
trsf_TPStructure<-data.frame(predict(dmy,newdata = fsrforMOD_df))
fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_TPStructure)

dmy<-dummyVars("~WOStatus",data = fsrforMOD_df)
trsf_WOStatus<-data.frame(predict(dmy,newdata = fsrforMOD_df))
fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_WOStatus)


dmy<-dummyVars("~WOCategory",data = fsrforMOD_df)
trsf_WOCategory<-data.frame(predict(dmy,newdata = fsrforMOD_df))
fsrforMOD_df<-cbind.data.frame(fsrforMOD_df,trsf_WOCategory)

fsrforMOD_df.new<-fsrforMOD_df[-2]

fsrforMOD_df.new<-fsrforMOD_df.new[-c(2:5)]
str(fsrforMOD_df.new)

fsrforMOD_df.new<-fsrforMOD_df.new[-c(6:8),-c(13,15)]
str(fsrforMOD_df.new)


fsrforMOD_df.new<-fsrforMOD_df.new[-13]
str(fsrforMOD_df.new)

# fsrforMOD.new is data frame with dummy variables.

# linear regression model

# split Data set

#install.packages("caTools")
library(caTools)

set.seed(88)
split=sample.split(fsrforMOD_df.new$ResponseTime,SplitRatio = 0.75)
split
fsrforMOD_df.new.train=subset(fsrforMOD_df.new,split==TRUE)
str(fsrforMOD_df.new.train)

fsrforMOD_df.new.test=subset(fsrforMOD_df.new,split==FALSE)


#install.packages("h20.ai")

#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
#pkgs <- c("RCurl","jsonlite")
#for (pkg in pkgs) {
 # if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
#}

#install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wheeler/2/R")

library(h2o)
h2o.init()


# LM using H2o

train.fsrforMOD_df.new.train <- as.h2o(fsrforMOD_df.new.train)
test.fsrforMOD_df.new.test <- as.h2o(fsrforMOD_df.new.test)
colnames(train.fsrforMOD_df.new.train)
y.dep <- 3
x.indep<- c(1:2,4:5,7:173)
regression.model1 <- h2o.glm( y = y.dep, x = x.indep, training_frame = train.fsrforMOD_df.new.train,family = "gaussian")
h2o.performance(regression.model1)  # R sq = 0.4646 AIC = 1349978


predict.reg1 <- as.data.frame(h2o.predict(regression.model1, test.fsrforMOD_df.new.test))

summary(predict.reg1)
str(predict.reg1)

summary(test.fsrforMOD_df.new.test$ResponseTime)  # mean is almost equal

r2.glm<-h2o.r2(regression.model1)
rmse.glm<-h2o.rmse(regression.model1)
mae.glm<-h2o.mae(regression.model1)

r2.glm





# Random Forest using H2o

rforest.model1 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.fsrforMOD_df.new.train, ntrees = 500, mtries = 3, max_depth = 4, seed = 1122)
h2o.performance(rforest.model1) 
summary(rforest.model1)
predict.rforest1 <- as.data.frame(h2o.predict(rforest.model1, test.fsrforMOD_df.new.test))
summary(predict.rforest1)
summary(test.fsrforMOD_df.new.test$ResponseTime) 

r2.rforest<-h2o.r2(rforest.model1)
rmse.rforest<-h2o.rmse(rforest.model1)
mae.rforest<-h2o.mae(rforest.model1)

r2.rforest


# Using GBM method

gbm.model1 <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.fsrforMOD_df.new.train, ntrees = 2000, max_depth = 4, learn_rate = 0.01, seed = 1122)
h2o.performance(gbm.model1) 
summary(gbm.model1)
predict.gbm1 <- as.data.frame(h2o.predict(gbm.model1, test.fsrforMOD_df.new.test))
summary(predict.gbm1)
h2o.r2(gbm.model1)

#par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(gbm.model1) # 

summary(test.fsrforMOD_df.new.test$ResponseTime) 
summary(predict.gbm1)
summary(predict.rforest1)
summary(predict.reg1)

r2.gbm<-h2o.r2(gbm.model1)
rmse.gbm<-h2o.rmse(gbm.model1)
mae.gbm<-h2o.mae(gbm.model1)


# Putting all model together

accuracy <- data.frame(Method = c("GLM","Random forest","GBM"), RMSE   = c(rmse.glm,rmse.rforest,rmse.gbm), MAE= c(mae.glm,mae.rforest,mae.gbm), R2=c(r2.glm,r2.rforest,r2.gbm))

accuracy

# Putting all prediceted values

predictions <- data.frame(actual = fsrforMOD_df.new.test$ResponseTime,linear.regression = predict.reg1,random.forest =predict.rforest1,gbm=predict.gbm1)
head(predictions)


