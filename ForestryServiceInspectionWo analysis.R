library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
Forestry_Service_Requests <- read_csv("C:/Users/Naksh/Desktop/R Project/Forestry_Service_Requests.csv")
Forestry_Tree_Points <- read_csv("C:/Users/Naksh/Desktop/R Project/Forestry_Tree_Points.csv")
Forestry_Work_Orders <- read_csv("C:/Users/Naksh/Desktop/R Project/Forestry_Work_Orders.csv")
Forestry_Inspections <- read_csv("C:/Users/Naksh/Downloads/Forestry_Inspections.csv")
fsr_df<-Forestry_Service_Requests
fis_df<-Forestry_Inspections
ftp_df<-Forestry_Tree_Points
fwo_df<-Forestry_Work_Orders

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

fsr_df<-fsr_df %>% select(ComplaintNumber,SRCategory,SRType,SRPriority,SRSource,SRResolution,BoroughCode,CommunityBoard,Postcode,StateAssembly,StateSenate,GlobalID,ClosedDate,UpdatedDate,ComplaintType,Latitude,Longitude,myCreatedDate,myUpdatedDate,Cyear,CMonth,Uyear,UMonth,UDate,CreatedMonth,UpdatedDate,ResponseTime)

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

fsrfistpwodemo_df %>% filter(!is.na(ComplaintType))%>%ggplot(aes(x=BoroughCode,fill=ComplaintType))+geom_bar()

fsrfistpwodemo_df %>% filter(!is.na(ComplaintType))%>%ggplot(aes(x=BoroughCode,fill=ComplaintType))+geom_bar(position = "fill") + ylab("Percent")



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

fsrfistpwodemo_df %>% filter(!is.na(InspectionStatus))



fsrfistpwodemo_df %>% filter(!is.na(ComplaintType))%>%ggplot(aes(x=BoroughCode,fill=ComplaintType))+geom_bar()

fsrfistpwodemo_df %>% filter(!is.na(ComplaintType))%>%ggplot(aes(x=BoroughCode,fill=ComplaintType))+geom_bar(position = "fill")+ylab("Proportion")


# Explore relationship between TP cnt and location cnt

fsrfistpwo_df %>% ggplot(aes(x=SRlocCnt,y=CntTPT,col=BoroughCode))+geom_point()  # Broklyn has more presence

# Contingency Table Between BoroughCode and Genus Species

BoroSpecies_df<-as.data.frame.matrix(table(fsrfistpwo_df$GenusSpecies,fsrfistpwo_df$BoroughCode))

prop.table(table(fsrfistpwo_df$GenusSpecies,fsrfistpwo_df$BoroughCode),1)

prop.table(table(fsrfistpwo_df$GenusSpecies,fsrfistpwo_df$ComplaintType),2)

fsrfistpwodemo_df %>% filter(SpeciesCnt>1000) %>% ggplot(aes(x=ComplaintType,fill=GenusSpecies))+geom_bar()

# identify location of Species 

fsrfistpwo_df %>% filter(SpeciesCnt>1500)%>%ggplot(aes(x=Longitude.x,y=Latitude.x,col=factor(SpeciesCnt),fill=GenusSpecies))+geom_point()

# London Planetree has more requests and Count is 10071 and Norway Maple has 7500

# Explore Request Month created  on these species

fsrfistpwo_df %>% filter(SpeciesCnt>7500) %>% ggplot(aes(x=CMonth,fill=GenusSpecies))+geom_bar()

# Explore Proportion of Species on time scale

fsrfistpwo_df %>% filter(SpeciesCnt>7500) %>% ggplot(aes(x=CMonth,fill=GenusSpecies))+geom_bar(position = "fill")

# Explore BoroughCode for these species
fsrfistpwo_df %>% filter(SpeciesCnt>7500) %>% ggplot(aes(x=BoroughCode,fill=GenusSpecies))+geom_bar() 

# Queen has most, followed by Broklyn

# Queen  has most cases and Broklyn has second most,explore CommunityBoard

fsrfistpwo_df %>% filter(SpeciesCnt>7500 & ComplaintType!="New Tree Request" & BoroughCode=="Queens") %>% ggplot(aes(x=factor(CommunityBoard.x),fill=GenusSpecies))+geom_bar()
fsrfistpwo_df %>% filter(SpeciesCnt>7500 & ComplaintType!="New Tree Request" & BoroughCode=="Brooklyn") %>% ggplot(aes(x=factor(CommunityBoard.x),fill=GenusSpecies))+geom_bar()


# Queen Community Board 413,411,415 Norway Maple Contributing most , Brooklyn 318, 315, 311, 310 , 312 has most cases

fsrfistpwo_df %>% filter(SpeciesCnt>7500 & ComplaintType!="New Tree Request" & BoroughCode=="Queens") %>% ggplot(aes(x=factor(CommunityBoard.x),fill=GenusSpecies))+geom_bar()


# Explore Complaint Type on these tress

fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" ) %>% ggplot(aes(x=ComplaintType,fill=GenusSpecies))+geom_bar()  

fsrfistpwo_df %>% filter(SpeciesCnt>1500 & ComplaintType!="New Tree Request" ) %>% ggplot(aes(x=ComplaintType,fill=GenusSpecies))+geom_bar(position = "fill")  

# NorWay Maple has most cntribution in Damaged , Dead or Dying Tree and very less contribution in overgrown or Root or Sear Problem

# Thornless Honeylocust has more contribution in Illegal Tree Damage, Overgrown and SW complaints.

# London Plantree has almost Same contribution in all Complaints

# Pin Oak and Tilla Cordata has more contribution in overgrown and SW problems

# Damaged Tree has most cases more than 9000  

# Explore Diameter of these trees:

fsrfistpwo_df %>% filter(ComplaintType!="New Tree Request" & DBH<250) %>% ggplot(aes(x=DBH))+geom_histogram(binwidth = 1)

fsrfistpwo_df %>% filter(ComplaintType!="New Tree Request" & DBH<50) %>% ggplot(aes(x=DBH))+geom_histogram(binwidth = 1)

# Explore Proportion of Species

fsrfistpwo_df %>% filter(ComplaintType!="New Tree Request" & DBH<50 & SpeciesCnt>1500) %>% ggplot(aes(x=DBH,fill=GenusSpecies))+geom_histogram(binwidth = 1)

# Most of request for Norway Maple is for DBH<22 While London plantree most cases when DBH are more than 20

# Pin Oak has more cases at DBH more than 20 while Pyrus and Tilia as well as Callery Pear  has most cases at less than 20 DBH

# Thornless Honeylocust has more cases in 10-20 DBH

# Explore Density of DBH 

fsrfistpwo_df %>% filter(ComplaintType!="New Tree Request" & DBH<50 & SpeciesCnt>1500 & GenusSpecies!="Unknown - Unknown") %>% ggplot(aes(x=DBH,col=GenusSpecies))+geom_density()

# There is Normal Curve for each species at specific DBH









# Join Community Demo with SR 



community_demo_Complaintytype_df<-community_demo_Complaintytype_df %>% rename(Population=`Population 2010`)

community_demo_Complaintytype_df<-community_demo_Complaintytype_df %>% rename(Household=`HU 2010`)

community_demo_Complaintytype_df<- community_demo_Complaintytype_df %>% rename(Hgrowth=`10 Yr Growth`)

community_demo_Complaintytype_df<-community_demo_Complaintytype_df %>% rename(AvgResponseTime=`mean(ResponseTime, na.rm = TRUE)`)

community_demo_Complaintytype_df<-community_demo_Complaintytype_df %>% rename(MedResponseTime=`median(ResponseTime, na.rm = TRUE)`)

community_demo_Complaintytype_df<-community_demo_Complaintytype_df %>% select(CommunityBoard,Population,Household,Hgrowth,Shape_Area,Shape_Leng,`Damaged Tree`,`Dead Tree`,`Dead/Dying Tree`,`Illegal Tree Damage`,`Overgrown Tree/Branches`,`New Tree Request`,`Root/Sewer/Sidewalk Condition`,totalRequest,AvgResponseTime,MedResponseTime)

fsrfistpwodemo_df<-full_join(fsrfistpwo_df,community_demo_Complaintytype_df,by=c("CommunityBoard","CommunityBoard"))


# Explore HouseHold with Shape Area

fsrfistpwodemo_df %>% ggplot(aes(x=Shape_Area,y=Hgrowth,col=BoroughCode))+geom_point()

# Queen has more area but less growth
# Bronx has more growth with Less Area
# Manhattan has less growth and less area
# Broklyn has negative Correlation between Area and Growth


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

# Calculate and Add Inspection time

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(InspTime=myInUpdatedDate-myInCreatedDate)

fsrfistpwodemo_df$InspTime<- as.numeric(fsrfistpwodemo_df$InspTime)

fsrfistpwodemo_df %>% ggplot(aes(x=InspTime,fill=ComplaintType))+geom_histogram()

fsrfistpwodemo_df %>% ggplot(aes(x=InspTime,fill=InspectionType))+geom_histogram()

fsrfistpwodemo_df %>% filter(!is.na(WOType))%>%ggplot(aes(x=InspTime,fill=WOType))+geom_histogram()

fsrfistpwodemo_df %>% ggplot(aes(x=InspTime,fill=BoroughCode))+geom_histogram()

#Add TimeDiff between service request creation and inspection Closure

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(InCloseTime=myInUpdatedDate-myCreatedDate)

fsrfistpwodemo_df$InCloseTime<- as.numeric(fsrfistpwodemo_df$InCloseTime)

#Add TimeDiff between Inspection updated date and WO Creation

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(WoDelayTime=myWoCreatedDate-myInUpdatedDate)

fsrfistpwodemo_df$WoDelayTime <- as.numeric(fsrfistpwodemo_df$WoDelayTime)


# Calculate Total WO Completion time

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(WoCloseTime=myWoUpdatedDate-myWoCreatedDate)

fsrfistpwodemo_df$WoCloseTime <- as.numeric(fsrfistpwodemo_df$WoCloseTime)

fsrfistpwodemo_df %>% ggplot(aes(x=WoCloseTime,fill=ComplaintType))+geom_histogram()

fsrfistpwodemo_df %>% ggplot(aes(x=WoCloseTime,fill=InspectionType))+geom_histogram()

fsrfistpwodemo_df %>% ggplot(aes(x=WoCloseTime,fill=WOType))+geom_histogram()

fsrfistpwodemo_df %>% ggplot(aes(x=WoCloseTime,fill=BoroughCode))+geom_histogram()

fsrfistpwodemo_df %>% filter(SpeciesCnt>2000 & GenusSpecies!="Unknown - Unknown") %>%ggplot(aes(x=WoCloseTime,fill=GenusSpecies))+geom_histogram(binwidth = 20)

fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRlocCnt<7) %>%ggplot(aes(x=WoCloseTime,fill=factor(SRlocCnt)))+geom_histogram(position = "fill")

fsrfistpwodemo_df %>% filter(CntTPT>1) %>%ggplot(aes(x=WoCloseTime,fill=factor(CntTPT)))+geom_histogram()


#Add TimeDiff between Service Creation and WO Update

fsrfistpwodemo_df <- fsrfistpwodemo_df %>% mutate(SRWocloseTime=myWoUpdatedDate-myCreatedDate)

fsrfistpwodemo_df$SRWocloseTime <- as.numeric(fsrfistpwodemo_df$SRWocloseTime)

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


fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime))+geom_density()

fsrfistpwodemo_df %>% filter(CntTPT>2 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime))+geom_density()

fsrfistpwodemo_df %>% filter(SpeciesCnt>2 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime))+geom_density()

# species count is not affecting Density but Location and Tp cnt is affecting. It means Repeated calls on Same location or Tree point Increasing time for closure

#  Explore time of closure for Different location cnt

fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=factor(SRlocCnt)))+geom_density()

# location count more than 18 has more delays.

fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=BoroughCode))+geom_density()

fsrfistpwodemo_df %>% filter(SRlocCnt>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=ComplaintType))+geom_density()

# Multiple request on Same location and complaint type Root/Sewar/Sidewalk has more closure time

fsrfistpwodemo_df %>% filter(SRlocCnt>1 & InDelayTime>0) %>% ggplot(aes(x=SRWocloseTime,col=ComplaintType))+geom_density()

# Explore Frequent SR on same Tree point affecting Closure

fsrfistpwodemo_df %>% filter(CntTPT>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=ComplaintType))+geom_density()

fsrfistpwodemo_df %>% filter(CntTPT>1 & SRWocloseTime>0) %>% ggplot(aes(x=SRWocloseTime,col=SRCategory))+geom_density()

# Multiple request on Same TreePoint and complaint type Root/Sewar/Sidewalk, Dead Tree and OverGrown Tree Brnaches has more closure time

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


# % of request to total population for each community Board

community_demo_Complaintytype_df<-community_demo_Complaintytype_df %>% mutate(ReqperHH=totalRequest/Household)

community_demo_Complaintytype_df<-community_demo_Complaintytype_df %>% mutate(ReqperPP=totalRequest/Population)

community_demo_Complaintytype_df %>% select(CommunityBoard,ReqperHH,ReqperPP,MedResponseTime,AvgResponseTime) %>% ggplot(aes(x=ReqperHH,y=ReqperPP,col=factor(MedResponseTime)))+geom_point()

# There is positive correlation between Req per HH and Req per person. Higher Request rate has high median response time. 

#  Explore Median and Avg response time for Service request which are inspected and completed by WO 

TimeSummary_df<-fsrfistpwodemo_df %>% filter(!is.na(InspectionDate) & !is.na(WOType)) %>% group_by(CommunityBoard,ComplaintType,WOType,GenusSpecies,DBH) %>% summarize(mean(ResponseTime,na.rm=TRUE),median(ResponseTime,na.rm=TRUE),mean(InDelayTime,na.rm=TRUE),median(InDelayTime,na.rm=TRUE),mean(SRWocloseTime,na.rm=TRUE),median(SRWocloseTime,na.rm=TRUE))

TimeSummary_df<-TimeSummary_df %>% rename(MeanSRWOCLOSE=`mean(SRWocloseTime, na.rm = TRUE)`)

TimeSummary_df %>% filter(DBH<250)%>%ggplot(aes(x=DBH,y=MeanSRWOCLOSE,col=factor(CommunityBoard)))+geom_point()

TimeSummary_df %>% filter(DBH<25)%>%ggplot(aes(x=DBH,y=MeanSRWOCLOSE,col=factor(WOType)))+geom_jitter(alpha=0.5)

TimeSummary_df %>% filter(DBH<25)%>%ggplot(aes(x=DBH,y=MeanSRWOCLOSE,col=factor(WOType)))+geom_jitter(alpha=0.5)

TimeSummary_df %>% filter(DBH<25 & ComplaintType!="New Tree Request")%>%ggplot(aes(x=DBH,y=MeanSRWOCLOSE,col=factor(ComplaintType)))+geom_jitter(alpha=0.5)

# Complaint Type Root Sewar at high Diameter have high Avg closure time. While Dead tree at lower diameter has high avg close time.

TimeSummary_df %>% filter(DBH<25 & ComplaintType!="New Tree Request")%>%ggplot(aes(x=DBH,y=MeanSRWOCLOSE,col=factor(WOType)))+geom_jitter(alpha=0.5)

# WoType Tree Removal are more concentrated at low diameter. Prune is concentrated at high diamter and higher closure time.

TimeSummary_df %>% filter(DBH<25 & ComplaintType!="New Tree Request")%>%ggplot(aes(x=DBH,y=`median(SRWocloseTime, na.rm = TRUE)`,col=factor(WOType)))+geom_jitter(alpha=0.5)


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

# Doc Graphs

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType)) %>% ggplot(aes(x=BoroughCode,fill=ComplaintType))+geom_bar()

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType)) %>% ggplot(aes(x=BoroughCode,fill=ComplaintType))+geom_bar(position = "fill")+ylab("Proportion")

fsrfistpwodemo_df%>% filter(!is.na(InspectionStatus) & !is.na(SRType)) %>% ggplot(aes(x=BoroughCode,fill=SRType))+geom_bar()

# Service Req Creation Time

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType)) %>% ggplot(aes(x=CreatedMonth,fill=ComplaintType))+geom_histogram(stat="count")

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) ) %>% ggplot(aes(x=CreatedMonth,fill=ComplaintType))+geom_histogram(stat="count",binwidth = 10)

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) ) %>% ggplot(aes(x=CreatedMonth,fill=SRType))+geom_histogram(stat="count",binwidth = 10)

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 ) %>% ggplot(aes(x=ResponseTime,col=ComplaintType))+geom_density()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 ) %>% ggplot(aes(x=ResponseTime,col=SRType))+geom_density()

 # SR Resolution

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 ) %>% ggplot(aes(x=ResponseTime,col=SRResolution))+geom_density()

R_df<-fsrfistpwodemo_df %>% group_by(SRResolution) %>% summarise(ResolutionCnt=n()) %>% arrange ((ResolutionCnt)) %>% filter(ResolutionCnt>1500 & !is.na(SRResolution)) 

#Join with Parent Dataset

fsrfistpwodemo_df<-full_join(fsrfistpwodemo_df,R_df)


fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 & ResolutionCnt>0 & !is.na(ResolutionCnt)) %>% ggplot(aes(x=BoroughCode,fill=SRResolution))+geom_bar()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 & ResolutionCnt>0 & !is.na(ResolutionCnt)) %>% ggplot(aes(x=BoroughCode,y=ResponseTime))+geom_boxplot()+facet_wrap(~ComplaintType)

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 & ResolutionCnt>0 & !is.na(ResolutionCnt)) %>% ggplot(aes(x=BoroughCode,y=ResponseTime))+geom_boxplot()+facet_wrap(~SRResolution)

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="Dead Tree") %>% ggplot(aes(x=BoroughCode,y=ResponseTime))+geom_boxplot()+facet_wrap(~SRResolution)

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="Root/Sewer/Sidewalk Condition") %>% ggplot(aes(x=BoroughCode,y=ResponseTime))+geom_boxplot()+facet_wrap(~SRResolution)

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="Overgrown Tree/Branches") %>% ggplot(aes(x=BoroughCode,y=ResponseTime))+geom_boxplot()+facet_wrap(~SRResolution)

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>100 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="Damaged Tree") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=BoroughCode))+geom_point()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>200 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="Dead Tree") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=BoroughCode))+geom_point()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>400 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="Root/Sewer/Sidewalk Condition") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=BoroughCode))+geom_point()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>400 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="New Tree Request") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=BoroughCode))+geom_point()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 & ResolutionCnt>0 & !is.na(ResolutionCnt)) %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=SRType))+geom_point()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>100 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="Damaged Tree") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=SRType))+geom_point()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>100 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="Dead Tree") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=SRType))+geom_point()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>100 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="Root/Sewer/Sidewalk Condition") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=SRType))+geom_point()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>0 & ResolutionCnt>0 & !is.na(ResolutionCnt) & ComplaintType=="New Tree Request") %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=SRType))+geom_point()

fsrfistpwodemo_df %>% filter (!is.na(InspectionStatus) & !is.na(SRType) & !is.na(ComplaintType) & ResponseTime>150 & ResolutionCnt>0 & !is.na(ResolutionCnt)) %>% ggplot(aes(x=Longitude.x,y=Latitude.x,col=SRType))+geom_point()+facet_wrap(~ComplaintType)

ftp_df %>% group_by(GenusSpecies) %>% summarise(cnt=n()) %>% arrange(desc(cnt))
