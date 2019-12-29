library(readxl)
library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)
library(openxlsx)

#import gift data 
#noted one member with no memberid in excel, updated excel to be NA-001 
gifts <- read_excel("MSU_FALL_2019/I5 Dataset Files/R Excel Files/1 - MSU Grad Member Donors Report 9.20.19.xlsx")

names(gifts)[1] <- "memberid"
names(gifts)[5] <- "gift_amount"

#calculate total amount of donations given by member over their membership
total <- gifts %>% 
  group_by(memberid) %>% 
  summarise(total_lifetime_donations= sum(gift_amount)) %>%
  ungroup()

#calculate the number of donations given by member over their membership
count <- gifts %>% 
  group_by(memberid) %>% 
  summarise(number_of_donations = n()) %>%
  ungroup()

#merge total amount and number of donations
gifts2 <- merge(total, count, by.x="memberid", by.y="memberid")

#import member location
#reviewed location and zip code in excel prior to importing to ensure 
#1. proper spelling of city and state
#2. zip code format
#updated a couple zip codes to be 5 numbers only and updated a few city mispellings as well 
location<- read_excel("MSU_FALL_2019/I5 Dataset Files/R Excel Files/1 - MSU Grad City-State- Zip Code Report 9.20.19.xlsx")

#update column names for simplicity 
names(location)[1] <- "memberid"
names(location)[2] <- "city"
names(location)[3] <- "state"
names(location)[4] <- "zip"

#what are the number of distict members
df<-select(location,memberid)
dfnums <-distinct(df,memberid)
#noted there are 6 NA memberids for some reason
dupmemebers <- df %>% group_by(memberid) %>% filter(n() > 1)
#5322 members and 6 NA ids

#want to add county for tableau purposes later 
#use csv of county data found via google
county<-read_csv("MSU_FALL_2019/I5 Dataset Files/R Excel Files/zip_code_county_database.csv")
county2<-select(county,zip,county, latitude, longitude)

locationcount <- merge(location, county2, by.x="zip", by.y="zip", all.x = TRUE)


#write to text to evaluate in excel 
#to ensure the leading zeros are kept for some member ids when writing to excel
locationcount$memberid <- paste0(locationcount$memberid,"\t")
write.xlsx(locationcount, 'location.xlsx')

#in excel found 
#noted 10 with zip, city, state without location information - manually updated by hand in spreadsheet
#noted one international location - added new column for country
#noted 6 member in file without member id & location information
#updated the 6 NA members to have unique NA ID continue numbering starting with NA-002 to NA-007
#deleted one row in excel that was all NAs

#reimport updated excel - did a save as and import as .xlsm to hold the leading zeros in member ids
locationcountedits <- read_excel('MSU_FALL_2019/I5 Dataset Files/R Excel Files/1 - MSU Grad City-State- Zip Code Report 9.20.19 - UPDATE2.xlsm')
#not a viable to update with location information by hand/in excel outside of R - need to evaluate other methods.
#maybe new database will be better with location?

#join member information with gift information 
giftslocal <- merge(x= locationcountedits, y= gifts2, by ="memberid", all = TRUE)
giftslocal$total_lifetime_donations[is.na(giftslocal$total_lifetime_donations)] <- 0
giftslocal$number_of_donations[is.na(giftslocal$number_of_donations)] <- 0

#import scholarships
scholar<- read_excel("MSU_FALL_2019/I5 Dataset Files/R Excel Files/1 - MSU Grad Members who have a scholarship membership 9.20.19.xlsx")
names(scholar)[1] <- "memberid"

scholarcount <-scholar %>% 
  group_by(memberid) %>% 
  summarise(count = n())

scholarcount<-transform(scholarcount, scholarship_member= ifelse(count >= 1, 1, 0))
scholarcount <- scholarcount[-2]

#merge scholarship information with the member info and gift info
gls <- merge(x= giftslocal, y=scholarcount, by ="memberid", all.x = TRUE)
#make all members who do not have a scholarship membership be 0
gls$scholarship_member[is.na(gls$scholarship_member)] <- 0

#import participation in events 
#note two members without ids in excel - changed to NA-008 and NA-009 in excel to continue numbering 
party<- read_excel("MSU_FALL_2019/I5 Dataset Files/R Excel Files/2 - MSU Grad Program Participation Report 9.23.2019.xlsx")
party<- party[-2:-10]
names(party)[1] <- "memberid"
names(party)[2] <- "program"
names(party)[3] <- "year"

library(fastDummies)

#make the year variable a dummy 
party2<-dummy_cols(party, select_columns =c("year"))
#delete random birthday parties
party2<- party2[-11:-12]
#make the programs a dummy variable 
party3<-dummy_cols(party2, select_columns =c("program"))

#there are duplicationso of member ids
#however some member ids are duplicated but have different combination of participation
#thefore, keep all distinct member and distinct combination of participation
test<-party3 %>% distinct(memberid,program, year, .keep_all = TRUE)
test<- test[-2:-3]

names(test)[5] <- "sev_eight_part"
names(test)[6] <- "sev_nine_part"
names(test)[7] <- "eight_nine_part"
names(test)[8] <- "all_seg_part"

#break out "sev_eight_part" into year_18 and year_17 and make a 1 in each column
test2<-test %>%
  mutate(year_18 = ifelse(sev_eight_part==1, 1, year_18),
         year_17 = ifelse(sev_eight_part==1, 1, year_17)
         )

#break out "sev_nine_part" into year_17 and year_19 and make a 1 in each column
test3<-test2 %>%
  mutate(year_17 = ifelse(sev_nine_part==1, 1, year_17),
         year_19 = ifelse(sev_nine_part==1, 1, year_19)
  )

#break out "eight_nine_part" into year_18 and year_18 and make a 1 in each column
test4<-test3 %>%
  mutate(year_18 = ifelse(eight_nine_part==1, 1, year_18),
         year_19 = ifelse(eight_nine_part==1, 1, year_19)
  )

#break out "all_seg_part" into year_17, year_18, and year_19 and make a 1 in each column
test5<-test4 %>%
  mutate(year_17= ifelse(all_seg_part==1, 1, year_17),
         year_18 = ifelse(all_seg_part==1, 1, year_18),
         year_19 = ifelse(all_seg_part==1, 1, year_19)
  )

party<-test5

#must merge duplicate memberid rows so participation in events is combined
df <- party%>%group_by(memberid)%>% mutate(total = n())
#subsets those without duplicates
ddc<-df[df$total==1,]
#subsets those with duplicates
ddd<-df[df$total==2,]
#the number of duplicates should halve - combine rows of duplicates and keep the max number between the duplicates for each column
ddd<- ddd %>% group_by(memberid) %>% summarise_all(funs(max(as.character(.))))
#combine the deleted duplicates dataframe back with nonduplicates
df2<-merge(ddc, ddd, all=TRUE)
#delete total column
df2<-df2[-13]

party<-df2

#rename columns
names(party)[9] <- "birthday_party"           
names(party)[10] <- "labs"      
names(party)[11] <- "preschool_prog"
names(party)[12] <- "homeschool_prog"

#delete the columns that were broken out ("sev_eight_part", "sev_nine_part", "eight_nine_part", "all_seg_part")
party<- party[-5:-8]

#rename participation year columns 
names(party)[2] <- "participate_2019"  
names(party)[3] <- "participate_2018"  
names(party)[4] <- "participate_2017"  

#merge participation, scholarship, gift, and membership information 
glsp <- merge(x= gls, y=party, by ="memberid", all.x = TRUE)

#import children -
kids<- read_excel("MSU_FALL_2019/I5 Dataset Files/R Excel Files/New_Ages.xlsx")

#there are many duplicates for each memberid
#keep the ages for each member id from the latest expiration date 
count <- kids%>% 
  group_by(memberid) %>%
  mutate(Date=as.Date(expires_on, format= "%m/%d/%y"))%>% 
  filter(Date==max(Date))

#delete the date column
count<-count[-4]

#break children into 5 age groups and create a new column for each age group
#young 0-4
#elementary 5-10
#middle 11-13
#high school 14-18
#adult 18+
count$age_group1 <- ifelse(count$age >=-1 & count$age <=4,1,0)
count$age_group2 <- ifelse(count$age >=5 & count$age <=10,1,0)
count$age_group3 <- ifelse(count$age >=11 & count$age <=13,1,0)
count$age_group4 <- ifelse(count$age >=14 & count$age <=18,1,0)
count$age_group5 <- ifelse(count$age >=19,1,0)

#must merge the duplicate memberid rows now 
#count the total number of instances for each member id 
count_edit <- count%>%group_by(memberid)%>% mutate(total = n())

#subsets the member ids without duplicates
nondup<-count_edit[count_edit$total==1,]
#delete exipires_on and age columns
nondup<-nondup[-2:-3]
nondup<-nondup[-7]

#subsets those with duplicates
dups<-count_edit[count_edit$total>=2,]
#delete exipires_on and age columns
dups<-dups[-2:-3]
dups<-dups[-7]
dups<- dups %>% group_by(memberid) %>% summarise_all(sum)

#combine the deleted duplicates dataframe back with nonduplicates 
ages<-merge(nondup, dups, all=TRUE)

#confirm the correct member ids that should be in ages above - 4913. 
agetest<- kids %>% distinct(memberid)

#create another column that gives the total children for each member
kidscount<-count %>% 
  group_by(memberid) %>% 
  summarise(total_kids = n())

#add column to the other children information
kidstotal<-merge(ages,kidscount, all=TRUE)
#noted 1 member without member id 
missingkid <- kidscount %>% filter(is.na(memberid))
#not much information can be preserved - conclude to delete
kidstotal <- kidstotal%>% filter(!is.na(memberid))

#merge with all other previous information
glsp_age <- merge(x= glsp, y=kidstotal, by ="memberid", all.x = TRUE, all.y=TRUE)

#using Chris's intial code to create the additonal columns and work with data from the latest renewal report 
dat2 <- read_csv("DATA\\newest_renewal_data.csv", guess_max = 100000) %>%
  rename(member_id = `Constituent ID`,
         joined_date = `Current Joined On Date`,
         renewal_date = `History All Renewed On Dates`,
         curr_expiration_date = `Current Expires On`,
         curr_member_category = `Current Category`,
         member_category = `History Membership Category`,
         expiration_date = `History Expires On`) %>%
  mutate(renewal_date = as.Date(renewal_date, format = "%m/%d/%Y"),
         curr_expiration_date = as.Date(curr_expiration_date, format = "%m/%d/%Y"),
         expiration_date = as.Date(expiration_date, format = "%m/%d/%Y"),
         member_category = as.factor(if_else(is.na(`Gift Reference`), member_category, "CEAP")),
         # member_category = as.factor(member_category),
         joined_date = as.Date(joined_date, format = "%m/%d/%Y")) %>%
  select(member_id, joined_date, renewal_date, curr_expiration_date, expiration_date, member_category) %>%
  arrange(member_id, expiration_date) %>%
  distinct() # the query change that added the gift recipient column led to some duplicates in the report with the history columns, goes away once you select distinct on just the columns we need

cutoff_date <- floor_date(Sys.Date() - months(0), "month") - 1

renewals2<-dat2 %>%
  group_by(member_id) %>%
  mutate(next_renewal = lead(renewal_date, order_by=member_id),
         future_expir = ifelse(expiration_date > Sys.Date(),1,0),
         member_is_numeric = ifelse(!grepl("\\D", member_id),1,0),
         CountInst = n(), 
         membership_year = row_number(),
         renewal_timing = as.numeric(difftime(next_renewal, expiration_date , units = "days")),
         renewed = if_else(is.na(renewal_timing) | renewal_timing > 183, 0, 1),
         member_category2 = as.factor((if_else(expiration_date >= "2018-10-31",
                                               paste0(member_category, " - 2018"),
                                               as.character(member_category)))))

#noted 8 members without member ids 
missings <- renewals2 %>% filter(is.na(member_id))
#make the members with no member id NA-010 - NA-018 to continue numbering and keep track of NA member id
#assigned member id based on joined date
missings$member_id[missings$joined_date == '2017-01-03'] <- "NA-010"
missings$member_id[missings$joined_date == '2017-02-08'] <- "NA-011"
missings$member_id[missings$joined_date == '2018-03-03'] <- "NA-012"
missings$member_id[missings$joined_date == '2018-10-16'] <- "NA-013"
missings$member_id[missings$joined_date == '2018-11-20'] <- "NA-014"
missings$member_id[missings$joined_date == '2019-03-17'] <- "NA-015"
missings$member_id[missings$joined_date == '2019-03-28'] <- "NA-016"
missings$member_id[missings$joined_date == '2019-05-25'] <- "NA-017"
#rejoin with the rest of the data
nonmissings <- renewals2 %>% filter(!is.na(member_id))
renewals22 <- rbind(nonmissings,missings)

#test to see the number of distinct member ids for data quality checks
df<-select(renewals22,member_id)
distinctnums<-distinct(df)
#5244 unique member ids are found and should be preserved.

memyear<- select(renewals22, member_id, joined_date, membership_year) %>% group_by(member_id) %>% top_n(1, membership_year)
#finds 5244 members with their total memebership years.

memcatg<- select(renewals22, member_id,expiration_date,member_category) %>%filter(expiration_date == max(expiration_date))
#noted there are 3 duplicated members for some reason?
dupmemebers <- memcatg %>% group_by(member_id) %>% filter(n() > 1)
#for now, delete the "duplicates"
memcatg2<-subset(memcatg, !duplicated(member_id))

#merge membership year with membership categories 
memberyearcat <- merge(x=memcatg2, y=memyear, by ="member_id", all.x = TRUE, all.y = TRUE)

glsp_age <- rename(glsp_age, member_id = memberid)

#merge together with the data from the previous information from glsp_age
all <- merge(x=glsp_age, y=memberyearcat, by ="member_id", all.x = TRUE, all.y = TRUE)
# write.xlsx(all, 'location.xlsx')

#calculate the number of times renewed - with a known value as early or late
countrenewals<-select(renewals22,member_id, renewal_timing) %>% group_by(member_id) %>%
  summarise(known_time_renewed = sum(!is.na(renewal_timing)))

#calculate the number of times renewed - without known if the renewal was early or late
#must subtract 1 from every row to disgregard the NA for the current year membership
countnas<-select(renewals22,member_id, renewal_timing) %>% group_by(member_id) %>%
  summarise(unknown_renewal_time = sum(is.na(renewal_timing))-1)

#merge with rest of data
all2 <- merge(x=countnas, y=countrenewals, by ="member_id", all.x = TRUE, all.y = TRUE)

#create a new column that calculates total times renewed 
all2$total_renewed_times <- all2$known_time_renewed + all2$unknown_renewal_time

#merge with the rest of the data
all3 <- merge(x=all, y=all2, by ="member_id", all.x = TRUE, all.y = TRUE)

#calculate the portion of known renewal time that is on time vs. late
countontime<-select(renewals22,member_id, renewal_timing)%>% group_by(member_id) %>%
  summarise(times_ontime = sum(renewal_timing <=0, na.rm = TRUE))

countonlate<-select(renewals22,member_id, renewal_timing)%>% group_by(member_id) %>%
  summarise(times_late = sum(renewal_timing >0, na.rm = TRUE))

#merge late and ontime together
latetime<- merge(x=countontime, y=countonlate, by ="member_id", all.x = TRUE, all.y = TRUE)

#merge with other data
all4 <- merge(x=all3, y=latetime, by ="member_id", all.x = TRUE, all.y = TRUE)

#add columns that covert the times late and times ontime to percentages
all4$percent_ontime<-all4$times_ontime/(all4$times_ontime+all4$times_late)
all4$percent_late<-all4$times_late/(all4$times_ontime+all4$times_late)
all4$percent_ontime[is.na(all4$percent_ontime)] <- 0
all4$percent_late[is.na(all4$percent_late)] <- 0

#fill NAs with 0's where necessary - assume NAs to be 0
all4$total_lifetime_donations[is.na(all4$total_lifetime_donations)] <- 0
all4$number_of_donations[is.na(all4$number_of_donations)] <- 0
all4$participate_2019[is.na(all4$participate_2019)] <- 0
all4$participate_2018[is.na(all4$participate_2018)] <- 0
all4$participate_2017[is.na(all4$participate_2017)] <- 0
all4$birthday_party[is.na(all4$birthday_party)] <- 0
all4$labs[is.na(all4$labs)] <- 0
all4$preschool_prog[is.na(all4$preschool_prog)] <- 0
all4$homeschool_prog[is.na(all4$homeschool_prog)] <- 0
all4$age_group1[is.na(all4$age_group1)] <- 0
all4$age_group2[is.na(all4$age_group2)] <- 0
all4$age_group3[is.na(all4$age_group3)] <- 0
all4$age_group4[is.na(all4$age_group4)] <- 0
all4$age_group5[is.na(all4$age_group5)] <- 0
all4$total_kids[is.na(all4$total_kids)] <- 0

#write the final data table to an excel file
#keep the member_id leading 0's
all4$member_id <- paste0(all4$member_id,"\t")
# write.csv(all4, file = "I5_dataset_created.csv",row.names=FALSE)
