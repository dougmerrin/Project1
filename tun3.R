#Volunteer Program (for these questions, you will need to use the Salesforce data)

# Q: Does working with a volunteer raise the probability of a client getting hired or getting hired sooner?
# Are there certain volunteer activities that are more effective than others?
# Do volunteer activities increase the quality job obtained – perhaps determined through salary level?
# (Essentially, can we prove quantitatively that our volunteers are increasing the overall effectiveness of our program?)

# Q: Is there a specific demographic profile of clients that use volunteer services (gender, location, service branch, level of education)?
# Is there a specific demographic profile of clients that request volunteer services and then miss or cancel (“Missed/Cancelled” field in Request data)?


# Q: Are there specific offices that have a higher probability of clients missing or cancelling requests?

# Data Context: In order to come to the answer, below is the data that will need to be
# pulled. Using the information, you should be able to figure out if the request [volunteer] activity occurred before
# the reported hire date. If yes, we can assume the volunteer activity had some level of input (even mock interviews occurring
# very close to the hire date had impact many times, because it was in preparation for the real interview). You will need to compare
# Contact records for clients that did go through a request [volunteer activity] and those that did not. ]



library(ggvis)
library(dplyr)
library(ggplot2)
#data
setwd("/Users/dougmerrin/Dropbox/TUN data challenge/Salesforce Data")
user.role = read.csv("UserRole.csv")
user = read.csv("User.csv")
task = read.csv("Task.csv")
record.type = read.csv("RecordType.csv")
Contact = read.csv("Contact.csv")
case = read.csv("Case.csv")
account = read.csv("Account.csv")
W_Dates = read.csv("All Hires with Contact Dates.csv")
wdf = read.csv("Working Data Frame v2.csv")

#code from alex to get salary range
range=as.character(Contact$Salary_Range__c)
lowerbound=0
for(i in 1:64549){
  lowerbound[i]=strsplit(range[i],"-")[[1]][1]
}
lowerbound=sub(",","",lowerbound)
lowerbound=sub("100000\\+","95000",lowerbound)
lowerbound=sub("\\$","",lowerbound)
lowerbound=sub(" ","",lowerbound)
lowerbound=sub("Lessthan 20000","15000",lowerbound)
salary=as.numeric(lowerbound)+5000
Contact$salary_clean = salary

#YRS_ED_IND_A constructed according to:
# If high school grad, 1, if some college, 2, if associates, 3, if bachelors, 4, if masters or more, 5, 0 if not contained in this range (only 2 obs)

#education level things to work with
C_Yrs_ed = Contact[which(Contact$Years_of_Education__c != ""),]
C_Yrs_ed$YRS_ED = as.character(C_Yrs_ed$Years_of_Education__c)
C_Yrs_ed$YRS_ED_IND_A = ifelse(C_Yrs_ed$YRS_ED == "12", 1, ifelse(C_Yrs_ed$YRS_ED == "13",2,ifelse(C_Yrs_ed$YRS_ED == "14",3,ifelse(C_Yrs_ed$YRS_ED == "15",3,
                              ifelse(C_Yrs_ed$YRS_ED == "16",4,ifelse(C_Yrs_ed$YRS_ED == "17",5,ifelse(C_Yrs_ed$YRS_ED == "18",5,
                              ifelse(C_Yrs_ed$YRS_ED == "19",5,ifelse(C_Yrs_ed$YRS_ED == "20",5,0)))))))))
#length(which(C_Yrs_ed$YRS_ED_IND_A == 0))                              

C_edu_summary = Contact[which(Contact$Education_Summary__c != ""),]
C_cont_edu = Contact[which(Contact$Continuing_Education__c != ""),]


#Education summary constructed according to:
# if high school:2,
# if 2 year college: 3
# if technical school: 4
# if 4 year college: 5
# if graduate school: 6  
# if doctorate: 7

C_Highest_edu = Contact[which(Contact$Highest_Level_of_Education_Completed__c != ""),]
d = C_Highest_edu
d$level.educ = as.character(C_Highest_edu$Highest_Level_of_Education_Comp)

#COLLEGE
d$el = ifelse(grepl("Bachelor", d$level.educ), 5, d$level.educ)
d$el = ifelse(grepl("college", d$level.educ), 5, d$el)
d$el = ifelse(grepl("College", d$level.educ), 5, d$el)
d$el = ifelse(grepl("BA", d$level.educ), 5, d$el)
d$el = ifelse(grepl("BS", d$level.educ), 5, d$el)
d$el = ifelse(grepl("Bachelor's", d$level.educ), 5, d$el)
d$el = ifelse(grepl("4 year Degree", d$level.educ), 5, d$el)
d$el = ifelse(grepl("4 year college", d$level.educ), 5, d$el)
d$el = ifelse(grepl("4", d$level.educ), 5, d$el)

#2 YEAR COLLEGE
d$el = ifelse(grepl("2 Year Degree", d$level.educ), 3, d$el)
d$el = ifelse(grepl("2 year college", d$level.educ), 3, d$el)
d$el = ifelse(grepl("2", d$level.educ), 3, d$el)

#GRADUATE
d$el = ifelse(grepl("Graduate Degree", d$level.educ), 6, d$el)
d$el = ifelse(grepl("Master", d$level.educ), 6, d$el)
d$el = ifelse(grepl("Masters", d$level.educ), 6, d$el)

#HIGHSCHOOL
d$el = ifelse(grepl("High School/GED", d$level.educ), 2, d$el)
d$el = ifelse(grepl("High School", d$level.educ), 2, d$el)
d$el = ifelse(grepl("HS", d$level.educ), 2, d$el)


#TECHNICAL SCHOOL/AS/Associates?
d$el = ifelse(grepl("Technical Certificate", d$level.educ), 4, d$el)
d$el = ifelse(grepl("AS", d$level.educ), 4, d$el)
d$el = ifelse(grepl("Associates Degree", d$level.educ), 4, d$el)
d$el = ifelse(grepl("Associate's", d$level.educ), 4, d$el)


#DOCTORATE
d$el = ifelse(grepl("Doctorate" , d$level.educ), 7, d$el)


#aggregating zip code, salary, and edu
new_for_map = d[which(d$el == "1"|d$el == "2"|d$el == "3"|d$el == "4"|d$el == "5"|d$el == "6"|d$el == "7"),]
new_for_map = cbind.data.frame(new_for_map$el, new_for_map$MailingPostalCode, new_for_map$salary_clean)
new_for_map = new_for_map[which(new_for_map$`new_for_map$MailingPostalCode` != ""),]
#adding salary to zip code and education level
new_for_map = na.omit(new_for_map)
#30726 observations retained --- snortsky
colnames(new_for_map) = c("Education Level", "Postal Code", "Salary Level")
write.csv(new_for_map, "map_with_edu_and_salary.csv")




#gender indicator
C_gender = Contact[which(as.character(Contact$a__Gender__c) != ""),]


#Military branch indicator
C_s_branch = Contact[which(as.character(Contact$Service_Branch__c) != ""),]


#Air Force = 1
C_s_branch$Service_Branch__c = as.character(C_s_branch$Service_Branch__c)
C_s_branch$branch = ifelse(grepl("AF", C_s_branch$Service_Branch__c), 1, C_s_branch$Service_Branch__c)
C_s_branch$branch = ifelse(grepl("Air Force", C_s_branch$Service_Branch__c), 1, C_s_branch$branch)
C_s_branch$branch = ifelse(grepl("USAF", C_s_branch$Service_Branch__c), 1, C_s_branch$branch)


#Air National Guard = 2
C_s_branch$branch = ifelse(grepl("Air National Guard", C_s_branch$Service_Branch__c), 2, C_s_branch$branch)
C_s_branch$branch = ifelse(grepl("ANG", C_s_branch$Service_Branch__c), 2, C_s_branch$branch)

#Army = 3 
C_s_branch$branch = ifelse(grepl("Army", C_s_branch$Service_Branch__c), 3, C_s_branch$branch)


#Coast Guard = 4
C_s_branch$branch = ifelse(grepl("Coast Guard", C_s_branch$Service_Branch__c), 4, C_s_branch$branch)

#Marines = 5
C_s_branch$branch = ifelse(grepl("Marine", C_s_branch$Service_Branch__c), 5, C_s_branch$branch)
C_s_branch$branch = ifelse(grepl("USMC", C_s_branch$Service_Branch__c), 5, C_s_branch$branch)

#Navy = 6
C_s_branch$branch = ifelse(grepl("Navy", C_s_branch$Service_Branch__c), 6, C_s_branch$branch)

#National Guard = 7
C_s_branch$branch = ifelse(grepl("National Guard", C_s_branch$Service_Branch__c), 7, C_s_branch$branch)

#Not applicable = 8
C_s_branch$branch = ifelse(grepl("Not Applicable", C_s_branch$Service_Branch__c), 8, C_s_branch$branch)

#Spouse = 9 
C_s_branch$branch = ifelse(grepl("Spouse", C_s_branch$Service_Branch__c), 9, C_s_branch$branch)

C_s_branch = C_s_branch[which(C_s_branch$branch == "1"|C_s_branch$branch == "2"|C_s_branch$branch == "3"|
                                       C_s_branch$branch == "4"|C_s_branch$branch == "5"|C_s_branch$branch == "6"|
                                       C_s_branch$branch == "7"|C_s_branch$branch == "8"|C_s_branch$branch == "9"),]


#service rank indicator construction
C_s_rank = Contact[which(as.character(Contact$Service_Rank__c) != ""),]
levels(C_s_rank$Service_Rank__c)
C_s_rank$Service_Rank__c[1:1000]
C_s_rank$Service_Rank__c = as.character(C_s_rank$Service_Rank__c)

#Enlisted ranks (range from 0-9)
#E 1
C_s_rank$rank= ifelse(grepl("E-1", C_s_rank$Service_Rank__c), 1, C_s_rank$Service_Rank__c)
C_s_rank$rank = ifelse(grepl("E1", C_s_rank$Service_Rank__c), 1, C_s_rank$rank)
#E 2
C_s_rank$rank = ifelse(grepl("E-2", C_s_rank$Service_Rank__c), 2, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("E2", C_s_rank$Service_Rank__c), 2, C_s_rank$rank)
#Etc.
C_s_rank$rank = ifelse(grepl("E-3", C_s_rank$Service_Rank__c), 3, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("E3", C_s_rank$Service_Rank__c), 3, C_s_rank$rank)

C_s_rank$rank = ifelse(grepl("E-4", C_s_rank$Service_Rank__c), 4, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("E4", C_s_rank$Service_Rank__c), 4, C_s_rank$rank)

C_s_rank$rank = ifelse(grepl("E-5", C_s_rank$Service_Rank__c), 5, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("E5", C_s_rank$Service_Rank__c), 5, C_s_rank$rank)

C_s_rank$rank = ifelse(grepl("E-6", C_s_rank$Service_Rank__c), 6, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("E6", C_s_rank$Service_Rank__c), 6, C_s_rank$rank)

C_s_rank$rank = ifelse(grepl("E-7", C_s_rank$Service_Rank__c), 7, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("E7", C_s_rank$Service_Rank__c), 7, C_s_rank$rank)

C_s_rank$rank = ifelse(grepl("E-8", C_s_rank$Service_Rank__c), 8, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("E8", C_s_rank$Service_Rank__c), 8, C_s_rank$rank)

C_s_rank$rank = ifelse(grepl("E-9", C_s_rank$Service_Rank__c), 9, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("E9", C_s_rank$Service_Rank__c), 9, C_s_rank$rank)

#Warrant Officer ranks (range from 11-15)
#Warrant Officers are outranked by all enlisted officers, although they may have more experience
# Warrants are found in technical specialties including aviation (flying warrants), communications security,
# medical, or special operations to list a few. The President commissions warrants for their technical specialty,
# not necessarily for their leadership role.


C_s_rank$rank = ifelse(grepl("W-1", C_s_rank$Service_Rank__c), 11, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("W-2", C_s_rank$Service_Rank__c), 12, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("W-3", C_s_rank$Service_Rank__c), 13, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("W-4", C_s_rank$Service_Rank__c), 14, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("W-5", C_s_rank$Service_Rank__c), 15, C_s_rank$rank)

C_s_rank$rank = ifelse(grepl("W1", C_s_rank$Service_Rank__c), 11, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("W2", C_s_rank$Service_Rank__c), 12, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("W3", C_s_rank$Service_Rank__c), 13, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("W4", C_s_rank$Service_Rank__c), 14, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("W5", C_s_rank$Service_Rank__c), 15, C_s_rank$rank)

C_s_rank$rank = ifelse(grepl("CW1", C_s_rank$Service_Rank__c), 11, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("CW2", C_s_rank$Service_Rank__c), 12, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("CW3", C_s_rank$Service_Rank__c), 13, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("CW4", C_s_rank$Service_Rank__c), 14, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("CW5", C_s_rank$Service_Rank__c), 15, C_s_rank$rank)

C_s_rank$rank = ifelse(grepl("CW-1", C_s_rank$Service_Rank__c), 11, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("CW-2", C_s_rank$Service_Rank__c), 12, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("CW-3", C_s_rank$Service_Rank__c), 13, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("CW-4", C_s_rank$Service_Rank__c), 14, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("CW-5", C_s_rank$Service_Rank__c), 15, C_s_rank$rank)


#Enlisted Officer ranks (range from 21-30)
C_s_rank$rank = ifelse(grepl("O-1", C_s_rank$Service_Rank__c), 21, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O1", C_s_rank$Service_Rank__c), 21, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O-2", C_s_rank$Service_Rank__c), 22, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O2", C_s_rank$Service_Rank__c), 22, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O-3", C_s_rank$Service_Rank__c), 23, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O3", C_s_rank$Service_Rank__c), 23, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O-4", C_s_rank$Service_Rank__c), 24, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O4", C_s_rank$Service_Rank__c), 24, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O-5", C_s_rank$Service_Rank__c), 25, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O5", C_s_rank$Service_Rank__c), 25, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O-6", C_s_rank$Service_Rank__c), 26, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O6", C_s_rank$Service_Rank__c), 26, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O-7", C_s_rank$Service_Rank__c), 27, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O7", C_s_rank$Service_Rank__c), 27, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O-8", C_s_rank$Service_Rank__c), 28, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O8", C_s_rank$Service_Rank__c), 28, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O-9", C_s_rank$Service_Rank__c), 29, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O9", C_s_rank$Service_Rank__c), 29, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O-10", C_s_rank$Service_Rank__c), 30, C_s_rank$rank)
C_s_rank$rank = ifelse(grepl("O10", C_s_rank$Service_Rank__c), 30, C_s_rank$rank)

class(C_s_rank$rank)

C_s_rank = C_s_rank[which(C_s_rank$rank == "1"|C_s_rank$rank == "2"|C_s_rank$rank == "3"|C_s_rank$rank == "4"|C_s_rank$rank == "5"|
                            C_s_rank$rank == "6"|C_s_rank$rank == "7"|C_s_rank$rank == "8"|C_s_rank$rank == "9"|C_s_rank$rank == "11"|
                            C_s_rank$rank == "12"|C_s_rank$rank == "13"|C_s_rank$rank == "14"|C_s_rank$rank == "15"|C_s_rank$rank == "21"|
                            C_s_rank$rank == "22"|C_s_rank$rank == "23"|C_s_rank$rank == "24"|C_s_rank$rank == "25"|C_s_rank$rank == "26"|
                            C_s_rank$rank == "27"|C_s_rank$rank == "28"|C_s_rank$rank == "29"|C_s_rank$rank == "30"),]

#Clean up case missed or cancelled column

case$Has_the_client_missed_this_opportunity__c = as.character(case$Has_the_client_missed_this_opportunity__c)
case_missed = case[which(case$Has_the_client_missed_this_opportunity__c != ""),]
case_missed$Has_the_client_missed_this_opportunity__c = as.factor(case_missed$Has_the_client_missed_this_opportunity__c)


test =  case[which(case$Has_the_client_missed_this_opportunity__c != ""),]
library(plyr)
# Q: Is there a specific demographic profile of clients that use volunteer services
# (gender, location, service branch, level of education)?


test =  case[which(case$Has_the_client_missed_this_opportunity__c != ""),c(4,38)]
colnames(test) = c("id","missed")

test2 = C_s_rank[,c(1,655)]
colnames(test2) = c("id","Rank_In_Military")

test3 = C_s_branch[,c(1,655)]
colnames(test3) = c("id","Branch_In_Military")

test4 = d[,c(1,654,656)]
colnames(test4) = c("id","Salary_Level","Education_Level")



skippers = join(test, test2, by = "id", type = "full")
skippers = join(skippers, test3, by = "id", type = "full")
skippers = join(skippers, test4, by = "id", type = "full")

unique(skippers$missed)
skippers$M= ifelse(grepl("1 Time Cancelled", skippers$missed), "1", skippers$missed)
skippers$M = ifelse(grepl("1 Time Missed", skippers$missed), "1", skippers$M)
skippers$M = ifelse(grepl("2 Times Cancelled", skippers$missed), "2", skippers$M)
skippers$M = ifelse(grepl("2 Times Missed", skippers$missed), "2", skippers$M)
skippers$M = ifelse(grepl("3 Times Missed", skippers$missed), "3", skippers$M)
skippers$M = ifelse(grepl("3 Times Cancelled", skippers$missed), "3", skippers$M)
skippers$M = ifelse(grepl("No", skippers$missed), "", skippers$M)



skippers$rank_numeric = as.numeric(skippers$Rank_In_Military)
skippers$branch_numeric = as.numeric(skippers$Branch_In_Military)
  
  
skippers$Education_Level = as.numeric(skippers$Education_Level)
aggregate(Education_Level ~ missed , data = skippers, FUN = mean)
aggregate(Education_Level ~ rank_numeric, data = skippers, FUN = mean)
aggregate(Education_Level ~ branch_numeric, data = skippers, FUN = mean)
aggregate(Salary_Level ~ branch_numeric, data = skippers, FUN = mean)


new_data = merge(skippers, wdf, by.x = "id", by.y = "Id", all.y = T)
write.csv(new_data, "merged_new_data.csv")

# ""                   "$10/hr - $14.50/hr" "$100,000+"          "$15/hr - $19.50/hr" "$20,000 - $29,999"  "$20/hr - $24.50/h" 
# [7] "$20/hr - $24.50/hr" "$25/hr - $29.50/hr" "$30,000 - $39,999"  "$30/hr - $34.50/hr" "$35/hr - $39.50/hr" "$40,000 - $49,999" 
# [13] "$40/hr - $44.50/hr" "$45/hr - $49.50/hr" "$50,000 - $59,999"  "$50/hr+"            "$60,000 - $69,999"  "$70,000 - $79,999" 
# [19] "$80,000 - $89,999"  "$90,000 - $99,999"  "Less than $10/hr"   "Less than $20,000"

#Salary, Hourly Wage
#Cleaning Minimum salary expectations

t2 = cbind.data.frame(Contact$Current_Salary__c, Contact$Id)
colnames(t2) = c("Current_Salary","id")
mmm = merge( t2, new_data, by = "id")
unique(mmm$Current_Salary)
mmm$Current_Salary = as.character(mmm$Current_Salary)
mmm$Salary_Level = as.character(mmm$Salary_Level)


ah = ifelse(mmm$Current_Salary != "NA", mmm$Current_Salary, mmm$Salary_Level)

#Location Indicator
C_m_postal_code = Contact[which(as.character(Contact$MailingPostalCode) != ""),]
C_m_postal_code$MailingPostalCode[1:500]





# Merging Set
#change to character
case$ContactId = as.character(case$ContactId)
Contact$Id = as.character(Contact$Id)

#create id variable to merge
Contact$id = Contact$Id
case$id = case$ContactId

#create new data set
df = merge(Contact, case, by = "id")

task2 = task[which(task$Discussion_Topic__c == "Salary / Job Offer Negotiation"),]

levels(Contact$Credit_Card_Number__c)
levels(Contact$Industry_hired_in__c)
length(which(Contact$Industry_hired_in__c != "NA"))



#####Q: Is there a specific demographic profile of clients that use volunteer services
#gender, location, service branch, level of education)?
#####Is there a specific demographic profile of clients that request volunteer services and then miss or cancel
#( “Missed/Cancelled” field in Request data)?


##################
#change to character
case$ContactId = as.character(case$ContactId)
contact$Id = as.character(contact$Id)

#create id variable to merge
contact$id = contact$Id
case$id = case$ContactId

#create new data set
df = merge(case, contact, by = "id")



x = full_join(contact, case, by = "id")
colnames(x)
x$mc = ifelse(grepl("Missed", x$Has_the_client_missed_this_opportunity__c), 1 , 0)
x$mc = ifelse(grepl("Cancelled", x$Has_the_client_missed_this_opportunity__c), 1 , x$mc)


glm(mc ~ MailingCity, data= x, family = binomial(link = "probit"))



#working on location shit

#user$UID = user$Id
case$UID = case$ContactId
wdf$UID = wdf$Id

u_in_case = join(user, case, by = "UID", type = "full")

merged_in_contact = join(u_in_case,wdf, by = "UID" , type = "left")
merged_in_contact$City = as.character(merged_in_contact$City)
merged_in_contact = merged_in_contact[which(merged_in_contact$City != ""),]

office = read.csv("Office Location.csv")
world = join(wdf, office, by = "X")
world$V1 = as.character(world$V1)
locations = world[which(world$V1 != ""),]
write.csv(locations, "locations.csv")

skippers$Id = skippers$id
world2 = join(skippers, world, by = "Id")
world2 = world2[which(world2$V1 != ""),]

#making a map of people who miss by their zip code
library(zipcode)
library(choroplethr)
library(choroplethrMaps)
library(ggmap)
data("zipcode")
omit_no_miss = world2[which(world2$M != ""),]
omit_no_miss$zip = omit_no_miss$postalcode
preformap = merge(omit_no_miss,zipcode, by.x = "zip", by.y = "zip")
preformap$M = as.numeric(preformap$M)

map_data = cbind.data.frame(preformap$M, preformap$city , preformap$state, preformap$latitude, preformap$longitude )
colnames(map_data) = c("Times Missed/Cancelled" ,"city", "state", "latitude", "longitude")

map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')

ggmap(map) + stat_bin_2d(aes(x=longitude, y=latitude, show_guide = TRUE),
  data=map_data, alpha=.5, na.rm = T, color = "red")
  

ddd = as.data.frame(aggregate(map_data$`Times Missed/Cancelled` ~ map_data$state, FUN = mean))
colnames(ddd) = c("state", "m")
ddd$value = ddd$m
ddd$region = stateFromLower(ddd$state)

#data for state map
state_map = cbind.data.frame(ddd$value, ddd$region)
colnames(state_map) = c("value","region")

#convert states to lower case
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

state_choropleth(state_map,num_colors = 1 )




class(newdataformap$M)
#scale_color_gradient(low = "#132B43", high = "#56B1F7")



aggregate(world2$hired~world2$V1, FUN = mean)
aggregate(world2$hired~ world2$missed, FUN = mean)
aggregate(world2$MID~ world2$V1, FUN = mean)

unique(world2$missed)

#summarizing misses by different locations
test_alpharetta = world2[which(world2$V1 == "Alpharetta"),]
test_atlanta = world2[which(world2$V1 == "Atlanta"),]
test_auburn = world2[which(world2$V1 == "Auburn"),]
test_colorado_springs = world2[which(world2$V1 == "Colorado Springs"),]
test_dallas = world2[which(world2$V1 == "Dallas"),]
test_rdu_airport = world2[which(world2$V1 == "RDU Airport"),]
test_san_diego = world2[which(world2$V1 == "San Diego"),]
test_torrance = world2[which(world2$V1 == "Torrance"),]

length(which(test_atlanta$MID != ""))









write.csv(world2, "world2.csv")
