
library(data.table)
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html


library(dplyr)

# 
# *****************************************************************************
# subjects1: “subject_label” variable for studysum table or registry table
# 
# 
# non-missing exam date2: from registry table where variable dd_field.name= “examdate” 
# and variable dd_revision_field.translated_value has non-missing value
# 
# 
# exam date3: from registry table where variable dd_field.name= “examdate”
# 
# visit4: “event.label” from registry table 
# 
# 
# disposition status5: from registry table where variable dd_field.name= “sdstatus” and 
# variable dd_revision_field.translated_value = “Never randomized”
# 
# subjects6: “PARTICIPANT ID” variable for a3blinded_prod table or a45blinded_prod table
# ******************************************************************************


# read the data
registry <- fread("nbg_enrollment_registry.csv")

names(registry)


#registry$`Dd Revision Field Translated Value`


studysum <- fread("nbg_enrollment_studysum.csv")


names(studysum)
a3blinded_prod <- fread("nbg_enrollment_a3blinded_prod.csv")

a45blinded_prod <- fread("nbg_enrollment_a45blinded_prod.csv")


#a45blinded_prod


# 1.	Consented: Unique subjects1 from either registry table OR studysum table
# Screen Fail pre-SV1: Unique subjects1 from studysum table that DO NOT have any non-missing exam date2 from the registry table.
# 

ls()
#Consented <- registry$`Subject Label` %>% union(studysum$`Subject Label`) %>% unique() 

df1 <- registry %>% select(`Subject Label`) 
df2 <- studysum %>% select(`Subject Label`)
df3 <- union(df1, df2) %>% distinct()
df3
names(df3)

Consented <- df3

Consented
write.csv(Consented, "Consented.csv")


#df1 <- registry %>% filter(`Dd Field Name` == "examdate") %>% filter(`Dd Revision Field Translated Value` != "")

df1 <- registry[`Dd Field Name` == "examdate" & `Dd Revision Field Translated Value` != ""]


df2<- studysum 

df3 <-df2[!(df2$`Subject Label` %in% df1$`Subject Label`), ] 

class(df3)

#df3$`Subject Label`


Screen_Fail_pre_SV1 <- df3 %>% select(`Subject Label`) %>% distinct()

names(Screen_Fail_pre_SV1)


write.csv(Screen_Fail_pre_SV1, "Screen_Fail_pre_SV1.csv")


# 2.	SV1
# Completed: Unique subjects1 who have completed the visit4 “Screening - Stage 1” with
#a non-missing exam date3 in the registry table.

# Pending: subjects1 who have visit4 “Screening - Stage 1” when
#the subjects1 have the maximum exam date3 AND the subjects1 are not present in the studysum table 


# Screen Fail: subjects1 who have visit4 “Screening - Stage 1” when the subjects1 have
# the maximum exam date3 AND the subjects1 are present in the studysum table with disposition status5 ‘Never randomized’
# 
# 


unique(registry$`Event Label`)

df1  <-  registry[`Dd Field Name` == "examdate" & `Event Label` == "Screening - Stage 1" &
                    `Dd Revision Field Translated Value` != ""]


SV1_Completed <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()

SV1_Completed



write.csv(SV1_Completed, "SV1_Completed.csv")


df1 <- registry[`Event Label`=="Screening - Stage 1"& `Dd Field Name` == "examdate"] %>% 
  filter( `Dd Revision Field Translated Value` != "") %>% 
  group_by(`Subject Label`) %>% 
  summarise(max_examdate = max(`Dd Revision Field Translated Value`))  

df2 <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()


df3<- studysum 

df4 <- Consented[!( Consented$`Subject Label` %in% df3$`Subject Label`),]



df5 <- df2 %>% inner_join(df4, by= "Subject Label") %>% distinct()


SV1_Pending <- df5

SV1_Pending

write.csv(SV1_Pending, "SV1_Pending.csv")


#SV1_Screen_Fail <- 

  
  
df1 <- registry[`Event Label`=="Screening - Stage 1"& `Dd Field Name` == "examdate"] %>% 
  filter( `Dd Revision Field Translated Value` != "") %>% 
  group_by(`Subject Label`) %>% 
  summarise(max_examdate = max(`Dd Revision Field Translated Value`))  

df2 <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()


df3 <- studysum[`Dd Field Name` == "sdstatus" &  `Dd Revision Field Translated Value` == "Never randomized" ] %>%
  distinct() %>% select(`Subject Label`)


df4 <- Consented[Consented$`Subject Label` %in% df3$`Subject Label`] %>% distinct()

df5 <- df2 %>% inner_join(df4, by= "Subject Label") %>% distinct()


SV1_Screen_Fail <- df5



SV1_Screen_Fail

dim(SV1_Screen_Fail)



write.csv(
  SV1_Screen_Fail, "SV1_Screen_Fail.csv")

# 
# 
# 
# 3.	SV2
# Completed: Unique subjects1 who have completed the visit4 “Screening - Stage 2” with a non-missing exam date3 in the registry table.
# Pending: subjects1 who have visit4 “Screening - Stage 2” when the subjects1 have the maximum exam date3 AND the subjects1 are not present in the studysum table 
# Screen Fail: subjects1 who have visit4 “Screening - Stage 2” when the subjects1 have the maximum exam date3 AND the subjects1 are present in the studysum table 
# with disposition status5 ‘Never randomized’
# 
# 
# 
# 
# 


df1  <-  registry[`Dd Field Name` == "examdate" & `Event Label` == "Screening - Stage 2" &
                    `Dd Revision Field Translated Value` != ""]


SV2_Completed <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()

SV2_Completed

write.csv(SV2_Completed, "SV2_Completed.csv")







df1 <- registry[`Event Label`=="Screening - Stage 2"& `Dd Field Name` == "examdate"] %>% 
  filter( `Dd Revision Field Translated Value` != "") %>% 
  group_by(`Subject Label`) %>% 
  summarise(max_examdate = max(`Dd Revision Field Translated Value`))  

df2 <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()


df3<- studysum 

df4 <- Consented[!( Consented$`Subject Label` %in% df3$`Subject Label`),]


df5 <- df2 %>% inner_join(df4, by= "Subject Label") %>% distinct()


SV2_Pending <- df5

SV2_Pending


write.csv(SV2_Pending, "SV2_Pending.csv")











df1 <- registry[`Event Label`=="Screening - Stage 2"& `Dd Field Name` == "examdate"] %>% 
  filter( `Dd Revision Field Translated Value` != "") %>% 
  group_by(`Subject Label`) %>% 
  summarise(max_examdate = max(`Dd Revision Field Translated Value`))  

df2 <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()


df3 <- studysum[`Dd Field Name` == "sdstatus" &  `Dd Revision Field Translated Value` == "Never randomized" ] %>%
  distinct() %>% select(`Subject Label`)


df4 <- Consented[Consented$`Subject Label` %in% df3$`Subject Label`] %>% distinct()



df5 <- df2 %>% inner_join(df4, by= "Subject Label") %>% distinct()


SV2_Screen_Fail <- df5






write.csv(
  SV2_Screen_Fail, "SV2_Screen_Fail.csv")







# 
# 
# 
# 4.	SV3
# Completed: Unique subjects1 who have completed the visit4 “Screening - Stage 3” with a non-missing exam date3 in the registry table.
# Pending: subjects1 who have visit4 “Screening - Stage 3” when the subjects1 have the maximum exam date3 AND the subjects1 are not present in the studysum table 
# Screen Fail: subjects1 who have visit4 “Screening - Stage 3 when the subjects1 have the maximum exam date3 AND the subjects1 are present in the studysum table with disposition status5 ‘Never randomized’
# 
# 
# 
# 




df1  <-  registry[`Dd Field Name` == "examdate" & `Event Label` == "Screening - Stage 3" &
                    `Dd Revision Field Translated Value` != ""]


SV3_Completed <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()



write.csv(SV3_Completed, "SV3_Completed.csv")





df1 <- registry[`Event Label`=="Screening - Stage 3"& `Dd Field Name` == "examdate"] %>% 
  filter( `Dd Revision Field Translated Value` != "") %>% 
  group_by(`Subject Label`) %>% 
  summarise(max_examdate = max(`Dd Revision Field Translated Value`))  

df2 <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()


df3<- studysum 

df4 <- Consented[!( Consented$`Subject Label` %in% df3$`Subject Label`),]


df5 <- df2 %>% inner_join(df4, by= "Subject Label") %>% distinct()


SV3_Pending <- df5


write.csv(SV3_Pending, "SV3_Pending.csv")







df1 <- registry[`Event Label`=="Screening - Stage 3"& `Dd Field Name` == "examdate"] %>% 
  filter( `Dd Revision Field Translated Value` != "") %>% 
  group_by(`Subject Label`) %>% 
  summarise(max_examdate = max(`Dd Revision Field Translated Value`))  

df2 <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()


df3 <- studysum[`Dd Field Name` == "sdstatus" &  `Dd Revision Field Translated Value` == "Never randomized" ] %>%
  distinct() %>% select(`Subject Label`)


df4 <- Consented[Consented$`Subject Label` %in% df3$`Subject Label`] %>% distinct()

df5 <- df2 %>% inner_join(df4, by= "Subject Label") %>% distinct()


SV3_Screen_Fail <- df5




write.csv(
  SV3_Screen_Fail, "SV3_Screen_Fail.csv")










# 
# 
# 5.	SV4
# Completed: Unique subjects1 who have completed the visit4 “Screening - Stage 4” with a non-missing exam date3 in the registry table.
# Pending: subjects1 who have visit4 “Screening - Stage 4” when the subjects1 have the maximum exam date3 AND the subjects1 are not present in the studysum table 
# Screen Fail: subjects1 who have visit4 “Screening - Stage 4 when the subjects1 have the maximum exam date3 AND the subjects1 are present in the studysum table with disposition status5 ‘Never randomized’
# 






df1  <-  registry[`Dd Field Name` == "examdate" & `Event Label` == "Screening - Stage 4" &
                    `Dd Revision Field Translated Value` != ""]


SV4_Completed <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()



write.csv(SV4_Completed, "SV4_Completed.csv")





df1 <- registry[`Event Label`=="Screening - Stage 4"& `Dd Field Name` == "examdate"] %>% 
  filter( `Dd Revision Field Translated Value` != "") %>% 
  group_by(`Subject Label`) %>% 
  summarise(max_examdate = max(`Dd Revision Field Translated Value`))  

df2 <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()


df3<- studysum 

df4 <- Consented[!( Consented$`Subject Label` %in% df3$`Subject Label`),]


df5 <- df2 %>% inner_join(df4, by= "Subject Label") %>% distinct()


SV4_Pending <- df5


write.csv(SV4_Pending, "SV4_Pending.csv")







df1 <- registry[`Event Label`=="Screening - Stage 4"& `Dd Field Name` == "examdate"] %>% 
  filter( `Dd Revision Field Translated Value` != "") %>% 
  group_by(`Subject Label`) %>% 
  summarise(max_examdate = max(`Dd Revision Field Translated Value`))  

df2 <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()


df3 <- studysum[`Dd Field Name` == "sdstatus" &  `Dd Revision Field Translated Value` == "Never randomized" ] %>%
  distinct() %>% select(`Subject Label`)


df4 <- Consented[Consented$`Subject Label` %in% df3$`Subject Label`] %>% distinct()

df5 <- df2 %>% inner_join(df4, by= "Subject Label") %>% distinct()


SV4_Screen_Fail <- df5






write.csv(
  SV4_Screen_Fail, "SV4_Screen_Fail.csv")






# 
# 6.	SV5
# Completed: Unique subjects1 who have completed the visit4 “Screening - Stage 5” with a non-missing exam date3 in the registry table.
# Pending: subjects1 who have visit4 “Screening - Stage 5” when the subjects1 have the maximum exam date3 AND the subjects1 are not present in the studysum table 
# Screen Fail: subjects1 who have visit4 “Screening - Stage 5 when the subjects1 have the maximum exam date3 AND the subjects1 are present in the studysum table with disposition status5 ‘Never randomized’
# 








df1  <-  registry[`Dd Field Name` == "examdate" & `Event Label` == "Screening - Stage 5" &
                    `Dd Revision Field Translated Value` != ""]


SV5_Completed <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()



write.csv(SV5_Completed, "SV5_Completed.csv")





df1 <- registry[`Event Label`=="Screening - Stage 5"& `Dd Field Name` == "examdate"] %>% 
  filter( `Dd Revision Field Translated Value` != "") %>% 
  group_by(`Subject Label`) %>% 
  summarise(max_examdate = max(`Dd Revision Field Translated Value`))  

df2 <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()


df3<- studysum 

df4 <- Consented[!( Consented$`Subject Label` %in% df3$`Subject Label`),]


df5 <- df2 %>% inner_join(df4, by= "Subject Label") %>% distinct()


SV5_Pending <- df5


write.csv(SV5_Pending, "SV5_Pending.csv")







df1 <- registry[`Event Label`=="Screening - Stage 5"& `Dd Field Name` == "examdate"] %>% 
  filter( `Dd Revision Field Translated Value` != "") %>% 
  group_by(`Subject Label`) %>% 
  summarise(max_examdate = max(`Dd Revision Field Translated Value`))  

df2 <- Consented[Consented$`Subject Label` %in% df1$`Subject Label`] %>% distinct()


df3 <- studysum[`Dd Field Name` == "sdstatus" &  `Dd Revision Field Translated Value` == "Never randomized" ] %>%
  distinct() %>% select(`Subject Label`)


df4 <- Consented[Consented$`Subject Label` %in% df3$`Subject Label`] %>% distinct()

df5 <- df2 %>% inner_join(df4, by= "Subject Label") %>% distinct()


SV5_Screen_Fail <- df5






write.csv(
  SV5_Screen_Fail, "SV5_Screen_Fail.csv")




ls()


# 
# 7.	Randomized: Unique subjects6 from either a3blinded_prod table or a45blinded_prod table
# Pending Randomization: (<SV5>.Completed minus <SV5>.Screen Fail minus Randomized)
# 
# Note: <SV5> data from step 6
# 
# 
# 

a3blinded_prod <- fread("nbg_enrollment_a3blinded_prod.csv")

a45blinded_prod <- fread("nbg_enrollment_a45blinded_prod.csv")


library(dplyr)
names(a3blinded_prod)

a3blinded_prod$`Site Id`





df1 <- a3blinded_prod %>% select(`Participant Id`) 
df2 <- a45blinded_prod %>% select(`Participant Id`)
df3 <- union(df1, df2) %>% distinct()
df3
names(df3)

Randomized <- df3

Consented
write.csv(Randomized, "Randomized.csv")


# 
df1 <- SV5_Completed[!(SV5_Completed$`Subject Label` %in% SV5_Screen_Fail$`Subject Label` )]


df2 <- df1[!(df1$`Subject Label` %in% Randomized$`Participant Id`  )] 


PendingRandomization <- df2 %>% distinct()


PendingRandomization

write.csv(PendingRandomization, "PendingRandomization.csv")






class(Consented)

dim(Consented)
names(Consented)

Consented_t <- data.frame(rep("Consented", nrow(Consented)), Consented$`Subject Label`)
colnames(Consented_t) <- c("Category", "Subject Label")

head(Consented_t)

write.csv(Consented_t, "Consented_t.csv")



Screen_Fail_pre_SV1_t <- data.frame(rep("Screen_Fail_pre_SV1", nrow(Screen_Fail_pre_SV1)), Screen_Fail_pre_SV1$`Subject Label`)
colnames(Screen_Fail_pre_SV1_t) <- c("Category", "Subject Label")


write.csv(Screen_Fail_pre_SV1_t, "Screen_Fail_pre_SV1_t.csv")




SV1_Completed_t <- data.frame(rep("SV1_Completed", nrow(SV1_Completed)), SV1_Completed$`Subject Label`)
colnames(SV1_Completed_t) <- c("Category", "Subject Label")
write.csv(SV1_Completed_t, "SV1_Completed_t.csv")




SV1_Pending_t <- data.frame(rep("SV1_Pending", nrow(SV1_Pending)), SV1_Pending$`Subject Label`)
colnames(SV1_Pending_t) <- c("Category", "Subject Label")

write.csv(SV1_Pending_t, "SV1_Pending_t.csv")



SV1_Screen_Fail

SV1_Screen_Fail_t <- data.frame(rep("SV1_Screen_Fail", nrow(SV1_Screen_Fail)), SV1_Screen_Fail$`Subject Label`)
colnames(SV1_Screen_Fail_t) <- c("Category", "Subject Label")

View(SV1_Screen_Fail_t)

write.csv(SV1_Screen_Fail_t, "SV1_Screen_Fail_t.csv")



SV2_Completed_t <- data.frame(rep("SV2_Completed", nrow(SV2_Completed)), SV2_Completed$`Subject Label`)
colnames(SV2_Completed_t) <- c("Category", "Subject Label")

write.csv(SV2_Completed_t, "SV2_Completed_t.csv")


SV2_Pending_t <- data.frame(rep("SV2_Pending", nrow(SV2_Pending)),SV2_Pending$`Subject Label`)
colnames(SV2_Pending_t) <- c("Category", "Subject Label")
write.csv(SV2_Pending_t, "SV2_Pending_t.csv")



SV2_Screen_Fail_t <- data.frame(rep("SV2_Screen_Fail", nrow(SV2_Screen_Fail)),SV2_Screen_Fail$`Subject Label`)
colnames(SV2_Screen_Fail_t) <- c("Category", "Subject Label")
write.csv(SV2_Screen_Fail_t, "SV2_Screen_Fail_t.csv")


SV3_Completed_t <- data.frame(rep("SV3_Completed", nrow(SV3_Completed)), SV3_Completed$`Subject Label`)
colnames(SV3_Completed_t) <- c("Category", "Subject Label")
write.csv(SV3_Completed_t, "SV3_Completed_t.csv")





SV3_Pending_t <- data.frame(rep("SV3_Pending", nrow(SV3_Pending)),SV3_Pending$`Subject Label`)
colnames(SV3_Pending_t) <- c("Category", "Subject Label")

write.csv(SV3_Pending_t, "SV3_Pending_t.csv")




SV3_Screen_Fail_t <- data.frame(rep("SV3_Screen_Fail", nrow(SV3_Screen_Fail)),SV3_Screen_Fail$`Subject Label`)
colnames(SV3_Screen_Fail_t) <- c("Category", "Subject Label")
write.csv(SV3_Screen_Fail_t, "SV3_Screen_Fail_t.csv")



SV4_Completed_t <- data.frame(rep("SV4_Completed", nrow(SV4_Completed)), SV4_Completed$`Subject Label`)
colnames(SV4_Completed_t) <- c("Category", "Subject Label")
write.csv(SV4_Completed_t, "SV4_Completed_t.csv")






SV4_Pending_t <- data.frame(rep("SV4_Pending", nrow(SV4_Pending)),SV4_Pending$`Subject Label`)
colnames(SV4_Pending_t) <- c("Category", "Subject Label")
write.csv(SV4_Pending_t, "SV4_Pending_t.csv")


SV4_Screen_Fail_t <- data.frame(rep("SV4_Screen_Fail", nrow(SV4_Screen_Fail)),SV4_Screen_Fail$`Subject Label`)
colnames(SV4_Screen_Fail_t) <- c("Category", "Subject Label")
write.csv(SV4_Screen_Fail_t, "SV4_Screen_Fail_t.csv")




SV5_Completed_t <- data.frame(rep("SV5_Completed", nrow(SV5_Completed)), SV5_Completed$`Subject Label`)
colnames(SV5_Completed_t) <- c("Category", "Subject Label")
write.csv(SV5_Completed_t, "SV5_Completed_t.csv")




SV5_Pending_t <- data.frame(rep("SV5_Pending", nrow(SV5_Pending)),SV5_Pending$`Subject Label`)
colnames(SV5_Pending_t) <- c("Category", "Subject Label")
write.csv(SV5_Pending_t, "SV5_Pending_t.csv")




SV5_Screen_Fail_t <- data.frame(rep("SV5_Screen_Fail", nrow(SV5_Screen_Fail)),SV5_Screen_Fail$`Subject Label`)
colnames(SV5_Screen_Fail_t) <- c("Category", "Subject Label")
write.csv(SV5_Screen_Fail_t , "SV5_Screen_Fail_t.csv")




Randomized_t <- data.frame(rep("Randomized", nrow(Randomized)),Randomized$`Participant Id`)
colnames(Randomized_t) <- c("Category", "Subject Label")
write.csv(Randomized_t, "Randomized_t.csv")




PendingRandomization_t <- data.frame(rep("PendingRandomization", nrow(PendingRandomization)),PendingRandomization$`Subject Label`)
colnames(PendingRandomization_t) <- c("Category", "Subject Label")
write.csv(PendingRandomization_t, "PendingRandomization_t.csv")





SV_all <- rbind(Consented_t,  
Screen_Fail_pre_SV1_t,
        
SV1_Screen_Fail_t ,
              
SV1_Completed_t,        

SV1_Pending_t,          

SV2_Completed_t,        

SV2_Pending_t,          

SV2_Screen_Fail_t,      

SV3_Completed_t,        

SV3_Pending_t,          

SV3_Screen_Fail_t,      

SV4_Completed_t,        

SV4_Pending_t,          

SV4_Screen_Fail_t,      

SV5_Completed_t,        

SV5_Pending_t,          

SV5_Screen_Fail_t,


Randomized_t,

PendingRandomization_t
)     %>% distinct()



write.csv(SV_all, "SV_all.csv")
















