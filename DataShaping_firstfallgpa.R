### scrip for first fall gpa by tier / college/ departments/ program /cohort
#### first fall gpa trends
library(readr)
ENROLLMENT12162021 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/DATA 202108/202108/Updated 12-16-21/ENROLLMENTS.csv")
## uwf first fall gpa ftic 2021 max term 202201
UWFGPA_FTIC_202108 <- ENROLLMENT12162021 %>% group_by(UNIV_ROW_ID) %>% filter(DEMO_TIME_FRAME == max(DEMO_TIME_FRAME)) %>% 
    select(UNIV_ROW_ID, DEMO_TIME_FRAME, GPA_INST_HRS, GPA_INST_GRD_PTS) %>% mutate(UWFGPA_TEMP = round(GPA_INST_GRD_PTS/GPA_INST_HRS,2) )

## uwf gpa for all students and all terms
UWF_GPA_ALL_202201 <- ENROLLMENT12162021 %>%  group_by(UNIV_ROW_ID) %>% 
    select( "UWFID"=UNIV_ROW_ID,DEMO_DATA_SOURCE, DEMO_TIME_FRAME, GPA_INST_HRS, GPA_INST_GRD_PTS) %>% 
    mutate(UWFGPA_TEMP = round(GPA_INST_GRD_PTS/GPA_INST_HRS,2) )  
is.na(UWF_GPA_ALL_202201$UWFGPA_TEMP) <- sapply(UWF_GPA_ALL_202201$UWFGPA_TEMP, is.infinite)

## admitted ftic 2017 to 2021
library(readr)
trim_apr_data2_V0 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/Jay's Space/_HIGH_PRIORITY_PROJECTS/APR/APR FTIC2021/trim_apr_data2_V0.csv")
FTICID_gpa <- trim_apr_data2_V0 %>% select(Cohort, UWFID) %>% filter(Cohort>= 2017)
addmargins(table(trim_apr_data2_V0$Cohort))

# merge and create new columns for stopout in spring and AVE 2.77
UWFGPA_FTIC1 <- merge(FTICID_gpa,UWF_GPA_ALL_202201, by="UWFID", all.x = T ) %>% 
    filter((Cohort == 2017 & DEMO_TIME_FRAME <= 201801 & DEMO_DATA_SOURCE == "SIF") |
                   (Cohort == 2018 & DEMO_TIME_FRAME <= 201901 & DEMO_DATA_SOURCE == "SIF") |
                          (Cohort == 2019 & DEMO_TIME_FRAME <= 202001 & DEMO_DATA_SOURCE == "SIF") |
                                 (Cohort ==2020 & DEMO_TIME_FRAME <= 202101 & DEMO_DATA_SOURCE == "SIF") |
                                        (Cohort == 2021 & DEMO_TIME_FRAME <= 202201)) %>% # first fall gpa and hrs
    group_by(UWFID) %>% filter(DEMO_TIME_FRAME == max(DEMO_TIME_FRAME)) %>% 
    mutate(is.spring.stopout = ifelse(((Cohort  == 2017 & DEMO_TIME_FRAME != 201801) |
                                           (Cohort  == 2018 & DEMO_TIME_FRAME != 201901) |
                                           (Cohort  == 2019 & DEMO_TIME_FRAME != 202001) |
                                           (Cohort  == 2020 & DEMO_TIME_FRAME != 202101) |
                                           (Cohort  == 2021 & DEMO_TIME_FRAME != 202201) ) , "Yes","No"
                                       )) %>% 
    mutate(AVE2.77 = ifelse( is.spring.stopout == "Yes", "Stopout",
                              ifelse(is.na(UWFGPA_TEMP),"Withdrawn" , 
                            ifelse(UWFGPA_TEMP >= 2.77, "Above 2.77",
                                   ifelse((UWFGPA_TEMP < 2.77 & UWFGPA_TEMP >= 2.00), "GPA[2.00,2.77)", "Below 2.77"))))) %>% 
    mutate(GPA2.00 = ifelse(is.spring.stopout == "Yes", "Stopout",
                            ifelse(is.na(UWFGPA_TEMP),"Withdrawn" ,
                            ifelse(UWFGPA_TEMP >= 2.00, "Above 2.00", "Below 2.00"))))

addmargins(table(UWFGPA_FTIC1$Cohort, UWFGPA_FTIC1$DEMO_TIME_FRAME))
addmargins(table(UWFGPA_FTIC1$Cohort, UWFGPA_FTIC1$is.spring.stopout))
addmargins(table(UWFGPA_FTIC1$Cohort, UWFGPA_FTIC1$AVE2.77))
addmargins(table(UWFGPA_FTIC1$Cohort, UWFGPA_FTIC1$GPA2.00))
summary.table(table(UWFGPA_FTIC1$Cohort, UWFGPA_FTIC1$AVE2.77))
NAGPA <- UWFGPA_FTIC1[is.na(UWFGPA_FTIC1$UWFGPA_TEMP),]
colnames(UWFGPA_FTIC1) <-  paste("F1", colnames(UWFGPA_FTIC1), sep = "_") 

### Y1 GPA
UWFGPA_FTICY1 <- merge(FTICID_gpa,UWF_GPA_ALL_202201, by="UWFID", all.x = T ) %>% 
    filter((Cohort == 2017 & DEMO_TIME_FRAME <= 201808 & DEMO_DATA_SOURCE == "SIF") |
               (Cohort == 2018 & DEMO_TIME_FRAME <= 201908 & DEMO_DATA_SOURCE == "SIF") |
               (Cohort == 2019 & DEMO_TIME_FRAME <= 202008 & DEMO_DATA_SOURCE == "SIF") |
               (Cohort == 2020 & DEMO_TIME_FRAME <= 202108 & (DEMO_DATA_SOURCE == "SIF" | DEMO_DATA_SOURCE =="SIFP" )  ) |
               (Cohort == 2021 & DEMO_TIME_FRAME <= 202208)) %>% # first fall gpa and hrs
    group_by(UWFID) %>% filter(DEMO_TIME_FRAME == max(DEMO_TIME_FRAME) ) %>% 
    filter(!duplicated(UWFID)) %>% 
    mutate(is.dropout = ifelse(((Cohort  == 2017 & DEMO_TIME_FRAME != 201808) |
                                (Cohort  == 2018 & DEMO_TIME_FRAME != 201908) |
                                (Cohort  == 2019 & DEMO_TIME_FRAME != 202008) |
                                (Cohort  == 2020 & DEMO_TIME_FRAME != 202108) |
                                (Cohort  == 2021 & DEMO_TIME_FRAME != 202208) ) , "Yes","No"
    )) %>% 
    mutate(AVE2.77 = ifelse( is.dropout == "Yes", "Dropout",
                             ifelse(is.na(UWFGPA_TEMP),"Withdrawn" , 
                                    ifelse(UWFGPA_TEMP >= 2.77, "Above 2.77",
                                           ifelse((UWFGPA_TEMP < 2.77 & UWFGPA_TEMP >= 2.00), "GPA[2.00,2.77)", "Below 2.77"))))) %>% 
    mutate(GPA2.00 = ifelse(is.dropout == "Yes", "Dropout",
                            ifelse(is.na(UWFGPA_TEMP),"Withdrawn" ,
                                   ifelse(UWFGPA_TEMP >= 2.00, "Above 2.00", "Below 2.00")))) 

addmargins(table(UWFGPA_FTICY1$Cohort, UWFGPA_FTICY1$DEMO_TIME_FRAME))
addmargins(table(UWFGPA_FTICY1$Cohort, UWFGPA_FTICY1$is.dropout))
addmargins(table(UWFGPA_FTICY1$Cohort, UWFGPA_FTICY1$AVE2.77))
addmargins(table(UWFGPA_FTICY1$Cohort, UWFGPA_FTICY1$GPA2.00))
summary.table(table(UWFGPA_FTICY1$Cohort, UWFGPA_FTICY1$AVE2.77))
NAGPA <- UWFGPA_FTICY1[is.na(UWFGPA_FTICY1$UWFGPA_TEMP),]
colnames(UWFGPA_FTICY1) <-  paste("Y1", colnames(UWFGPA_FTICY1), sep = "_") 
dupid <- UWFGPA_FTICY1[duplicated(UWFGPA_FTICY1$Y1_UWFID),]

# F1 + Y1
UWFGPA_FTIC <-  merge(UWFGPA_FTIC1, UWFGPA_FTICY1, by.x ="F1_UWFID", by.y="Y1_UWFID", all = T) # 5291
FTIC2017 <-  UWFGPA_FTIC  %>% filter(F1_Cohort == 2017) %>%  select("UWFID"= F1_UWFID)
FTIC2018 <-  UWFGPA_FTIC  %>% filter(F1_Cohort == 2018) %>%  select("UWFID"= F1_UWFID)
FTIC2019 <-  UWFGPA_FTIC  %>% filter(F1_Cohort == 2019) %>%  select("UWFID"= F1_UWFID)
FTIC2020 <-  UWFGPA_FTIC  %>% filter(F1_Cohort == 2020) %>%  select("UWFID"= F1_UWFID)
FTIC2021 <-  UWFGPA_FTIC  %>% filter(F1_Cohort == 2021) %>%  select("UWFID"= F1_UWFID)
# import new gpa data
library(readr)
UWFGPA_crs_grade_points_ends202108 <- read_csv("UWFGPA_crs_grade_points_ends202108.csv")
UWFGPA_2017 <- merge(UWFGPA_crs_grade_points_ends202108, FTIC2017, by ="UWFID", all.y = TRUE) %>% 
    group_by(UWFID) %>% arrange(crs_DEMO_TIME) %>%  
    filter(crs_DEMO_TIME <= 201808 & crs_DEMO_TIME >= 201708 ) %>%  
    mutate(Term_id = ifelse(crs_DEMO_TIME == 201708 , "Fall1",
                     ifelse(crs_DEMO_TIME == 201801 , "Spring1",
                     ifelse(crs_DEMO_TIME == 201805 , "Summer1", 
                     ifelse(crs_DEMO_TIME == 201808 , "Fall2",NA)))))
UWFGPA_2018 <- merge(UWFGPA_crs_grade_points_ends202108, FTIC2018, by ="UWFID", all.y = TRUE) %>% 
    group_by(UWFID) %>% arrange(crs_DEMO_TIME) %>%  
    filter(crs_DEMO_TIME <= 201908 & crs_DEMO_TIME >= 201808 ) %>%  
    mutate(Term_id = ifelse(crs_DEMO_TIME == 201808 , "Fall1",
                            ifelse(crs_DEMO_TIME == 201901 , "Spring1",
                                   ifelse(crs_DEMO_TIME == 201905 , "Summer1", 
                                          ifelse(crs_DEMO_TIME == 201908 , "Fall2",NA)))))
UWFGPA_2019 <- merge(UWFGPA_crs_grade_points_ends202108, FTIC2019, by ="UWFID", all.y = TRUE) %>% 
    group_by(UWFID) %>% arrange(crs_DEMO_TIME) %>%  
    filter(crs_DEMO_TIME <= 202008 & crs_DEMO_TIME >= 201908 ) %>%  
    mutate(Term_id = ifelse(crs_DEMO_TIME == 201908 , "Fall1",
                            ifelse(crs_DEMO_TIME == 202001 , "Spring1",
                                   ifelse(crs_DEMO_TIME == 202005 , "Summer1", 
                                          ifelse(crs_DEMO_TIME == 202008 , "Fall2",NA)))))
UWFGPA_2020 <- merge(UWFGPA_crs_grade_points_ends202108, FTIC2020, by ="UWFID", all.y = TRUE) %>% 
    group_by(UWFID) %>% arrange(crs_DEMO_TIME) %>%  
    filter(crs_DEMO_TIME <= 202108 & crs_DEMO_TIME >= 202008 ) %>%  
    mutate(Term_id = ifelse(crs_DEMO_TIME == 202008 , "Fall1",
                            ifelse(crs_DEMO_TIME == 202101 , "Spring1",
                                   ifelse(crs_DEMO_TIME == 202105 , "Summer1", 
                                          ifelse(crs_DEMO_TIME == 202108 , "Fall2",NA)))))
UWFGPA_2021 <- merge(UWFGPA_crs_grade_points_ends202108, FTIC2021, by ="UWFID", all.y = TRUE) %>% 
    group_by(UWFID) %>% arrange(crs_DEMO_TIME) %>%  
    filter(crs_DEMO_TIME <= 202208 & crs_DEMO_TIME >= 202108 ) %>%  
    mutate(Term_id = ifelse(crs_DEMO_TIME == 202108 , "Fall1",
                            ifelse(crs_DEMO_TIME == 202201 , "Spring1",
                                   ifelse(crs_DEMO_TIME == 202205 , "Summer1", 
                                          ifelse(crs_DEMO_TIME == 202208 , "Fall2",NA)))))
 
UWFGPA_CRS_TAKEN <- rbind(UWFGPA_2017,UWFGPA_2018,UWFGPA_2019,UWFGPA_2020,UWFGPA_2021)
colnames(UWFGPA_CRS_TAKEN) <- paste('CRS', colnames(UWFGPA_CRS_TAKEN), sep="_")
UWFGPA_FTIC_V0 <- merge(UWFGPA_FTIC, UWFGPA_CRS_TAKEN , by.x= "F1_UWFID", by.y= "CRS_UWFID", all.x = T)
write.csv(UWFGPA_FTIC_V0, "UWFGPA_FTIC_V0.csv", row.names = F)

