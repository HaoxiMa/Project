#Generate the data set for SAS analysis

setwd("/Users/mahaoxi/Desktop/Project/BMC")

library(readxl)
library(tidyverse)
library(mice)
library(xlsx)

Demograph <- as.data.frame(read_excel("Final_dataset.xlsx", sheet = "Demographics"))
No_shows <- as.data.frame(read_excel("Final_dataset.xlsx", sheet = "No shows", range = cell_cols("A:D")))
Interp_first <- as.data.frame(read_excel("Final_dataset.xlsx", sheet = "Interp at First Completed Visit", range = cell_cols("A:D")))
All_completed <- as.data.frame(read_excel("Final_dataset.xlsx", sheet = "All Completed"))

#Calculate no show rate
Noshows_count <- No_shows %>% select(ID, SERVICE_DT) %>% group_by(ID) %>% summarise(noshows_times=n())
Completed_count <- All_completed %>% select(ID,NUM_COMPLETED_VISITS)
Noshow_rate_data <- left_join(Noshows_count, Completed_count, by="ID") %>%
  mutate(Noshow_rate = (noshows_times/(noshows_times+NUM_COMPLETED_VISITS))) %>%
  mutate(Noshow_rate = ifelse(is.na(Noshow_rate), 1, Noshow_rate)) %>%
  select(ID, Noshow_rate)

full_dataset <- left_join(Demograph, Noshow_rate_data)

md.pattern(Demograph, plot = FALSE)

#Child_Language
full_dataset$CHILD_LANG <- ifelse(full_dataset$CHILD_LANG %in% c("English", "Spanish", "Haitian Creole"), full_dataset$CHILD_LANG, "Other")

#Insurance
full_dataset <- full_dataset %>% 
  mutate(new_group = case_when(
    str_detect(PRIMARY_INSURANCE, "BCBS") ~ "BCBS",
    str_detect(PRIMARY_INSURANCE, "BMC") ~ "BMC",
    str_detect(PRIMARY_INSURANCE, "MEDICAID") ~ "MEDICAID",
    str_detect(PRIMARY_INSURANCE, "TUFTS") ~ "TUFTS",
    TRUE ~ PRIMARY_INSURANCE
  ))

full_dataset$PRIMARY_INSURANCE <- ifelse(full_dataset$new_group %in% c("BMC", "TUFTS", "MEDICAID", "BCBS"),
                                         full_dataset$new_group, ifelse(is.na(full_dataset$new_group), NA, "Other")) 
full_dataset <- full_dataset %>%
  select(-RACE,-ETHNICITY,-CITY,-BIRTH_MOM_EDUCATION1,-new_group)

#Mother Primary Language
full_dataset$BIRTH_MOM_LANG <- ifelse(full_dataset$BIRTH_MOM_LANG %in% c("English", "Spanish", "Haitian Creole"),
                                      full_dataset$BIRTH_MOM_LANG, ifelse(is.na(full_dataset$BIRTH_MOM_LANG),NA,"Other"))


#calculate whether use interpreter at first time
Whe_use <- function(str, unstr){
  if (is.na(str) & is.na(unstr)){
    return(sum(str, unstr))
  } else {
    return(sum(str, unstr, na.rm = TRUE))
  }
}

Interp_first$INTERP_UNSTRUC <- as.numeric(Interp_first$INTERP_UNSTRUC)
Interp_first$Interp_note <- mapply(Whe_use, Interp_first$INTERP_STRUC, Interp_first$INTERP_UNSTRUC)
Interp_first <- Interp_first %>% select(ID, Interp_note)

full_dataset <- full_dataset %>% left_join(Interp_first)

## New way to calculate the noshow rate--Because new patient is the frist appointment of the patient, which means, 
##although the patient no show, we can't say the interpreter influence

Noshows_count_interp <- No_shows %>% filter(APPT_TYPE_NM != "NEW PATIENT") %>%
  select(ID, SERVICE_DT) %>% group_by(ID) %>% summarise(noshows_times=n())

Completed_count <- All_completed %>% select(ID,NUM_COMPLETED_VISITS)

Cal_noshowrate <-function(x,y){
  if (is.na(x) & is.na(y)){
    return(NA)
  } else if (is.na(x) & !is.na(y)){
    return(0)
  } else if (!is.na(x) & is.na(y)){
    return(1)
  } else {
    return(x/(x+y))
  }
}

full_dataset <- left_join(full_dataset, Noshows_count_interp) %>% left_join(Completed_count)%>%
  mutate(Noshow_rate_interp = mapply(Cal_noshowrate,noshows_times, NUM_COMPLETED_VISITS)) %>%
  select(-noshows_times, -NUM_COMPLETED_VISITS)

write.xlsx(full_dataset, file = "Cleaned_data.xlsx")





