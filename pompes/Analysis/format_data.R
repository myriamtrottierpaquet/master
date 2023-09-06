rm(list=ls())

library(tidyverse)
library(magrittr)
library(ggplot2)

cort <- readxl::read_xlsx("pompes/data/CORT_zoo.xlsx",
                              sheet = 1, 
                              col_names = T, 
                              na = c("-", ""))

write_rds(cort, "CORT_zoo.Rds")

#quick overview
glimpse(cort)
head(cort) ;tail(cort)
summary(cort)
dim(cort)

#quick plot
ggplot(subset(cort, species=="Canada Goose"), aes(date, cort))+geom_point()+geom_smooth()+ggtitle("Canada Goose")
ggplot(subset(cort, species=="Snow Goose"), aes(date, cort))+geom_point()+geom_smooth()+ggtitle("Snow Goose")

##################################
### CORRECT PROBLEMS WITH DATA ###
##################################

# --> The following code corrects some inconsistency in the dataset

raw_data[grepl("BSO", raw_data$Sample_ID), ]$date_de_dépôt <- "13-07-95"
raw_data[grepl("BSO", raw_data$Sample_ID), ]$`16-05-95` <- NA
raw_data[grepl("BWO", raw_data$Sample_ID), ]$`16-05-95` <- NA

####################
### TIDY DATASET ###
####################

# --> The following code formats the dataset so it is "tidy"

data.frame(colnames(raw_data))# get number of columns 

poop = raw_data %>% 
  rename(sample_id = Sample_ID,
         habitat = O_C,
         season = S_W,
         mass = init_mass_g,
         date_dropped = date_de_dépôt) %>% # shorter english names , lower caps
  pivot_longer(cols = 7:80,
               names_to = "visit",
               values_to = "state",
               values_drop_na = T) %>% # convert to long format
  mutate(date_dropped = lubridate::dmy_hms(paste(date_dropped, "00:00:00")),
         date_visited = lubridate::parse_date_time(visit, 
                                                   c("dmy", "dmY")),# parse date
         species = case_when(species =="goat"~ "Mountain goat",
                             species == "White tailed"~ "White-tailed deer",
                             T~species), # set consistent names
         size = factor(case_when(mass == "P" ~ "small",
                                 mass == "M" ~ "medium",
                                 mass == "G" ~ "big", 
                                 T~NA_character_), 
                       levels = c("small", "medium", "big")),
         mass=as.numeric(mass)) %>% # split real mass from sizes
  mutate_at(vars("sample_id", 
                 "species", 
                 "habitat",
                 "season"), 
            ~as.factor(as.character(.))) %>% # format these as factors
  relocate(sample_id:mass, 
           size, 
           date_dropped,
           date_visited,
           #visit, 
           state) %>% 
  arrange(sample_id, 
          date_dropped, 
          date_visited)

# --> The following code converts all state = 8 to 7 as it is supposed to be the maximum 
# value in the chart. 

poop[poop$state == 8, ]$state <- 7 # error !

#################################
### ADD STATE AT DATE DROPPED ###
#################################

# --> The following code adds a new entry at date 1 with state = 1 for each sample id.
# These new records are then combined with the rest of the dataset in a new dataframe
# named poopy.

poopy<-rbind(subset(poop, duplicated(sample_id)==F)%>%
               select(1:9)%>%
               mutate(state=1)%>%
               rename(date_visited2=date_dropped) %>% 
               select(-date_visited) %>% 
               rename(date_visited = date_visited2) %>% 
               mutate(date_dropped = date_visited) %>% 
               select(1:6,9,7:8),
             select(poop, c(1:9)))


###############################
### CORRECT INCONSISTENCIES ###
###############################

# --> Are there any inconsistencies in states among samples (e.g. state improved with time) ? 

prob_ids=poopy %>% 
  mutate(bugs=case_when(sample_id ==lag(sample_id) & state < lag(state)~ "state_improved_w_time", #makes new column which indicates
                        T~"ok")) %>% 
  subset(!bugs=="ok") %>% 
  select(sample_id, bugs) 

nrow(prob_ids)

#There are 118 out of 200 cases where a state improved with time

# --> Correction of consistencies based on these assumptions:

#1: State=1 for any visit on date_dropped.
#2: The value recorded on the last visit is TRUE.
#3: The number of error is the smallest possible.
#4: The value of the error is the smallest possible.
#5: States recorded at earlier visits are more subject to errors than later visits.

poopy3 <- poopy %>%
  arrange(sample_id, date_visited) %>%
  group_by(sample_id) %>%  
  mutate(last_visit = max(date_visited),
         max_state = dplyr::last(state)) %>% 
  #Line added to specify state=1 when 1st visit on date dropped. 
  mutate(state1 = ifelse(date_visited == date_dropped, 1, state)) %>% 
  #The next line deletes and replaces any value bigger than the state on the 
  #last visit by the maximum value.
  mutate(cor_state = ifelse(state1 > max_state, max_state, state1)) %>% 
  #Simple correction: The next line creates a column that flags a state when it is different than
  #lead and lag and in the case that lead and lag are equals.
  mutate(middle_weirdo = ifelse(row_number() > 1 & row_number() < n() & cor_state != lag(cor_state) & cor_state != lead(cor_state) & lead(cor_state) == lag(cor_state), #1 
                                "middle_weirdo", 
                                "is_ok")) %>% 
  #This next line creates a column that flags state when they are bigger than lead or smaller than the lag.
  mutate(bug_type = case_when(cor_state > lead(cor_state) & row_number() != n() ~ "bigger_than_lead",
                              row_number() > 1 & cor_state < lag(cor_state) & row_number() != n() ~ "smaller_than_lag",
                              TRUE ~ "ok")) %>% 
  #When the lead value isn't "bigger than lead", the lag value isn't "smaller 
  #than lag" and when the previous value is the same as the one before,
  #the next line corrects these cases, specifying when state is bigger than
  #lead, then the states takes the minimum value and when state is smaller than
  #it takes the max value.
  mutate(cor2_state = case_when(middle_weirdo == "middle_weirdo" & cor_state < lead(cor_state) & lead(bug_type)!="bigger_than_lead" & lag(bug_type)!= "smaller_than_lag" & lag(cor_state)==lag(lag(cor_state))~ pmax(cor_state, lag(cor_state, default = last(cor_state))),
                                middle_weirdo == "middle_weirdo" & cor_state > lead(cor_state) & lead(bug_type)!="bigger_than_lead" & lag(bug_type)!= "smaller_than_lag" & lag(cor_state)==lag(lag(cor_state))~ pmin(cor_state, lead(cor_state, default = last(cor_state))),
                                TRUE~cor_state)) 

# -- > How many inconsistencies after correction?

prob_ids = poopy3 %>% 
  mutate(bugs=case_when(sample_id ==lag(sample_id) & cor2_state < lag(cor2_state)~ "state_improved_w_time", #makes new column which indicates
                        T~"ok")) %>% 
  subset(!bugs=="ok") %>% 
  select(sample_id, bugs) 

nrow(prob_ids)

## There are still 53 inconsistencies in the dataset.

# --> The following code takes state 1 if there is a remaining problem,
#if there are no problems, we keep the corrected state. We also correct
#cases where a state is bigger than the lead value.

poopy3 %<>%
  arrange(sample_id, date_visited) %>%
  group_by(sample_id) %>% 
  mutate(cor3_state = ifelse(row_number() != n() & cor2_state > lead(cor2_state),                                                               
                             pmin(cor2_state, lead(cor2_state, default = last(cor2_state))), 
                             cor2_state))

# -- > How many inconsistencies after correction?

prob_ids=poopy3 %>% 
  mutate(bugs=case_when(sample_id ==lag(sample_id) & cor3_state < lag(cor3_state)~ "state_improved_w_time", #makes new column which indicates
                        T~"ok")) %>% 
  subset(!bugs=="ok") %>% 
  select(sample_id, bugs) 

nrow(prob_ids)

poopy_prob = poopy3 %>% 
  subset(sample_id %in% prob_ids$sample_id) %>% 
  select(sample_id, date_visited, state1, cor3_state)

## There are still 11 inconsistencies in the dataset.

poopy3 %<>% 
  group_by(sample_id) %>% 
  mutate(state3 = case_when(sample_id == "BSO-3" | sample_id == "MSC-15"| sample_id == "MSC-5"| sample_id == "MSO-21" & row_number() != n() & cor3_state > lead(cor3_state)~                                                               
                              pmin(cor3_state, lead(cor3_state, default = last(cor3_state))),
                            sample_id ==  "MSC-25"  ~ cor2_state,
                            sample_id ==  "MSC-7"  & row_number() > 1 & cor3_state < lag(cor3_state) & row_number() != n() ~ lag(cor3_state),
                            sample_id ==  "WWO-16"  & row_number() > 1 & cor3_state < lag(cor3_state) & row_number() != n() ~ lag(cor3_state),
                            sample_id ==  "WWO-17"  & row_number() > 1 & cor3_state < lag(cor3_state) & row_number() != n() ~ lag(cor3_state),
                            sample_id ==  "WWO-22"  & row_number() > 1 & cor3_state < lag(cor3_state) & row_number() != n() ~ lag(cor3_state),
                            sample_id ==  "WWO-6"  & row_number() > 1 & cor3_state < lag(cor3_state) & row_number() != n() ~ lag(cor3_state),
                            TRUE~cor3_state)) 

# -- > How many inconsistencies after correction?

prob_ids= poopy3 %>% 
  mutate(bugs=case_when(sample_id ==lag(sample_id) & state3 < lag(state3)~ "state_improved_w_time", #makes new column which indicates
                        T~"ok")) %>% 
  subset(!bugs=="ok") %>% 
  select(sample_id, bugs) 

nrow(prob_ids)

poopy_prob = poopy3 %>% 
  subset(sample_id %in% prob_ids$sample_id) %>% 
  select(sample_id, date_visited, state1:state3)

## There are still 7 inconsistencies in the dataset.

poopy3 %<>% 
  group_by(sample_id) %>% 
  mutate(state4 = case_when(sample_id ==  "MSC-25"  & row_number() > 1 & state3 < lag(state3) & row_number() != n() ~ lag(state3),
                            sample_id ==  "MSC-7"  & row_number() > 1 & state3 < lag(state3) & row_number() != n() ~ lag(state3),
                            sample_id ==  "WWO-16"  & row_number() > 1 & state3 < lag(state3) & row_number() != n() ~ lag(state3),
                            sample_id ==  "WWO-17" & row_number() > 1 & state3 < lag(state3) & row_number() != n() ~ lag(state3),
                            sample_id ==  "WWO-22"  & row_number() > 1 & state3 < lag(state3) & row_number() != n() ~ lag(state3),
                            sample_id ==  "WWO-6"  & row_number() > 1 & state3 < lag(state3) & row_number() != n() ~ lag(state3),
                            TRUE~state3))


# -- > How many inconsistencies after correction?

prob_ids=poopy3 %>% 
  mutate(bugs=case_when(sample_id ==lag(sample_id) & state4 < lag(state4)~ "state_improved_w_time", #makes new column which indicates
                        T~"ok")) %>% 
  subset(!bugs=="ok") %>% 
  select(sample_id, bugs) 

## There is still 1 inconsistencies in the dataset.

poopy3 %<>% 
  group_by(sample_id) %>% 
  mutate(state4 = case_when(sample_id ==  "MSC-25"  & row_number() > 1 & state4 < lag(state4) & row_number() != n() ~ lag(state4),
                            TRUE~state4))

# -- > How many inconsistencies after correction?

prob_ids = poopy3 %>% 
  mutate(bugs=case_when(sample_id ==lag(sample_id) & state4 < lag(state4)~ "state_improved_w_time", #makes new column which indicates
                        T~"ok")) %>% 
  subset(!bugs=="ok") %>% 
  select(sample_id, bugs) 

nrow(prob_ids)
## There is still 0 inconsistency in the dataset.

## Flag multiple 7 and max state (for censored analysis)
poopy3 %<>%
  arrange(sample_id, date_visited) %>%
  group_by(sample_id) %>%  
  #Creates a column keep or delete based on first 7 and on 1st state max for censored analysis 
  mutate(keep7=if_else(state4 == 7 & state4 == lag(state4)& sample_id==lag(sample_id), "delete", "keep"),
         keep=if_else(state4 == max_state & state4 == lag(state4)& sample_id==lag(sample_id), "delete", "keep"),
         #Creates a state with NAs instead of corrections
         state_NA = if_else(state4 ==  state, state4, NA)) %>% 
  rename(field_state=state)

poopy3 %<>%
  arrange(sample_id, date_visited) %>%
  group_by(sample_id) %>%   
  #Creates a row keep or delete
  mutate(NAs=sum(is.na(state_NA)),
         keepNA=if_else(NAs>=2 & species=="Bighorn sheep", "delete", "keep"))

############################
### EXPORT CLEAN DATASET ###
############################

cox_data_final <- poopy3 %>%   
  filter(keep=="keep") %>% 
  select(sample_id : field_state, state4, state_NA) %>% 
  rename(state = state4)

cox_data_wo_bs <- poopy3 %>%   
  filter(keep=="keep",
         keepNA=="keep") %>% 
  select(sample_id : field_state, state4, state_NA) %>% 
  rename(state = state4)

data_final <- poopy3 %>%   
  filter(keep7=="keep") %>% 
  select(sample_id : field_state, state4, state_NA) %>% 
  rename(state = state4)

data_wo_bs <- poopy3 %>%   
  filter(keep7=="keep",
         keepNA=="keep") %>% 
  select(sample_id : field_state, state4, state_NA) %>% 
  rename(state = state4)

write_rds(cox_data_final, "cox_data_clean.Rds")
write_rds(cox_data_wo_bs, "cox_data_test.Rds")
write_rds(data_final, "data_clean.Rds")
write_rds(data_wo_bs, "data_test.Rds")
