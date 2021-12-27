# Machine Learning 1

#### Settings ####
getwd()

library("magrittr")
library("data.table")
library("dplyr")
library("lubridate")
library("caret")


#### Loading, joining and preparing data ####
# source : https://www.kaggle.com/rikdifos/credit-card-approval-prediction
# last update: 20-03-24
df.ar <- fread("data/application_record.csv", stringsAsFactors = TRUE)
df.cr <- fread("data/credit_record.csv", stringsAsFactors = TRUE)

str(df.ar)
# [NOTE FOR RMD] Convert variables to factors: ID, FLAG_x, CNT_x; calculate age and work in years
head(df.ar)
# [NOTE FOR RMD] OCCUPATION_TYPE blank => replace with NA

str(df.cr)
# [NOTE FOR RMD] Convert ID to factor
head(df.cr)
# [NOTE FOR RMD] Add explanation for interpretation of MONTHS_BALANCE, STATUS columns

# De-dupe df.ar records, removing all IDs that are duplicated
# df.ar$ID %>% unique() %>% length()                                           # 438510 unique IDs
duplicates <- df.ar %>% group_by(ID) %>% summarise(count = n()) %>% filter(count > 1)
df.ar.mod <- df.ar %>% filter(!ID %in% duplicates$ID)
# df.ar.mod$ID %>% unique() %>% length()                                       # 438463 unique IDs

# Extract month and year from months_balance in df.cr
# [NOTE FOR RMD] months_balance column: 0 = current month, -1 = previous month, -2 = 2 months prior, etc.
# The original dataset was made available on 2020-03-24, so it is presumed current month is March 2020.
# From months_balance we create 2 new columns indicating the month and year for each observation.
df.cr$month_balance <- format(as.Date("2020-03-24") %m+% months(df.cr$MONTHS_BALANCE), "%B")
df.cr$year_balance <- format(as.Date("2020-03-24") %m+% months(df.cr$MONTHS_BALANCE), "%Y") %>% as.integer(.)

# Remove records older than 2 years
df.cr.mod <- df.cr %>%
  filter(MONTHS_BALANCE > -24)

# cnt_status_good = status 0, 1, C
# i.e. paid off in under 60 days
df.status <- df.cr.mod %>%
  group_by(ID) %>%
  summarise(cnt_status_good = length(STATUS[STATUS==0|STATUS==1|STATUS=='C']), 
            cnt_status_bad = n() - cnt_status_good, 
            percent_good = round(cnt_status_good / n() * 100),
            percent_bad = round(cnt_status_bad / n() * 100)) %>%
  mutate(
    customer_status_good = ifelse(percent_good >= 50, 1, 0),
    customer_status_bad = ifelse(percent_good < 50, 1, 0)
  ) %>%
  select(ID, customer_status_good)

df.status$customer_status_good <- as.factor(df.status$customer_status_good)

# Join datasets where ID match exists
df.cc.raw <- inner_join(x = df.status, y = df.ar.mod, by = "ID") %>%
  mutate(ID = as.factor(ID),
         CNT_CHILDREN = as.integer(CNT_CHILDREN),
         CNT_FAM_MEMBERS = as.integer(CNT_FAM_MEMBERS),
         age_years = as.numeric(round(DAYS_BIRTH * (-1) / 365.25), 0),         # instead of DAYS_BIRTH
         work_years = as.numeric(round((DAYS_EMPLOYED * (-1) / 365.25), 0)),   # instead of DAYS_EMPLOYED, negative: Pensioner or unemployed
         FLAG_MOBIL = as.factor(FLAG_MOBIL),
         FLAG_WORK_PHONE = as.factor(FLAG_WORK_PHONE),
         FLAG_PHONE = as.factor(FLAG_PHONE),
         FLAG_EMAIL = as.factor(FLAG_EMAIL))

# Change column names to lower case
setnames(df.cc.raw, tolower(names(df.cc.raw)))

# Look at the factor levels
df.cc.raw %>% 
  select(-id) %>% 
  select_if(is.factor) %>%
  sapply(., FUN = levels)                                                      # alternatively: FUN=table

# flag_mobil = 1 for all IDs => exclude from final dataset

# occupation_type has "" level => replace with NA
levels(df.cc.raw$occupation_type)[levels(df.cc.raw$occupation_type)==""] <- "NA"
# df.cc.raw$occupation_type %>% table()                                        # check that "" are updated as NA

# Set order of levels in education type
df.cc.raw$name_education_type <- factor(df.cc.raw$name_education_type, levels=c("Lower secondary", "Secondary / secondary special", "Incomplete higher", "Higher education", "Academic degree"), ordered = TRUE)

# Convert -ve values to NA
df.cc.raw$work_years[df.cc.raw$work_years == '-1000'] <- NA

# Selecting columns
# [NOTE FOR RMD] months_balance, days_birth, days_employed and flag_mobil columns are excluded
df.cc <- df.cc.raw %>%
  select(id,
         code_gender,
         age_years,
         name_education_type,
         name_income_type,
         name_housing_type,
         name_family_status,
         cnt_children,
         cnt_fam_members,
         flag_own_car,
         flag_own_realty,
         work_years,
         amt_income_total,
         occupation_type,
         flag_work_phone,
         flag_phone,
         flag_email,
         customer_status_good)

# Meta information from df.cc
nlevels(df.cc$id)
str(df.cc)
head(df.cc)
