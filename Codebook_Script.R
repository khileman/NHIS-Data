getwd()
setwd('/Users/kierstenhileman/Desktop/Data Analysis')
list.files()
library(tidyverse)
library(dplyr)
library(scales)
library(tidyverse)
library(magrittr)
install.packages('fastmatch')
library(fastmatch)
library(lubridate)
library(kableExtra)

#The two files the general survey data as well as the income computational data
NHIS_21 <- read.csv('adult21.csv')
INC_21 <- read.csv('adultinc21.csv')

#The large survey data contains 622 variables. Using fmatch, I was identifying where data was via column numnber to transform the data into a smaller data set
fmatch("HHX", names(NHIS_21))
fmatch("SRVY_YR", names(NHIS_21))
fmatch("REGION", names(NHIS_21))
fmatch("INTV_MON", names(NHIS_21))
fmatch("AGEP_A", names(NHIS_21))
fmatch("EDUCP_A", names(NHIS_21))
fmatch("PHSTAT_A", names(NHIS_21))
fmatch("MHRX_A", names(NHIS_21))
fmatch("IMPNUM_A", names(NHIS_21))
fmatch("HOUTENURE_A", names(NHIS_21))

#Creating new data set with fewer variables
df2 <-  NHIS_21 %>% 
  select(621, 184, 182, 617, 186, 111, 612, 327, 190)

#Joining the two datasets by survey ID which is coded as "HHX"
df3 <-full_join(df2, INC_21,
                by = join_by('HHX'),
                multiple = 'all')

#renaming columns to corresponding with the question
colnames(df3) <- c('Survey ID', 'Year', 'Region', 'Month', 'Age', 'Highest Education Level',
                   'Health Status', 'Mental Health Medications', 'Property Status', 'Family Income to Poverty Ratio', 'Household Income Imputation', 
                   'Income Imputation Flag', 'Record Type', 'Family Poverty Ratio')

# recording variable as factor variables
df3$Month <- as.factor(df3$Month)
df3$`Highest Education Level` <- as.factor(df3$`Highest Education Level`)
df3$`Health Status` <- as.factor(df3$`Health Status`)
df3$`Mental Health Medications` <- as.factor(df3$`Mental Health Medications`)
df3$`Property Status` <- as.factor(df3$`Property Status`)


#Standardizing missing data to 3 identifying to then conver to NAs
df3$`Mental Health Medications` <- replace(df3$`Mental Health Medications`, df3$`Mental Health Medications` == 7, 97)
df3$`Mental Health Medications` <- replace(df3$`Mental Health Medications`, df3$`Mental Health Medications` == 8, 98)
df3$`Mental Health Medications` <- replace(df3$`Mental Health Medications`, df3$`Mental Health Medications` == 9, 99)
df3$`Health Status` <- replace(df3$`Health Status`, df3$`Health Status` == 8, 98)
df3$`Health Status` <- replace(df3$`Health Status`, df3$`Health Status` == 9, 99)
df3$`Health Status` <- replace(df3$`Health Status`, df3$`Health Status` == 7, 97)
df3$`Property Status` <- replace(df3$`Property Status`, df3$`Property Status` == 7, 97)
df3$`Property Status` <- replace(df3$`Property Status`, df3$`Property Status` == 8, 98)
df3$`Property Status` <- replace(df3$`Property Status`, df3$`Property Status` == 9, 99)

df3 [df3 == 97] <- NA
df3 [df3 == 98] <- NA
df3 [df3 == 99] <- NA

#Renaming the data for region as there are only 4 and it is easy enough to see
df3$Month <- month(df3$Month, label = TRUE)
df3$Region <- replace(df3$Region, df3$Region ==1, 'Northeast')
df3$Region <- replace(df3$Region, df3$Region ==2, 'Midwest')
df3$Region <- replace(df3$Region, df3$Region ==3, 'South')
df3$Region <- replace(df3$Region, df3$Region ==4, 'West')

#Omiting all NA's without removing the columns containing missing data because it would negate large portions of the dataset
df4 <-na.omit(df3)

#Because the second dataset is computational, each survey ID is duplicated 10 times so removing the duplicates here
df5 <-df3[!duplicated(df3$`Survey ID`), ]

df6 <- df5 %>% 
  select(1, 3, 4, 5, 6, 7, 8, 9, 11, 14, 10)


#Creating an equation to build frequency tables for a codebook
cbfactor = function(.data, x){
  x = enquo(x)
  count(.data, !!x) %>%
    mutate(
      values = as.numeric(!!x),
      labels = as_factor(!!x),
      freq = n,
      perc = n/sum(n)*100,
      .keep = 'unused'
    ) %>%
    knitr::kable(format = 'pipe', digits = 1L)
}

#Utilizing the equation to get frequency tables for the codebook 

count(df6$Month)
cbfactor(df6, Region)
cbfactor(df6, Month)
cbfactor(df6, Age)
cbfactor(df6, `Highest Education Level`)
cbfactor(df6, `Health Status`)
cbfactor(df6, `Mental Health Medications`)
cbfactor(df6, `Property Status`)
cbfactor(df6, `Family Income to Poverty Ratio`)
cbfactor(df6, `Household Income Imputation`)

save(df6, file = "K_HILEMAN_DATA.Rdata")
write.csv(df6, '/Users/kierstenhileman/Desktop/Data Analysis/NHIS_2021_Data.csv')

