library(dplyr)
library(mice)
wd ="C:/Users/nghia/OneDrive/Documents/GitHub/covid19-precondition-analysis"
setwd(wd)
  
data = as.data.frame(
  read.csv("covid.csv")
)

data$id = NULL

subset = data[ ,c(3,4,5,8)]
data = data[ , -c(3,4,5,8)]
data = apply(data, 2, as.numeric)

#this function assign 1 as yes and 0 as no
ident <- function(x){
  temp = case_when(
    x == 1 ~ 1, 
    x == 2 ~ 0,
    TRUE ~ 100 #100 is the flag for NULL record
  )
  return(temp)
}

#convert variables to factor
data = as.data.frame(
  apply(data, 2, ident)
  )

#death flag
subset = subset %>% mutate(
  subset,
  death = ifelse(date_died == "9999-99-99", 0, 1))
#time from symptoms to entry
subset$date_symptoms = as.Date(subset$date_symptoms, 
                                format = "%Y-%m-%d")
subset$entry_date = as.Date(subset$entry_date, 
                          format = "%Y-%m-%d")
subset$duration = as.numeric(subset$entry_date - subset$date_symptoms) 

data$id = c(1:nrow(data)) #create id
data = cbind(data, subset) #combine data sets

#deleting unsed variables
data$count = NULL 
data$date_died = NULL
data$date_symptoms = NULL
data$entry_date = NULL
#data$age = NULL
data$pregnancy = NULL
data$contact_other_covid = NULL
data$covid_res = NULL
data$icu = NULL
data$intubed = NULL
#data$duration = NULL
#data$death = NULL
#deleting NULL record
temp = data %>% filter_all(any_vars(. %in% 100)) #select every record with NULL
id = temp$id
data = data[-id, ] #delete record with NULL
data$id = NULL #discard id

#for(i in 1:16){
#  if(i == 14 | i == 16) next
 # data[ ,i] = factor(data[ ,i],
 #                    levels = c("0", "1"))
#}

head(data)
model = glm(data = data,
                   formula = death~.,
                   family = binomial(link = "logit"))

