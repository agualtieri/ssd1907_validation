## JMMI - Last Update: September 2021
rm(list = ls ())

## libraries
library(tidyverse)
library(openxlsx)
library(cluster)
library(cleaninginspectoR)


## load sources
source("./R/check_time.R")
source("./R/data_falsification.R")
source('./R/descriptive_stats.R')
source("./R/median_calculation.R")
source("./R/check_log.R")

## Utilities
`%nin%` <- Negate(`%in%`)

## load inputs
tool <- read.xlsx("./input/trader_tool.xlsx")
raw <- read.xlsx("./input/raw.xlsx")

## load clean dataset and cleaning log
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("./input/JMMI_data.xlsx")

data <- mysheets[["CLEAN"]]
names(data)[names(data) == "_index"] <- "index"
names(data)[names(data) == "_uuid"] <- "uuid"

cleaning.log <- mysheets[["cleaning_log"]]
names(cleaning.log)[names(cleaning.log) == "_uuid"] <- "uuid"


deletions <- mysheets[["feedback_deleted"]]

## Check deletions and cleaning log
deletions$`_uuid` %in% data$uuid ## ok

uuid.not.in.data <- cleaning.log %>% filter(uuid %nin% data$uuid) ## all weird NAs
cleaning.log <- cleaning.log %>% filter(uuid %in% data$uuid)

questions.not.in.data <- cleaning.log %>% filter(question.name %nin% names(data))
cleaning.log <- cleaning.log %>% filter(question.name %in% names(data))

log.i <- check_log(data, cleaning.log) %>% mutate(., check = ifelse(new.value == value_extracted, "Log applied correctly", "Please check log")) %>%
  filter(check == "Please check log")

write.xlsx(log.i, paste0("./output/jmmi_issues with cleaning log_",lubridate::today(),".xlsx"))


## check falsification
false.data <- calculateEnumeratorSimilarity(data, tool, "org", "location")
write.xlsx(false.data, paste0("./output/jmmi_falsificaion issues_",lubridate::today(),".xlsx"))


false.data2 <- calculateDifferences(data, tool) %>% filter(number.different.columns < 5)
write.xlsx(false.data2, paste0("./output/jmmi_similar surveys_",lubridate::today(),".xlsx"))



## check outliers
data <- data %>% mutate(index = 1:nrow(.))

issues <- inspect_all(data) %>% filter(!is.na(index)) %>% mutate(uuid= data[.$index,"uuid",drop=T],
                                                                 area = data[.$index,"location",drop=T])

issues <- semi_join(issues, cleaning.log, "uuid")
write.xlsx(issues, paste0("./output/jmmi_outliers check_",lubridate::today(),".xlsx"))                                       


## check median, min, max
#stats <- descriptive_stats(data, "location", ends_with("_ssp"))

data$location <- as.character(data$location)
price.vec <- select(data, "location", ends_with("_price_unit_ssp"))

sapply(price.vec, class)



median_items <- data %>%                                                                              
                select(location, ends_with("_price_unit_ssp")) %>% 
                group_by(location) %>%    
                mutate_if(., is.character, as.double)%>%
                summarise_all(funs(median(., na.rm = TRUE)))  

write.xlsx(median_items, paste0("./output/jmmi_medians check_",lubridate::today(),".xlsx"))

