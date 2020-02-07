library(tidyverse)
library(readxl)
library(magrittr)

##minnesota is nice and easy, the warnings relate to columns we don't use anyway so don't need to pay attention to them
MN_results <- dir() %>% str_subset("MN_") %>% lapply(read_excel, sheet = "Results") 

WI_clean_function <- function(sheetIDs, list)
{
  ##reads the data into a nested list, where the top level of the list is the election year and the second level is the CD
  for (i in 1:4)
  {
    list[[i]] <- sapply(district_IDs, function(x) {
      read_excel(
        sheetIDs[i],
        sheet = x,
        skip = 9,
        .name_repair = "unique"
        )
    })
  }
  return(list)
}

district_IDs <- paste(rep("District"), 1:8)
WI_sheets <- dir() %>% str_subset("WI_")
WI_col_names <- c("County", "Ward", "Total Votes", "Republican", "Democrat", "temp", "Independent", "Scattering")

WI_results <- list()
WI_results <- WI_clean_function(WI_sheets, WI_results) 
##adding the district factor separately, for unclear reasons Map won't work within function written above
WI_results <- lapply(WI_results, function(x){Map(cbind, x, District = district_IDs)})



##next time, go through each of the nested DFs in WI, change them to a matching number of columns (b/c there's some with 0 INDs, some with 1/2)
##and then change the column names to match in order to allow merging all into one

for(i in 1:8)
{
  if(ncol(WI_results[[i]]) == 7)
  {
    WI_results[[i]] <- add_column(WI_results[[i]], NA, .after = 5)
  }
}

##use this to remove county and total values, in order to avoid double counting
#WI_results  %<>% bind_rows() %>% filter((!str_detect(County, "Total") | is.na(County)) & (!str_detect(Ward, "Total")))


