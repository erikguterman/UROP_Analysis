library(tidyverse)
library(readxl)
library(magrittr)
library(zoo)

##minnesota is nice and easy, the warnings relate to columns we don't use anyway so don't need to pay attention to them
MN_results <-
  dir() %>% str_subset("MN_") %>% lapply(read_excel, sheet = "Results")

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
WI_rowcleaning <- function(df)
{
  if (ncol(df) == 7)
  {
    df <- add_column(df, temp = NA, .after = 5)
  }
  if (ncol(df) == 8)
  {
    df <- add_column(df, temp1 = NA, .after = 5)
  }
  if (ncol(df) == 9)
  {
    df <- add_column(df, temp2 = NA, .after = 5)
  }
  if (ncol(df) == 10)
  {
    df <- add_column(df, temp3 = NA, .after = 5)
  }
  return(df)
}

district_IDs <- paste(rep("District"), 1:8)
years <- as.factor(seq(from = 2012, to = 2018, by = 2))
WI_sheets <- dir() %>% str_subset("WI_")
WI_col_names <-
  c(
    "County",
    "Ward",
    "Total Votes",
    "Republican",
    "Democrat",
    "Independent",
    "Independent 2", 
    "temp",
    "Independent",
    "Scattering", 
    "District"
  )

WI_results <- list()
WI_results <- WI_clean_function(WI_sheets, WI_results)
##adding the district factor separately, for unclear reasons Map won't work within function written above
WI_results <-
  lapply(WI_results, function(x) {
    Map(cbind, x, District = district_IDs)
  })

##cleans dataframes for WI in order to make them the same length (makes it easier to merge later)
##this happens when there's multiple independents tracked
##and then bind all districts together so it's just one df per year

for (i in 1:4)
{
  WI_results[[i]] <- lapply(WI_results[[i]], WI_rowcleaning)
  WI_results[[i]] <- lapply(WI_results[[i]], setNames, nm = WI_col_names)
  WI_results[[i]] <- bind_rows(WI_results[[i]])
  WI_results[[i]]$Year <- years[i]
}

WI_results %<>%
  ##binds into one dataframe, changes the county factor to apply to every line instead of the first one 
  ##and then filters out the rows which are just sums of the county-level data or the totals
  bind_rows() %>% mutate(County = na.locf(County)) %>% filter((!str_detect(County, "Total") &
                                                                           (!str_detect(Ward, "Total"))))

##and now we do the cleaning process for minnesota
for(i in 1:4)
{
  MN_results[[i]]$Year <- years[[i]]
}

temp <- bind_rows(MN_results)
