#' @import Lahman
#' 
na.zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

age_adjustment <- function(age) {
  if (is.na(age)) {
    1
  } else if (age <= 0) { #johnsbi01 ?
    1
  } else if (age > 29) {
    1 / (1 + 0.003 * (age-29))
  } else if (age < 29) {
    1 + 0.006 * (29-age)
  } else {
    1
  }
}

#' Age adjustment - re
age_adjustment_reciprocal <- function(age) {
  if (is.na(age)) {
    1
  } else if (age <= 0) { #johnsbi01 ?
    1
  } else if (age > 29) {
    1 + 0.003 * (age-29)
  } else if (age<29) {
    1/(1 + 0.006 * (29-age))
  } else {
    1
  }
}

#' Get primary position
#' 
#' Gets primary position based on `Fielding` table from the 
#' `Lahman` package. Uses the categories from the Lahman Fielding table 
#' so all OF positons are collapsed into `OF`. Main use is to filter
#' pitchers before computing seasonal averages batting stats.
#' 
#' @return A data frame containing playerID, yearID, primary position, 
#' and number of games at primary position.
#' @import dplyr
#' @export
#' @examples 
#' primary_pos <- get_primary_pos()
#' primary_pos %>% group_by(POS) %>% summarise(n=n())
get_primary_pos <- function(year=NULL) {
  
  if (is.null(year)) {
    fielding <- Lahman::Fielding
  } else {
    fielding <- Lahman::Fielding %>% filter(yearID==year) 
  }
  
  PrimaryPosition <- fielding %>%
    group_by(playerID, yearID, POS) %>%
    summarise(n.game = sum(G)) %>%
    arrange(playerID, yearID, desc(n.game)) %>%
    mutate(rr=row_number()) %>%
    filter(rr==1) %>%
    select(-rr) %>%
    ungroup()
}

append_previous_years <- function(data, previous_years=3) {
  df_list <- list()
  for (year_offset in 1:previous_years)
    df_list[[year_offset]] <- data %>% mutate(yearID = yearID+year_offset-1)
  
  seasonal_averages <- get_seasonal_batting_ave(data)
  for(idx in 1:previous_years) {
    this_df = df_list[[idx]]
    all_data <- left_join(data, this_df, by = c("playerID", "yearID"), 
                          suffixes = c("",sprintf(".%d", idx)))

    this_sa <- get_seasonal_batting_ave(this_df)
    seasonal_averages <- left_join(seasonal_averages, this_sa,
                                   by="yearID",
                                   suffixes = c("",sprintf(".%d", idx)))
  }
  all_data %>% inner_join(seasonal_averages, by="yearID", suffixes=c("", ".SA"))
}


