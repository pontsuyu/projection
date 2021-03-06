#' シーズンごとの平均打撃成績
#' 
#' @param data A data frame containing batting stats.
#' @return A data frame containing league averages for batting stats
#' @export
#' 
get_seasonal_batting_ave <- function(data){
  data %>%
    select(-stint, -lgID) %>%
    gather(key, value, -playerID, -yearID, -teamID, -PA) %>%
    mutate(value = as.numeric(value)) %>%
    group_by(key, yearID) %>%
    summarise(lgAv = sum(value, na.rm=TRUE) / sum(PA, na.rm=TRUE)) %>%
    ungroup() %>%
    spread(key, lgAv)
}

#' 選手×年度ごとの打撃成績
#' 
#' Gets stats for batters. Pulls data from Lahman:::battingStats(), 
#' aggregates over stints and appends primary position and age.
#' 
#' @param PrimaryPosition A data frame containing primary position. If not provided, 
#' it will be generated by the function. 
#' @return A data frame containing batting stats, primary position, and age.
#' @seealso \code{\link{get_primary_pos}} \code{\link{combine_batter_stints}}
#' @export
get_seasonal_batting_player <- function(data, PrimaryPosition=NULL) {
  if (is.null(PrimaryPosition))
    PrimaryPosition <- get_primary_pos() # 最も守ったポジションをひもづける
  
  columns_to_sum <- c("G","PA","AB","H","X2B","X3B","HR", "R","RBI",
                      "SB","CS","BB","SO","IBB","HBP","SH","SF","GIDP")
  BattingStats <- data %>% 
    group_by(playerID, yearID) %>% 
    mutate_at(columns_to_sum, sum, na.rm=T) %>% 
    mutate(OB    = OBP * (PA - SH), 
           BIP   = AB - SO - HR + SF, 
           HOBIP = H - HR, 
           OBP   = sum(OB) / sum(PA - SH), 
           SLG   = sum(TB) / sum(AB), 
           BABIP = sum(HOBIP) / sum(BIP)) %>% 
    ungroup() %>%
    filter(stint==1) %>% 
    mutate(X1B = H - X2B - X3B - HR) %>% 
    left_join(PrimaryPosition %>% select(playerID, yearID, POS),
              by=c("playerID", "yearID"))
  BattingStats[which(is.na(BattingStats$POS)),]$POS <- 'DH'
  BattingStats$Age <- BattingStats %>%
    inner_join(Lahman::People %>% select(playerID, birthYear), by="playerID") %>%
    mutate(age = yearID-birthYear) %>%
    pull(age)
  
  return(BattingStats)
}

#' Apply marcels for batters
#' 
#' @param data A data frame with batting stats, including seasonal averages.
#' @param metric A string given the name of the metric to compute projections for, e.g. 'HR'
#' @param age_adjustment A callable to make the age adjustment
#' @param metric_weights An array with the weights to give to the projected stats for the previous seasons. 
#' The ith elemnt is the weight for the season i years previous. The default is the c(5, 4, 3).
#' @param playing_time_weights An array with the weights to be used for projecting playing time
#' 
#' @return A data frame containg Marcel projections for 'metric'. The projection is given the generic
#' name 'proj_value'.
#' @examples
#' a <- get_seasonal_batting_player(data) %>% filter(yearID>=2016) %>% filter(POS!="P")
#' b <- tbl_df(append_previous_years(a, previous_years = 1))
#' mcl <- apply_marcel_batting(b, "HR", age_adjustment)
#' mcl %>% filter(projectedYearID==2004, playerID=='beltrca01')
#' 
#' @seealso \code{\link{apply_marcel_pitching}}, 
#' \code{\link{get_team_projected_batting}}, 
#' \code{\link{export_marcels}}
#' @export
# 成績予測関数
apply_marcel_batting <- function(data, metric, age_adjustment,
                                 metric_weights = c(5,4,3),
                                 playing_time_weights = c(0.5, 0.1, 0)) {
  sw <- sum(metric_weights)
  x_metric <- 0
  x_pa <- 0
  x_av_num <- 0
  x_av_denom <- 0
  proj_pa <- 200
  pebble <- 1e-6
  metric_target_num <- 0
  metric_target_denom <- 0
  
  for (idx in seq_along(metric_weights)) {
    metric_key <- sprintf('%s.%d', metric, idx)
    metric_av_key <- paste0(metric_key, ".SA")
    pa_key <- sprintf('%s.%d', "PA", idx)
    pa <- na.zero(data[[pa_key]])
    sa_value <- na.zero(data[[metric_av_key]])
    
    x_metric <- x_metric + na.zero(data[[metric_key]]) * metric_weights[idx]
    x_pa <- x_pa + pa * metric_weights[idx]
    x_av_num <- x_av_num + (sa_value * metric_weights[idx] * (pa + pebble))
    x_av_denom <- x_av_denom + (metric_weights[idx] * (pa + pebble))
    proj_pa <- proj_pa + playing_time_weights[[idx]] * pa
    metric_target_num <- metric_target_num + (metric_weights[idx] * sa_value)
    metric_target_denom <- metric_target_denom + metric_weights[idx]
  }
  data$age_adj <- sapply(data$Age, age_adjustment)
  
  x_av <- x_av_num / x_av_denom # 打席数調整
  metric_target <- metric_target_num / metric_target_denom
  data.frame(playerID = data$playerID,
             yearID   = data$yearID,
             projectedYearID = data$yearID+1,
             age_adj = data$age_adj,
             x_metric = x_metric,
             x_pa = x_pa, 
             x_av = x_av, 
             proj_pa = proj_pa,
             metric_target = metric_target) %>%
    mutate(num = x_av*100*sw+x_metric, 
           denom = x_pa+100*sw,
           proj_rate_raw = num/denom,
           proj_rate = age_adj*proj_rate_raw,
           proj_value = proj_pa*proj_rate) %>% 
    group_by(yearID) %>% 
    mutate(metric_agg = sum(proj_value)/sum(proj_pa),
           proj_value_floating = proj_value,
           metric_multiplier = ifelse(metric_agg>0, metric_target/metric_agg, 1),
           proj_value=proj_value_floating*metric_multiplier) %>% 
    ungroup()
  
}

