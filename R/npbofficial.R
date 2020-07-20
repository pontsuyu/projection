library(tidyverse)
library(rvest)

# スクレイピング
To <- read_html("http://npb.jp/bis/players/all/index.html") %>% 
  html_nodes("#pl_lk_unit") %>% 
  html_nodes("a") %>%
  html_attr("href")
To <- paste0("http://npb.jp/bis/players/all/", To)

tmp <- list()
for (to in 21:length(To)) {
  To2 <- read_html(To[to]) %>%
      html_nodes(".three_column_player") %>%
      html_nodes("a") %>%
      html_attr("href")
  To2 <- paste0("http://npb.jp", To2)
  pb <- txtProgressBar(min = 1, max = length(To2), style = 3)
  for(to2 in 1:length(To2)){
    setTxtProgressBar(pb, value = to2)
    page <- read_html(To2[to2])
    
    name <- page %>% 
      html_node(xpath = "//li[@id='pc_v_name']") %>% 
      html_text() %>% 
      str_squish()
  
    temp <- page %>%
      html_nodes(xpath = "//*[@id='stats_b']") %>%
      html_nodes("table") %>% 
      html_table()
  
    if(length(temp)==0) next
  
    temp[[1]]$name <- name
    tmp <- c(tmp, list(temp[[1]]))
    Sys.sleep(.2)
  }
}
res <- do.call("rbind", tmp) %>% 
  distinct() %>% 
  filter(!is.na(年度))

# 公開用
write.csv(res, "upload/SIGNATE[tsuyupon]_ひろしまQuest2020_4.csv", row.names = F, fileEncoding = "cp932")
# 加工用
write.csv(res, "data/公式年度打撃成績_all.csv", row.names = F, fileEncoding = "cp932")
