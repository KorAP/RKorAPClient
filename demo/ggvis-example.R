library(RKorAPClient)
library(ggvis)

df <- new("KorAPConnection", verbose=TRUE) %>% frequencyQuery(c("Schlumpf","Dings"), paste("pubDate in", c(2000:2010))) %>% ipm() %>%
  mutate(year=as.numeric(queryStringToLabel(vc)))
a <- df %>%
  ggvis(~year, ~ipm, stroke=~query) %>% # , key := ~webUIRequestUrl) %>%
  layer_points() %>%
#  layer_ribbons(x=~year, y2=~conf.low, y=~conf.high, fill:=~query, opacity:=.2) %>%
  layer_lines() %>%
  add_tooltip(function(x) paste0("year: ", x$year,"<br />ipm: ", format(x$ipm, digits=2)), 'hover') %>%
  add_tooltip(function(x) paste0("<script>window.open('", x$webUIRequestUrl, "', 'korap');</script>"), 'click')
print(a)
