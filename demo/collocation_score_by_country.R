library(RKorAPClient)
library(tidyverse)

NODE <- '[tt/l=Ei]' # see https://github.com/KorAP/RKorAPClient/issues/8
COLLOCATES <- c('[tt/l=pellen]', '[tt/l=sch\u00e4len]') # Demos may only contain ASCII characters
COUNTRIES <- c("AT", "BE", "CH", "DE", "IT", "LU")
VC <- "textType=/Zeit.*/ & pubPlaceKey=%s"

df <- expand_grid(node = NODE, collocate = COLLOCATES, country = COUNTRIES) %>%
  mutate(vc = sprintf(VC, country))

g <- KorAPConnection(verbose=TRUE) %>%
  collocationScoreQuery(df$node, df$collocate, df$vc, smoothingConstant = 0) %>%
  bind_cols(df %>% select(country)) %>%
  ggplot(aes(x = country, y = logDice, label = sprintf("(%d)", O), fill = collocate))  +
  geom_col(position="dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust=1.5) +
  labs(title = sprintf("Collocates of '%s' by country of publication.", NODE),
       caption = "(absolute cooccurrence frequencies in parentheses)")

print(g)
