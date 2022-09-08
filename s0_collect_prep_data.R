
library(tidyverse)
library(rtweet)

api_key <- "qwe"
api_secret_key <- "..."
access_token <- "asd"
access_token_secret <- "..."

token <- create_token(
  app = "yxc",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

auth_get()

# collect data ------------------------------------------------------------

rstats <- search_tweets("#rstats",
                        n = 18000, retryonratelimit = TRUE,
                        include_rts = FALSE)
save(rstats, file = "data/rstats.RData")

python <- search_tweets("#python",
                        n = 18000, retryonratelimit = TRUE,
                        include_rts = FALSE)
save(python, file = "data/python.RData")

r_python <- search_tweets("#rstats AND #python",
                          n = 18000, retryonratelimit = TRUE,
                          include_rts = FALSE)
save(r_python, file = "data/r_python.RData")


# prep data ---------------------------------------------------------------

py <- python %>% filter(lang == "en") %>% 
  mutate(text = str_to_lower(text)) %>% 
  filter(!str_detect(text, "#rstats")) %>%
  mutate(r_or_py = "only_py")

R <- rstats %>% filter(lang == "en") %>% 
  mutate(text = str_to_lower(text)) %>% 
  filter(!str_detect(text, "#python")) %>%
  mutate(r_or_py = "only_r")

Rpy <- r_python %>% filter(lang == "en") %>% 
  mutate(text = str_to_lower(text),
         r_or_py = "r_py")

df <- bind_rows(py, R, Rpy)

save(df, file = "data/data.RData")

set.seed(666)
d <- df %>% group_by(r_or_py) %>% sample_n(1000) %>% ungroup()
save(d, file = "data/data_sample.RData")

library(tweetbotornot2)
bot_or_not <- predict_bot(d)
d <- d %>% full_join(bot_or_not, by = c("screen_name" = "screen_name",
                                        "user_id" = "user_id")) 
d <- d %>% mutate(is_bot = as.factor(ifelse(prob_bot > .7, "Bot", "Not_Bot")))
save(d, file = "data/data_sample.RData")

d %>% count(is_bot)
d %>% janitor::tabyl(is_bot, r_or_py)

d %>% select(is_bot, r_or_py) %>%
  group_by(is_bot, r_or_py) %>%
  count()

d %>% ggplot(aes(x = is_bot, fill = r_or_py)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "", 
       y = "",
       title = "#python | #rstats | #python AND #rstats \nhastags used on Twitter",
       subtitle = "grouped by being categorized as bot or not",
       caption = "N = 3000 || 1000 | 1000 | 1000") +
  theme_minimal()
