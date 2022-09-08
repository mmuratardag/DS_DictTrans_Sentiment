
load("data/data_sample.RData")

R_vader <- vader::vader_df(d$text)
library(tidyverse)
d <- bind_cols(d, R_vader %>% select(R_vader_word_scores = word_scores,
                                     R_vader_compound = compound,
                                     R_vader_pos = pos,
                                     R_vader_neu = neu,
                                     R_vader_neg = neg))

d %>% ggplot(aes(R_vader_compound)) + geom_histogram() + theme_minimal()

d %>% ggplot(aes(x = R_vader_compound, fill = r_or_py)) + 
  geom_density(alpha = .2) + 
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~is_bot) + 
  labs(x = "vader compound scores",
       title = "#python | #rstats | #python AND #rstats \nhastags used on Twitter",
       subtitle = "Sentiment Score Distribution") + 
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

d %>% filter(R_vader_pos == max(R_vader_pos)) %>% select(text)
d %>% filter(r_or_py %in% "only_r") %>% 
  arrange(desc(R_vader_pos)) %>% 
  select(text, R_vader_pos) %>% head(2)
d %>% filter(r_or_py %in% "only_py") %>% 
  arrange(desc(R_vader_pos)) %>% 
  select(text, R_vader_pos) %>% head(2)
d %>% filter(r_or_py %in% "r_py") %>% 
  arrange(desc(R_vader_pos)) %>% 
  select(text, R_vader_pos) %>% head(2)

#............this will run on WinOS but not on Ubuntu............
tw_xlm_ro <- huggingfaceR::hf_load_model("cardiffnlp/twitter-xlm-roberta-base-sentiment")
tw_xlm_ro_sent <- tw_xlm_ro(d$text)
#................................................................

d <- d %>% mutate(tw_xlm_ro_sent = tw_xlm_ro_sent) %>% 
  unnest_wider(tw_xlm_ro_sent) %>%
  rename(hugging_face_label = label, hugging_face_score = score)

d %>% 
  ggplot(aes(hugging_face_score, fill = hugging_face_label)) + 
  geom_density(alpha = .3) + theme_minimal() +
  facet_wrap(hugging_face_label ~ r_or_py) +
  theme_minimal()

save(d, file = "data/data_sample.RData")

d %>% ggplot(aes(hugging_face_score, fill = is_bot)) + geom_density(alpha = .2) + 
  facet_wrap(hugging_face_label ~ r_or_py) +
  theme_minimal()

d %>% ggplot(aes(x = hugging_face_label, fill = r_or_py)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~is_bot) + 
  labs(x = "", 
       y = "",
       title = "#python | #rstats | #python AND #rstats \nhastags used on Twitter",
       subtitle = "HuggingFace Sentiment Classification",
       caption = "model: cardiffnlp/twitter-xlm-roberta-base-sentiment") +
  theme_minimal()

library(patchwork)

pw1 <- d %>% 
  ggplot(aes(x = is_bot, y = R_vader_compound, color = hugging_face_label)) +
  geom_boxplot(width = .1, show.legend = FALSE, outlier.alpha = .3) +
  facet_wrap(hugging_face_label ~ r_or_py) +
  labs(x = "", 
       y = "vader compound scores",
       title = "",
       subtitle = "Distribution visualiazation 1",
       caption = "") + 
  theme_minimal()

pw2 <- d %>%
  ggplot(aes(x = is_bot, y = R_vader_compound, color = hugging_face_label)) + 
  ggdist::stat_halfeye(adjust = .5,
                       alpha = .5,
                       width = .6, 
                       .width = 0, 
                       justification = -.2, 
                       point_colour = NA) + 
  geom_boxplot(width = .1, 
               outlier.shape = NA) +
  gghalves::geom_half_point(side = "l", 
                            range_scale = .4, 
                            alpha = .2) +
  coord_cartesian(xlim = c(1, NA), clip = "off") +
  facet_wrap(hugging_face_label ~ r_or_py) +
  labs(x = "", 
       y = "",
       title = "",
       subtitle = "Distribution visualiazation 2",
       caption = "") + 
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

(pw1 | pw2) +
  plot_annotation(title = "#python | #rstats | #python AND #rstats \nhastags used on Twitter",
                  subtitle = "HF model: cardiffnlp/twitter-xlm-roberta-base-sentiment",
                  caption = "Let's focus on 1. & 2. since they have the most outliers
                  \n1. Bots that have only #rstats and are classified as neutral by HF model
                  \n2. Not_Bots that have both #rstats & #python and are classified as neutral by HF model
                  \nThough, what is also interesting is
                  \n3.1. There is no single Bot that uses both #python & #rstats and is categorized as (-)
                  \n3.2. Only 3 tweets are categorized as (+) when it is a Bot with both #python & #rstats")


# neutral only_r bot ------------------------------------------------------
d %>% filter(hugging_face_label == "Neutral" & 
               r_or_py == "only_r" &
               is_bot == "Bot") %>%
  arrange(desc(R_vader_compound)) %>% select(text, R_vader_compound,
                                             hugging_face_score) %>% head(3)

d %>% filter(hugging_face_label == "Neutral" & 
               r_or_py == "only_r" &
               is_bot == "Bot") %>%
  arrange(desc(R_vader_compound)) %>% select(text) %>% head(1) %>% pull()
# "best books for deep learning \nbest books for deep learning, are you seeking 
# resources for deep https://t.co/bbvcbvuct5 #datascience #rstudio #rstats"

d %>% filter(hugging_face_label == "Neutral" & 
               r_or_py == "only_r" &
               is_bot == "Bot") %>%
  arrange(R_vader_compound) %>% select(text, R_vader_compound,
                                       hugging_face_score) %>% head(3)

d %>% filter(hugging_face_label == "Neutral" & 
               r_or_py == "only_r" &
               is_bot == "Bot") %>%
  arrange(R_vader_compound) %>% select(text) %>% head(1) %>% pull()
# "unemployment update for the 2 largest eu economies: german unemployment is 
# at 2.8% in jun 2022, a decrease of -0.1p.p. unemployment in france at 7.2%, 
# flat relative to  prior month #rstats https://t.co/rfryo4kxa5"



# neutral r_py Not_Bot ----------------------------------------------------
d %>% filter(hugging_face_label == "Neutral" & 
               r_or_py == "r_py" &
               is_bot == "Not_Bot") %>%
  arrange(desc(R_vader_compound)) %>% select(text, R_vader_compound,
                                             hugging_face_score) %>% head(3)

d %>% filter(hugging_face_label == "Neutral" & 
               r_or_py == "r_py" &
               is_bot == "Not_Bot") %>%
  arrange(desc(R_vader_compound)) %>% select(text) %>% head(1) %>% pull()

# "we are #hiring\n☑️like #data #analysis\n☑️ experience w #biological or relate
# d #data\n☑️ like engaging w experimentalists\n🔝 coding #rstats &amp; |
# #python &amp;  stats analyses\n🔅interest in #ml\njob #security🔛 
# #friendly team🔛#flexible working\n\napply by 26th sep. more details? 
# talk to me https://t.co/8tkscovjbj"

d %>% filter(hugging_face_label == "Neutral" & 
               r_or_py == "r_py" &
               is_bot == "Not_Bot") %>%
  arrange(R_vader_compound) %>% select(text, R_vader_compound,
                                       hugging_face_score) %>% head(3)

d %>% filter(hugging_face_label == "Neutral" & 
               r_or_py == "r_py" &
               is_bot == "Not_Bot") %>%
  arrange(R_vader_compound) %>% select(text) %>% head(1) %>% pull()

# "6 ways to combat phishing attacks \n#programminglanguages  #machinelearning  
# #datascience #sql #cybersecurity #bigdata #analytics #ai #iiot #python #rstats 
# #tensorflow #reactjs #cloudcomputing #serverless https://t.co/0yrjspjqri"
