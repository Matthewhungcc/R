# load libraries ----
suppressPackageStartupMessages({
library(extrafont)
library(ggraph)
library(ggridges)
library(pdftools)
library(tidyverse)
library(tidytext)
library(forcats)
library(reshape2)
library(tidyr)
library(igraph)
library(widyr)
library(viridis)}

)

fed_import1 <- pdf_text("https://www.federalreserve.gov/monetarypolicy/files/20180713_mprfullreport.pdf")
str(fed_import1)
substr(fed_import1[7],1,500)
fed_text_raw <- 
  data.frame(text=unlist(strsplit(fed_import1,"\r"))) %>% 
  mutate(report="July2018", 
         line=row_number(),
         text=gsub("\n","",text))
fed_text <- 
  fed_text_raw %>% 
  as_tibble() %>%
  unnest_tokens(word,text)

fed_text2 <- 
  fed_text %>% 
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%
  filter(word != "")

fed_text2  %>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE) 

fed_text2 %>%
  inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE)

fed_bigrams <-   
  fed_text_raw %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  as_tibble()

fed_bigrams

fed_bigrams %>%
  count(bigram, sort = TRUE)
bigrams_separated <- fed_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_filtered
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") 

bigrams_united
bigrams_filtered %>%
  filter(word1 == "gross") %>%
  count( word2, sort = TRUE)
custom_stop_words2 <- 
  bind_rows(tibble(word = c("debt",
                                "gross",
                                "crude",
                                "well",
                                "maturity",
                                "work",
                                "marginally",
                                "leverage"), 
                       lexicon = c("custom")), 
            stop_words)
fed_sentiment <-
  fed_text %>%
  anti_join(custom_stop_words2) %>%
  inner_join(get_sentiments("bing")) %>%
  count(report, index = line %/% 1.5, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

fed_sentiment
ggplot(fed_sentiment,  aes(index, sentiment, fill = sentiment>0)) +
  geom_col(show.legend = FALSE) +
    scale_fill_manual(values=c("red","#27408b"))+
  facet_wrap(~report, ncol = 5, scales = "free_x")+
  theme_ridges(font_family="Roboto")+
  labs(x="index (approximately 3 pages per unit)",y="sentiment",
       title="Sentiment through Federal Reserve Monetary Policy Report",
       subtitle="customized bing lexicon",
       caption="@lenkiefer\nSource: https://www.federalreserve.gov/monetarypolicy/files/20180713_mprfullreport.pdf")
