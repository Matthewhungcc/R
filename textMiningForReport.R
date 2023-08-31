#http://lenkiefer.com/2018/07/29/beige-ian-statistics/
#install.packages("rvest")
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
library(viridis)
library(rvest)})



# Example usage
fed_import1 <- pdf_text("https://www.federalreserve.gov/monetarypolicy/files/20180713_mprfullreport.pdf")

fed_text_raw <- 
  data.frame(text=unlist(strsplit(fed_import1,"\r"))) %>% 
  mutate(report="July2018", 
         line=row_number(),
         text=gsub("\n","",text))
fed_text <- 
  fed_text_raw %>% 
  as_tibble() %>%
  unnest_tokens(word,text)
#################################################################################### sentiment     
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
#####################################################################################
#look into bigrams only
fed_bigrams <-   
  fed_text_raw %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  as_tibble()

fed_bigrams
#cut filter and put it back
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
#one report setiment change
fed_sentiment <-
  fed_text %>%
  anti_join(custom_stop_words2) %>%
  inner_join(get_sentiments("bing")) %>%
  count(report, index = line %/% 2, sentiment) %>%
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


###########################################
extract_text_from_html <- function(url) {
  webpage <- read_html(url)
  paragraphs <- html_nodes(webpage, "p")
  text <- html_text(paragraphs, trim = TRUE)
  combined_text <- paste(text, collapse = " ")
  return(combined_text)
}
get_many_data <- function(urls){
  combined_text <- ""
  for (url in urls){
    text <- extract_text_from_html(url)
    combined_text <- paste(combined_text, text, sep="")
    combined_text <- gsub("Accessible", "", combined_text)
    combined_text <- gsub("version", "", combined_text)
    combined_text <- gsub("Version", "", combined_text)
    combined_text <- gsub("text", "", combined_text)
    combined_text <- gsub("Text", "", combined_text)
  }
  return(combined_text)
}

# fed.links=list(c("https://www.federalreserve.gov/monetarypolicy/mpr_20070718_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20070718_part2.htm"))
# df_fed <- 
#   data.frame(report="Jul2023",stringsAsFactors = FALSE) %>%
#   mutate(text= map(fed.links,get_many_data)) %>% unnest(text) %>% 
#   group_by(report) %>% mutate(page=row_number()) %>%
#   ungroup() %>% mutate(text=strsplit(text,"\r")) %>% unnest(text) %>% mutate(text=gsub("\n","",text)) %>%
#   group_by(report) %>% mutate(line=row_number()) %>% ungroup() %>% select(report,line,page,text)
# print(df_fed)
# list of reports, comments indicate important events around release of report
fed.links=list(c("https://www.federalreserve.gov/monetarypolicy/2023-06-mpr-summary.htm", "https://www.federalreserve.gov/monetarypolicy/2023-06-mpr-part1.htm", "https://www.federalreserve.gov/monetarypolicy/2023-06-mpr-part2.htm", "https://www.federalreserve.gov/monetarypolicy/2023-06-mpr-part3.htm"),  
            c("https://www.federalreserve.gov/monetarypolicy/2022-06-mpr-summary.htm", "https://www.federalreserve.gov/monetarypolicy/2022-06-mpr-part1.htm", "https://www.federalreserve.gov/monetarypolicy/2022-06-mpr-part2.htm", "https://www.federalreserve.gov/monetarypolicy/2022-06-mpr-part3.htm"),  
            c("https://www.federalreserve.gov/monetarypolicy/2021-07-mpr-summary.htm", "https://www.federalreserve.gov/monetarypolicy/2021-07-mpr-part1.htm", "https://www.federalreserve.gov/monetarypolicy/2021-07-mpr-part2.htm", "https://www.federalreserve.gov/monetarypolicy/2021-07-mpr-part3.htm"),  
            c("https://www.federalreserve.gov/monetarypolicy/2020-06-mpr-summary.htm", "https://www.federalreserve.gov/monetarypolicy/2020-06-mpr-part1.htm", "https://www.federalreserve.gov/monetarypolicy/2020-06-mpr-part2.htm", "https://www.federalreserve.gov/monetarypolicy/2020-06-mpr-part3.htm"),  
            c("https://www.federalreserve.gov/monetarypolicy/2019-07-mpr-summary.htm", "https://www.federalreserve.gov/monetarypolicy/2019-07-mpr-part1.htm", "https://www.federalreserve.gov/monetarypolicy/2019-07-mpr-part2.htm", "https://www.federalreserve.gov/monetarypolicy/2019-07-mpr-part3.htm"),  
            c("https://www.federalreserve.gov/monetarypolicy/2018-07-mpr-summary.htm", "https://www.federalreserve.gov/monetarypolicy/2018-07-mpr-part1.htm", "https://www.federalreserve.gov/monetarypolicy/2018-07-mpr-part2.htm", "https://www.federalreserve.gov/monetarypolicy/2018-07-mpr-part3.htm"),  
            c("https://www.federalreserve.gov/monetarypolicy/2017-07-mpr-summary.htm", "https://www.federalreserve.gov/monetarypolicy/2017-07-mpr-part1.htm", "https://www.federalreserve.gov/monetarypolicy/2017-07-mpr-part2.htm", "https://www.federalreserve.gov/monetarypolicy/2017-07-mpr-part3.htm"), 
            c("https://www.federalreserve.gov/monetarypolicy/mpr_2016_0621_summary.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20160621_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20160621_part2.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20160621_part3.htm"),            # released in jun 2016, but we'll label it July
            c("https://www.federalreserve.gov/monetarypolicy/mpr_20150715_summary.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20150715_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20150715_part2.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20150715_part3.htm"),         # July 2015  ( before lift off)
            c("https://www.federalreserve.gov/monetarypolicy/mpr_20140715_summary.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20140715_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20140715_part2.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20140715_part3.htm"), 
            c("https://www.federalreserve.gov/monetarypolicy/mpr_20130717_summary.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20130717_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20130717_part2.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20130717_part3.htm"),           # July 2013  ( after Taper Tantrum)
            c("https://www.federalreserve.gov/monetarypolicy/mpr_20120717_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20120717_part2.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20120717_part3.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20120717_part4.htm"), 
            c("https://www.federalreserve.gov/monetarypolicy/mpr_20110713_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20110713_part2.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20110713_part3.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20110713_part4.htm"),           # July 2011  ( early recovery)
            c("https://www.federalreserve.gov/monetarypolicy/mpr_20100721_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20100721_part2.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20100721_part3.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20100721_part4.htm"), 
            c("https://www.federalreserve.gov/monetarypolicy/mpr_20090721_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20090721_part2.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20090721_part3.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20090721_part4.htm"),            # July 2009  ( end of Great Recession)
            c("https://www.federalreserve.gov/monetarypolicy/mpr_20080715_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20080715_part2.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20080715_part3.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20080715_part4.htm"), 
            c("https://www.federalreserve.gov/monetarypolicy/mpr_20070718_part1.htm", "https://www.federalreserve.gov/monetarypolicy/mpr_20070718_part2.htm"),       # July 2007  ( eve of  Great Recession)
            c("https://www.federalreserve.gov/boarddocs/hh/2006/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/2006/july/Reportsection2.htm"),
            c("https://www.federalreserve.gov/boarddocs/hh/2005/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/2005/july/Reportsection2.htm"),                      # July 2005  ( housing boom)
            c("https://www.federalreserve.gov/boarddocs/hh/2004/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/2004/july/Reportsection2.htm"),
            c("https://www.federalreserve.gov/boarddocs/hh/2003/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/2003/july/Reportsection2.htm") ,                     # July 2003  ( deflation fears)
            c("https://www.federalreserve.gov/boarddocs/hh/2002/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/2002/july/Reportsection2.htm"),
            c("https://www.federalreserve.gov/boarddocs/hh/2001/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/2001/july/Reportsection2.htm"),                      # July 2001  ( dot come Recession)
            c("https://www.federalreserve.gov/boarddocs/hh/2000/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/2000/july/Reportsection2.htm"),
            c("https://www.federalreserve.gov/boarddocs/hh/1999/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/1999/july/Reportsection2.htm"),                      # July 1999  ( eve of dotcom Recession)
            c("https://www.federalreserve.gov/boarddocs/hh/1998/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/1998/july/Reportsection2.htm"),
            c("https://www.federalreserve.gov/boarddocs/hh/1997/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/1997/july/Reportsection2.htm"),                       # July 1997 ( irrational exhuberance)
            c("https://www.federalreserve.gov/boarddocs/hh/1996/july/Reportsection1.htm", "https://www.federalreserve.gov/boarddocs/hh/1996/july/Reportsection2.htm")
            )


df_fed <- 
  data.frame(report=c("Jul2023",paste0("Jul",seq(2022,1996,-1))),stringsAsFactors = FALSE) %>%
  mutate(text= map(fed.links,get_many_data)) %>% unnest(text) %>% 
  group_by(report) %>% mutate(page=row_number()) %>%
  ungroup() %>% mutate(text=strsplit(text,"\r")) %>% unnest(text) %>% mutate(text=gsub("\n","",text)) %>%
  group_by(report) %>% mutate(line=row_number()) %>% ungroup() %>% select(report,line,page,text)

fed_words <- df_fed %>%
  unnest_tokens(word, text) %>%
  count(report, word, sort = TRUE) %>%
  ungroup()

total_words <- fed_words %>% 
  group_by(report) %>% 
  summarize(total = sum(n))


# total words per report

ggplot(data=total_words, aes(x=seq(1996,2023),y=total))+
  geom_line(color="#27408b")+
  geom_point(shape=29,fill="white",color="#27408b",size=3,stroke=1.1)+
  scale_y_continuous(labels=scales::comma)+
  theme_ridges(font_family="Roboto")+
  labs(x="year",y="number of words",
       title="Number of words in Federal Reserve Monetary Policy Report",
       subtitle="July of each year 1996-2018",
       caption="@lenkiefer Source: Federal Reserve Board Monetary Policy Reports")

fed_text <- 
  df_fed %>% 
  select(report,page,line,text) %>%
  unnest_tokens(word,text)

fed_text %>% 
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%  # keep only letters (drop numbers and special symbols)
  filter(word != "") %>%
  anti_join(stop_words) %>%
  group_by(report) %>%
  count(word,sort=TRUE) %>% 
  mutate(rank=row_number()) %>%
  ungroup() %>% 
  arrange(rank,report) %>%
  filter(rank<11) %>% 
  ggplot(aes(y=n,x=fct_reorder(word,n))) +
  geom_col(fill="#27408b")+
  facet_wrap(~report,scales="free", ncol=5)+
  coord_flip()+
  theme_ridges(font_family="Roboto", font_size=10)+
  labs(x="",y="",
       title="Most Frequent Words Federal Reserve Monetary Policy Report",
       subtitle="Excluding stop words and numbers.",
       caption="@lenkiefer Source: Federal Reserve Board Monetary Policy Reports")

##############################################################################
# Custom stop words 
custom_stop_words <- 
  bind_rows(tibble(word = c(tolower(month.abb), "one","two","three","four","five","six",
                                "seven","eight","nine","ten","eleven","twelve","mam","ered",
                                "produc","ing","quar","ters","sug","quar",'fmam',"sug",
                                "cient","thirty","pter", "https", "pp", "ui",
                                "pants","ter","ening","ances","www.federalreserve.gov",
                                "tion","fig","ure","figure","src"), 
                       lexicon = c("custom")), 
            stop_words)


fed_textb <- 
  fed_text %>%

  mutate(word = gsub("[^A-Za-z ]","",word)) %>%  # keep only letters (drop numbers and special symbols)
  filter(word != "") %>%
  count(report,word,sort=TRUE) %>%
  bind_tf_idf(word, report, n) %>%
  arrange(desc(tf_idf))

fed_textb %>% 
    anti_join(custom_stop_words, by="word") %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(report) %>%
  mutate(id=row_number()) %>%
  ungroup() %>%
  filter(id<11) %>%
  ggplot(aes(word, tf_idf, fill = report)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~report,scales="free", ncol=5)+
  coord_flip()+
  theme_ridges(font_family="Roboto", font_size=10)+
  theme(axis.text.x=element_blank())+
  labs(x="",y ="tf-idf",
       title="Highest tf-idf words in each Federal Reserve Monetary Policy Report: 1996-2018",
       subtitle="Top 10 terms by tf-idf statistic: term frequncy and inverse document frequency",
       caption="@lenkiefer Source: Federal Reserve Board Monetary Policy Reports\nNote: omits stop words, date abbreviations and numbers.")
################################################################
print(fed_text)
fed_sentiment <-
  fed_text %>%
  anti_join(custom_stop_words2) %>%
  inner_join(get_sentiments("bing")) %>%
  count(report, index = line %/% 5, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
## Joining, by = "word"
## Joining, by = "word"
ggplot(fed_sentiment,  aes(index, sentiment, fill = sentiment>0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=c("red","#27408b"))+
  facet_wrap(~report, ncol = 5, scales = "free_x")+
  theme_ridges(font_family="Roboto")+
  labs(x="index (approximately 3 pages per unit)",y="sentiment",
       title="Sentiment through Federal Reserve Monetary Policy Report",
       subtitle="customized bing lexicon",
       caption="@lenkiefer Source: Federal Reserve Board Monetary Policy Reports")
###############################################################################################
fed_sentiment2 <-
  fed_text %>%
  anti_join(custom_stop_words2) %>%
  inner_join(get_sentiments("bing")) %>%
  count(report, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
## Joining, by = "word"
## Joining, by = "word"
ggplot(fed_sentiment2,  aes(factor(1996:2023), sentiment/(negative+positive), fill = sentiment)) +
  geom_col(show.legend = FALSE) +scale_fill_viridis_c(option="C")+
    theme_ridges(font_family="Roboto",font_size=10)+
  labs(x="report for July of each year",y="Sentiment (>0 positive, <0 negtaive)",
       title="Sentiment of Federal Reserve Monetary Policy Report: 1996-2018",
       subtitle="customized bing lexicon",
       caption="@lenkiefer Source: Federal Reserve Board Monetary Policy Reports")
#############################################################################################

word_cors <- 
  fed_text2 %>% 
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  filter(!word %in% stop_words$word) %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors %>%
  filter(correlation > .10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color ="#27408b", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void(base_family="Roboto")+
  labs(title="  Pairs of words in Federal Reserve Monetary Policy Reports that show at\n  least a 0.15 correlation of appearing within the same 10-line section", caption="  @lenkiefer Source: July Federal Reserve Board Monetary Policy Reports 1996-2018    \n")

