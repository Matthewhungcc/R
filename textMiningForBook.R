#install.packages(c("extrafont", "ggraph", "ggridges", "pdftools", "tidyverse", "tidytext", "forcats", "reshape2", "tidyr", "igraph", "widyr", "lubridate", "ggrepel", "viridis"))
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
library(lubridate)
library(ggrepel)
library(viridis)}
)
# get all data ----
# links to pdf ----
beige.links.all<-
  tibble::tribble(
    ~url,   ~report, ~report.date,
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20220112.pdf", 20220112L, "2022-01-12",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20220302.pdf", 20220302L, "2022-03-02",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20220420.pdf", 20220420L, "2022-04-20",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20220601.pdf", 20220601L, "2022-06-01",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20220713.pdf", 20220713L, "2022-07-13",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20220907.pdf", 20220907L, "2022-09-07",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20221019.pdf", 20221019L, "2022-10-19",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20221130.pdf", 20221130L, "2022-11-30",

    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20210113.pdf", 20210113L, "2021-01-13",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20210303.pdf", 20210303L, "2021-03-03",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20210414.pdf", 20210414L, "2021-04-14",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20210602.pdf", 20210602L, "2021-06-02",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20210714.pdf", 20210714L, "2021-07-14",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20210908.pdf", 20210908L, "2021-09-08",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20211020.pdf", 20211020L, "2021-10-20",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20211201.pdf", 20211201L, "2021-12-01",

    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20200115.pdf", 20200115L, "2020-01-15",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20200304.pdf", 20200304L, "2020-03-04",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20200415.pdf", 20200415L, "2020-04-15",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20200527.pdf", 20200527L, "2020-05-27",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20200715.pdf", 20200715L, "2020-07-15",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20200902.pdf", 20200902L, "2020-09-02",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20201021.pdf", 20201021L, "2020-10-21",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20201202.pdf", 20201202L, "2020-12-02",

    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20190116.pdf", 20190116L, "2019-01-16",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20190306.pdf", 20190306L, "2019-03-06",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20190417.pdf", 20190417L, "2019-04-17",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20190605.pdf", 20190605L, "2019-06-05",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20190717.pdf", 20190717L, "2019-07-17",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20190904.pdf", 20190904L, "2019-09-04",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20191016.pdf", 20191016L, "2019-10-16",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20191127.pdf", 20191127L, "2019-11-27",

    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20180117.pdf", 20180117L, "2018-01-17",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20180307.pdf", 20180307L, "2018-03-07",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20180418.pdf", 20180418L, "2018-04-18",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20180530.pdf", 20180530L, "2018-05-30",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20180718.pdf", 20180718L, "2018-07-18",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20180912.pdf", 20180912L, "2018-09-12",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20181024.pdf", 20181024L, "2018-10-24",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20181205.pdf", 20181205L, "2018-12-05",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20170118.pdf", 20170118L, "2017-01-18",
    "https://www.federalreserve.gov/monetarypolicy/files/Beigebook_20170301.pdf", 20170301L, "2017-03-01",
    "https://www.federalreserve.gov/monetarypolicy/files/Beigebook_20170419.pdf", 20170419L, "2017-04-19",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20170531.pdf", 20170531L, "2017-05-31",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20170712.pdf", 20170712L, "2017-07-12",
    "https://www.federalreserve.gov/monetarypolicy/files/Beigebook_20170906.pdf", 20170906L, "2017-09-06",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20171018.pdf", 20171018L, "2017-10-18",
    "https://www.federalreserve.gov/monetarypolicy/files/BeigeBook_20171129.pdf", 20171129L, "2017-11-29",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20160113.pdf", 20160113L, "2016-01-13",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20160302.pdf", 20160302L, "2016-03-02",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20160413.pdf", 20160413L, "2016-04-13",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20160601.pdf", 20160601L, "2016-06-01",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20160713.pdf", 20160713L, "2016-07-13",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20160907.pdf", 20160907L, "2016-09-07",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20161019.pdf", 20161019L, "2016-10-19",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20161130.pdf", 20161130L, "2016-11-30",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20150114.pdf", 20150114L, "2015-01-14",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20150304.pdf", 20150304L, "2015-03-04",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20150415.pdf", 20150415L, "2015-04-15",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20150603.pdf", 20150603L, "2015-06-03",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20150715.pdf", 20150715L, "2015-07-15",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20150902.pdf", 20150902L, "2015-09-02",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20151014.pdf", 20151014L, "2015-10-14",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20151202.pdf", 20151202L, "2015-12-02",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20140115.pdf", 20140115L, "2014-01-15",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20140305.pdf", 20140305L, "2014-03-05",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20140416.pdf", 20140416L, "2014-04-16",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20140604.pdf", 20140604L, "2014-06-04",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20140716.pdf", 20140716L, "2014-07-16",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20140903.pdf", 20140903L, "2014-09-03",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20141015.pdf", 20141015L, "2014-10-15",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20141203.pdf", 20141203L, "2014-12-03",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20130116.pdf", 20130116L, "2013-01-16",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20130306.pdf", 20130306L, "2013-03-06",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20130417.pdf", 20130417L, "2013-04-17",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20130605.pdf", 20130605L, "2013-06-05",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20130717.pdf", 20130717L, "2013-07-17",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20130904.pdf", 20130904L, "2013-09-04",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/BeigeBook_20131016.pdf", 20131016L, "2013-10-16",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20131204.pdf", 20131204L, "2013-12-04",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20120111.pdf", 20120111L, "2012-01-11",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20120229.pdf", 20120229L, "2012-02-29",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20120411.pdf", 20120411L, "2012-04-11",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20120606.pdf", 20120606L, "2012-06-06",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20120718.pdf", 20120718L, "2012-07-18",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20120829.pdf", 20120829L, "2012-08-29",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20121010.pdf", 20121010L, "2012-10-10",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/Beigebook_20121128.pdf", 20121128L, "2012-11-28",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20110112.pdf", 20110112L, "2011-01-12",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20110302.pdf", 20110302L, "2011-03-02",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20110413.pdf", 20110413L, "2011-04-13",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20110608.pdf", 20110608L, "2011-06-08",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20110727.pdf", 20110727L, "2011-07-27",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20110907.pdf", 20110907L, "2011-09-07",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20111019.pdf", 20111019L, "2011-10-19",
    "https://www.federalreserve.gov/monetarypolicy/beigebook/files/fullreport20111130.pdf", 20111130L, "2011-11-30",
    "https://www.federalreserve.gov/fomc/beigebook/2010/20100113/fullreport20100113.pdf", 20100113L, "2010-01-13",
    "https://www.federalreserve.gov/fomc/beigebook/2010/20100303/fullreport20100303.pdf", 20100303L, "2010-03-03",
    "https://www.federalreserve.gov/fomc/beigebook/2010/20100414/fullreport20100414.pdf", 20100414L, "2010-04-14",
    "https://www.federalreserve.gov/fomc/beigebook/2010/20100609/fullreport20100609.pdf", 20100609L, "2010-06-09",
    "https://www.federalreserve.gov/fomc/beigebook/2010/20100728/fullreport20100728.pdf", 20100728L, "2010-07-28",
    "https://www.federalreserve.gov/fomc/beigebook/2010/20100908/fullreport20100908.pdf", 20100908L, "2010-09-08",
    "https://www.federalreserve.gov/fomc/beigebook/2010/20101020/fullreport20101020.pdf", 20101020L, "2010-10-20",
    "https://www.federalreserve.gov/fomc/beigebook/2010/20101201/fullreport20101201.pdf", 20101201L, "2010-12-01",
    "https://www.federalreserve.gov/fomc/beigebook/2009/20090114/fullreport20090114.pdf", 20090114L, "2009-01-14",
    "https://www.federalreserve.gov/fomc/beigebook/2009/20090304/fullreport20090304.pdf", 20090304L, "2009-03-04",
    "https://www.federalreserve.gov/fomc/beigebook/2009/20090415/fullreport20090415.pdf", 20090415L, "2009-04-15",
    "https://www.federalreserve.gov/fomc/beigebook/2009/20090610/fullreport20090610.pdf", 20090610L, "2009-06-10",
    "https://www.federalreserve.gov/fomc/beigebook/2009/20090729/fullreport20090729.pdf", 20090729L, "2009-07-29",
    "https://www.federalreserve.gov/fomc/beigebook/2009/20090909/fullreport20090909.pdf", 20090909L, "2009-09-09",
    "https://www.federalreserve.gov/fomc/beigebook/2009/20091021/fullreport20091021.pdf", 20091021L, "2009-10-21",
    "https://www.federalreserve.gov/fomc/beigebook/2009/20091202/fullreport20091202.pdf", 20091202L, "2009-12-02",
    "https://www.federalreserve.gov/fomc/beigebook/2008/20080305/fullreport20080305.pdf", 20080305L, "2008-03-05",
    "https://www.federalreserve.gov/fomc/beigebook/2008/20080416/fullreport20080416.pdf", 20080416L, "2008-04-16",
    "https://www.federalreserve.gov/fomc/beigebook/2008/20080611/fullreport20080611.pdf", 20080611L, "2008-06-11",
    "https://www.federalreserve.gov/fomc/beigebook/2008/20080723/fullreport20080723.pdf", 20080723L, "2008-07-23",
    "https://www.federalreserve.gov/fomc/beigebook/2008/20080903/fullreport20080903.pdf", 20080903L, "2008-09-03",
    "https://www.federalreserve.gov/fomc/beigebook/2008/20081015/fullreport20081015.pdf", 20081015L, "2008-10-15",
    "https://www.federalreserve.gov/fomc/beigebook/2008/20081203/fullreport20081203.pdf", 20081203L, "2008-12-03"
  )

# get data ----
fed_text_raw <-
  beige.links.all %>%
  mutate(text= map(url,pdf_text))  %>% 
  unnest(text) %>% 
  group_by(report) %>%
  # create a page number indicator
  mutate(page=row_number()) %>% 
  ungroup() 

  fed_text_raw <-
  fed_text_raw %>% 
  mutate(text=strsplit(text,"\r")) %>% unnest(text) %>%
  mutate(
    line=row_number(),
    text=gsub("\n","",text)) %>%
  mutate(ctext =strsplit(text, "\\s\\s+")) %>% unnest(ctext) %>% 
  group_by(report,line) %>% mutate(col.id=row_number()) %>%
  arrange(col.id,line) %>%
  group_by(page,line) %>% 
  distinct(ctext, .keep_all=TRUE) %>%
  mutate(rdate= as.Date(as.character(report),"%Y%m%d")) %>%   #create report data
  ungroup()     

fed_text <- 
  fed_text_raw %>% 
  select(report,rdate,page,line,col.id,ctext) %>%
  as_tibble() %>%
  unnest_tokens(word,ctext)

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
  count(rdate, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(positive+negative))

g1 <-
ggplot(fed_sentiment,  aes(rdate, sentiment, fill = sentiment>0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=c("red","#27408b"))+
  #facet_wrap(~report, ncol = 8, scales = "free_x")+
  theme_ridges(font_family="Roboto")+
  labs(x="report (~8 per year)",y="sentiment",
       title="Sentiment in Federal Reserve Beige Book",
       subtitle="customized bing lexicon\nsentiment = (positive-negative)/(positive+negative)",
       caption="@lenkiefer\nSource: Beige Book March 2008 - July 2018")

print(g1)
custom_stop_words3 <- 
  bind_rows(tibble(word = c(tolower(month.abb), "one","two","three","four","five","six",
                                "seven","eight","nine","ten","eleven","twelve","mam","ered",
                                "produc","ing","quar","ters","sug","quar",'fmam',"sug",
                                "cient","thirty","pter",
                                # new 'words' fragments
                                "ty","manufactur","estly","increas","tinued","transporta",
                                "sc","md","struction","cial","manufac","crease","wva","mercial",
                                "ness","commer","al","indus","dis","creases","ported","idential",
                                "er","es","ers","ii","ued","de","mand","ment","moder","contin",
                                "con","tacts", "manu","ments","construc","creased","busi",
                                "mod","tions","mained","ed","va","nc","tive","ly",
                                "charlottesville","vermont","oregon","antic","condi",
                                "antici","pres","facturing","tial","pro","confi","activi","als",
                                # end new words
                                "pants","ter","ening","ances","www.federalreserve.gov",
                                "tion","fig","ure","figure","src"), 
                       lexicon = c("custom")), 
            stop_words)
fed_text_by_month <- 
  fed_text %>%
  filter(lubridate::month(rdate)==7) %>% 
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%  # keep only letters (drop numbers and special symbols)
  filter(word != "") %>%
  count(report=as.character(rdate,format="%b%Y"),word,sort=TRUE) %>%
  bind_tf_idf(word, report, n) %>%
  arrange(desc(tf_idf))

g2<-
fed_text_by_month %>% 
  anti_join(custom_stop_words3, by="word") %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(report) %>%
  mutate(id=row_number()) %>%
  ungroup() %>%
  filter(id<14) %>%
  ggplot(aes(word, tf_idf, fill = report)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~report,scales="free", ncol=4)+
  coord_flip()+
  theme_ridges(font_family="Roboto", font_size=10)+
  theme(axis.text.x=element_blank())+
  labs(x="",y ="tf-idf",
       title="Highest tf-idf words in July Federal Beige Book: 2008-2018",
       subtitle="Top 10 terms by tf-idf statistic: term frequncy and inverse document frequency",
       caption="@lenkiefer Source: Federal Reserve Board Beige Book \nNote: omits stop words, date abbreviations and numbers.")
print(g2)
fed_text_by_year <- 
  fed_text %>%
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%  # keep only letters (drop numbers and special symbols)
  filter(word != "") %>%
  count(report=format(rdate,"%Y"),word,sort=TRUE) %>%
  bind_tf_idf(word, report, n) %>%
  arrange(desc(tf_idf)) %>%
  anti_join(custom_stop_words3, by="word") %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(report) %>%
  mutate(id=row_number()) %>%
  ungroup() %>%
  filter(id<11)
g3<-
fed_text_by_year %>% 

  ggplot(aes(word, tf_idf, fill = report)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~report,scales="free", ncol=4)+
  coord_flip()+
  theme_ridges(font_family="Roboto", font_size=10)+
  theme(axis.text.x=element_blank())+
  labs(x="",y ="tf-idf",
       title="Highest tf-idf words by year for Federal Beige Book: 2008-2018",
       subtitle="Top 10 terms by tf-idf statistic: term frequncy and inverse document frequency",
       caption="@lenkiefer Source: Federal Reserve Board Beige Book \nNote: omits stop words, date abbreviations and numbers.")
print(g3)
g4 <-
  fed_text_by_year %>% 
  filter(id<6) %>%
  ggplot(aes(x = 1, y = 1, size = tf_idf, label = word, color=tf_idf)) +
  geom_text_repel(segment.size = 0, force = 100,segment.color = 'transparent') +
  scale_size(range = c(2, 15), guide = "none") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  scale_color_viridis_c(option="C",end=0.85 )+
  guides(color=F)+
    theme_ridges(font_family="Roboto", font_size=10)+
  labs(x = '', y = '') +
  facet_wrap(~report)+
  labs(x="",y ="tf-idf",
       title="Highest tf-idf words by year for Federal Beige Book: 2008-2018",
       subtitle="Top 5 terms by tf-idf statistic: term frequncy and inverse document frequency",
       caption="@lenkiefer Source: Federal Reserve Board Beige Book \nNote: omits stop words, date abbreviations and numbers.")
g4



beige_words <- 
  
  fed_text %>% 
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%  # keep only letters (drop numbers and special symbols)
  filter(word != "") %>%
  anti_join(stop_words) %>%
  group_by(rdate) %>%
  count(word,sort=TRUE) %>% 
  mutate(rank=row_number())

g5 <-
beige_words %>%
  filter(word=="inflation") %>%
  ggplot(aes(x=rdate,y=n))+
  geom_line(color="#27408b")+
  geom_point(shape=21,fill="white",color="#27408b",size=3,stroke=1.1)+
  scale_y_continuous(labels=scales::comma)+
  theme_ridges(font_family="Roboto")+
  labs(x="report date",y="number of appearances",
       title='Number of times "inflation" appears in report',
       subtitle="Beige Book Mar 2008-July 2018",
       caption="@lenkiefer Source: Federal Reserve Board Beige Book")
g5


#count words per report
beige_words <- 
  
  fed_text %>% 
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%  # keep only letters (drop numbers and special symbols)
  filter(word != "") %>%
  anti_join(stop_words) %>%
  group_by(rdate) %>%
  count(word,sort=TRUE) %>% 
  mutate(rank=row_number())

g6 <-
beige_words %>%
  filter(word %in% c("inflation","tax","vacancy","shortages")) %>%
  ggplot(aes(x=rdate,y=n))+
  geom_line(color="#27408b")+
  facet_wrap(~word)+
  theme_ridges(font_family="Roboto")+
  labs(x="report date",y="number of appearances",
       title="Number of times term appears in Beige Book",
       subtitle="Beige Book Mar 2008-July 2018",
       caption="@lenkiefer Source: Federal Reserve Board Beige Book")
g6



fed_bigrams <-   
  fed_text_raw %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  as_tibble()

bigrams_separated <- fed_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
# terms associatd with shortgages----

# second word shortage
bigrams_filtered %>%
  filter(word2 == "shortages") %>%
  count( word1, sort = TRUE)

  # first word shortage
bigrams_filtered %>%
  filter(word1 == "shortages") %>%
  count( word2, sort = TRUE)



word_cors <- 
  fed_text %>% 
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%
  filter(word != "") %>%
  filter(report==20180718) %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  filter(!word %in% stop_words$word) %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)



g7 <- 
  word_cors %>%
  filter(correlation > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), edge_colour = "#27408b",show.legend=FALSE) +
  geom_node_point(color ="#27408b", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void(base_family="Roboto")+
  theme(plot.margin = unit(c(rep(.5,4)), "cm"))+
  labs(title="  Pairs of words in Federal Reserve Beige Book that show at\n  least a 0.1 correlation of appearing within the same 10-line section", caption="  @lenkiefer Source: Federal Reserve Board Beige Book July 2018    \n")
g7