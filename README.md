# text_mining_ghana_budgets
 Mapping Ghana’s investment in Research and Development from budget statements and publication records with R 
#Mapping Ghana’s investment in Research and Development from budget statements and publication records
#By Paul A. Agbodza (June 30, 2022)


#load libraries

library(tidyverse)
library(tm)
library(wordcloud)
library(wordcloud2)
library(pdftools)

#Fig. 1

#research and development expenditure plot africa

library(rvest)

# list of countries by research and development (R&D) spending in real terms and as per latest data available.

world_r_d_wiki <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_research_and_development_spending")%>%
  html_nodes("table")%>%
  .[[2]]%>%html_table()|>
  janitor::clean_names()

#to subset data for african countries
#which countries are african

afric_country <- rvest::read_html("https://en.wikipedia.org/wiki/List_of_African_countries_by_GDP_(PPP)")%>%
  html_nodes("table")%>%
  .[[1]]%>%html_table()

#subset world data to filter for african countries only

afric_wikiGDP <- world_r_d_wiki[world_r_d_wiki$country_region %in% afric_country$Country, ]
names(afric_wikiGDP)[1:5] <- c("Rank", "Country", "R_D_Exp", "Percent_GDP", "USD_perCap")

#code inspired by
#https://www.r-bloggers.com/2017/06/calculate-inflation-with-the-blscraper-package-3/
#and the blscrapeR vignette
#CPI: Tracking Escalation

# use of the CPI is to determine price escalation
#what is the price escalation of gdp expenditure/investment by countries 
#made in specific years by May 2022 value?


#get an API key from   
#https://data.bls.gov/registrationEngine/
#https://www.bls.gov/news.release/cpi.toc.htm


library(blscrapeR)

#wrapper for the Bureau of Labor Statistics (BLS.) 

## Multiple Series request with full params allowed

cpi_2009 <- bls_api("CUSR0000SA0", startyear = 2000, endyear = 2009,
       registrationKey = "my_key")

cpi_2019 <- bls_api("CUSR0000SA0", startyear = 2010, endyear = 2019)

cpi_2022 <- bls_api("CUSR0000SA0", startyear = 2020, endyear = 2022)

cpi_usa <- bind_rows(cpi_2009, cpi_2019, cpi_2022)

#USA inflation
#"Data source: https://fred.stlouisfed.org/series/FPCPITOTLZGUSA"

usa_inflation <- read_csv("~/FPCPITOTLZGUSA.csv")                                              

usa_inflation2 <- usa_inflation|>
  mutate(YEAR = as.integer(lubridate::year(DATE)),
  moving_avg = zoo::rollapplyr(FPCPITOTLZGUSA, 2, 
                               mean, partial = TRUE))


# Computing the updated value of the required invested amount.
(base_value / base_cpi) * new_cpi
#"Expenditures on R&D per capita  (US$ PPP)" 

afric_wikiGDP_up <- afric_wikiGDP|>dplyr::mutate_at("USD_perCap", readr::parse_number)|>
   left_join(usa_inflation2,  afric_wikiGDP, by = c("year"= "YEAR"))|>
   left_join(cpi_usa|>select(year, value), by = "year")|>
   dplyr::select(-c(DATE, source))|>rename(inflat = FPCPITOTLZGUSA)|>
   group_by(year, Country, USD_perCap, Percent_GDP)|>
   summarise(mean_inf = mean(value, na.rm = TRUE))|>
   mutate(disc_RDperCap = (USD_perCap/mean_inf)*291) #new_CPI (2022 May) =  291.


afric_wikiGDP_up|>
  ggplot(aes(reorder(Country, disc_RDperCap), disc_RDperCap))+ 
  geom_col(color = "grey90", alpha = 0.5)+
  geom_text(aes(x = Country, y = disc_RDperCap, 
                label = dollar(round(disc_RDperCap, 2))), 
            vjust = 0.3, hjust= -0.2, size = 3.2)+
  geom_text(aes(x = Country, y = disc_RDperCap, label = year), 
            hjust = 1.5, vjust= -0.5, size = 3.2, color = "brown")+
  coord_flip()+
  papaja::theme_apa(base_size = 14)+
  labs(title = "Research & Development expenditure per capita",
       subtitle = "Select African countries discounted for May 2022 dollar value",
       y = "", x = "",
       caption = "Author's discounted values based on: 
       https://en.wikipedia.org/wiki/List_of_countries_by_research_and_development_spending
       USA inflation from https://fred.stlouisfed.org/series/FPCPITOTLZGUSA,
       USA CPI from blscrapeR in R")




#plot 2

##ghana d and r plot


###  research and development expenditure
r_and_d_index <- readxl::read_excel("~/API_GB.XPD.RSDV.GD.ZS_DS2_en_excel_v2_4151438.xls")
#source WDI

r_and_d_index <- r_and_d_index[-(1:2),]
colnames(r_and_d_index) <- NULL
colnames(r_and_d_index) <- c(r_and_d_index[1,])
r_and_d_index <- r_and_d_index[-1,]
names(r_and_d_index)[1] <- "Country"

#Research and development expenditure (% of GDP)

ghana_randd <- r_and_d_index|>filter(Country == "Ghana")


ghana_randd[,-c(2:4)]|>pivot_longer(-Country)|>
  mutate_at(3, readr::parse_number)|>
  mutate(value = replace_na(value, 0))|>
  filter(name > 1999, name < 2017)|>
ggplot()+
  geom_col(aes(name, value), fill = "grey40")+
  geom_text(data =  data.frame(year = c("2007", "2010"),
                               amount = c(0.23, 0.38)),
    aes(x = year, y = scales::percent(amount), 
              #  label = dollar(round(amount, 2))), 
                label = percent(round(amount, 2))), 
            vjust = -0.4, hjust= 0.3, size = 3.2)+
  labs(title = "Ghana R & D expenditure of GDP",
       caption = "Data source: World Development Indicator",
       y = "percent of GDP", x = "")+
  hrbrthemes::theme_ipsum_pub()



#plot 3 text mining of ghana budgets 1999 - 2022

#download documents Ghana Budget Statements in pdf from

#https://mofep.gov.gh/sites/default/files/budget-statements/
#https://www.mofep.gov.gh/sites/default/files/budget-statements/
#https://www.ghanaweb.com/GhanaHomePage/economy/budget.php


#Getting a list of documents in a folder

file_vector <- list.files("~/your_path_name", pattern = ".pdf$", all.files = TRUE)

length(file_vector) #24

budget_files <- paste0("~/your_path_name", file_vector)

file_title <- paste0("B_", str_extract_all(file_vector, "\\(?[0-9,]+\\)?"))

budget_texts_df <- data.frame(document = file_title,
                              text = sapply(budget_files, function(x) 
                                paste0(pdf_text(x), collapse = ' ')))

rownames(budget_texts_df) <- NULL

#split the text of all year statements into a data frame by chapter.

budget_chapters1 <- budget_texts_df %>%
     group_by(document) %>%
     tidytext::unnest_tokens(chapter, text, token = "regex", 
         pattern = "^Section|SECTION") %>% 
     ungroup()%>%  mutate(rownames_to_column(., "rowID"))


#remove redundant sections eg table of content
#data not well-structured uniformly so it is better to do this manually
#data to be removed here are sections that constitute table of content
#they also begin with the term section
#find their row numbers are remove


budget_chapters <- budget_chapters1[-c(1:7,13:20,27:35,43:54,64:75, 84:92,
                          101:109, 118:128, 139:149, 169, 171:179, 190:198,
                          208:218, 229:240, 252:262, 273:282, 292:301, 
                          311:320, 330:337, 345:352, 360:366, 373:378,
                          385:392, 400:406),]
#rename columns

names(budget_chapters)[1:2] <- c("author", "text")

#some lines of this cleaning part were inspired by stackoverflow pages

#clean data

budget_chapters$text <- str_remove(budget_chapters$text, "^0+")
budget_chapters$text <- str_replace(budget_chapters$text, "^0+" ,"")
budget_chapters$text <- str_remove(budget_chapters$text, "0+$")
budget_chapters$text <- str_replace(budget_chapters$text, "0+$" ,"")
budget_chapters$text <- gsub("(?<![0-9])0+", "", budget_chapters$text, perl = TRUE)
budget_chapters$text <- gsub('[0-9.]', '', budget_chapters$text)
budget_chapters$text <- gsub(" *\\b(?<!-)\\p{L}{1,2}(?!-)\\b *", " ", 
                             budget_chapters$text, perl = TRUE)
budget_chapters$text <- iconv(budget_chapters$text, from="", to="ASCII//TRANSLIT")
budget_chapters$text <- str_replace_all(budget_chapters$text, "\\n", " ")
#budget_chapters$text <- gsub("[[:space:]]", " ", budget_chapters$text)
budget_chapters$text <- trimws(budget_chapters$text, which = "both")
budget_chapters$text <- trimws(budget_chapters$text, which = "both")

#optional writing of file to your machine
write.csv(budget_chapters, "~/your_path_name/budget_chapters.csv",
          row.names = FALSE)


#create a dictionary of words to remove

redundant <- data.frame(sentence = c("minister", "hon", "speaker", "mister", "per", "cent", "cedi", 
            "budget", "first", "use", "end", "madam", "lady", "gentlemen", "motherland", "parliament",
         "also", "percent", "president", "national", "mr", "deputy", "government", "republic", "presented",
        "statement",  "ghana", "honourable", "mister", "gh", "ministry", "million", "will", "thousand",
     "excellency", "chairman", "excellencies", "members", ".", ",", "your", "|", "year", "billion",
     "performance", "sector", "flt", "nana", "billions cedis", "acronyms", "abbreviations", "the"))


stopword_sent <- as_tibble(stopwords::stopwords("en")) 
stopword_sent <- rename(stopword_sent, sentence = value)

#create more stopwords

zahlen <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
            "ten", "zero")

redundant2 <- data.frame(word = c("minister", "hon", "speaker", "mister", "per", "cent", "cedi", 
                    "budget", "first", "use", "end", "madam", "lady", "gentlemen", "motherland", "parliament",
              "also", "percent", "president", "national", "mr", "deputy", "government", "republic", "presented",
         "statement",  "ghana", "honourable", "mister", "gh", "ministry", "million", "will", "thousand",
       "excellency", "chairman", "excellencies", "members", ".", ",", "your", "|", "year", "billions",
       "speech", "sector", "mun", "sessional", "appendix", "table", "flt", "nana", "trillion", "the",
       "introduction", "beg", "move", "house", "approves", "budget", "statement",
       "billion", "billions", "for", "will", "and", "with", "conclusion", "introduction",
       "acronyms", "abbreviations", "theme",  "speaker", "mister", "per", "ghana", "honourable", "mister", 
       "gh", "ministry", "million", "will", "also", "percent", "national", zahlen))



#text mining

#create function

require(tm)

text_transform <- function(dfXT){
  #corpus <-iconv(apple$text, to = "utf-8-mac") #if web-document
  corpus <- Corpus(VectorSource(dfXT))
  #Text transformation
  toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
  corpus.trans <- tm_map(corpus, toSpace, "/")
  corpus.trans <- tm_map(corpus.trans, toSpace, "@")
  corpus.trans <- tm_map(corpus.trans, toSpace, "\\|")
  corpus.trans <- tm_map(corpus.trans, toSpace, "¢")
  corpus.trans <- tm_map(corpus.trans, toSpace, "")
  #Cleaning the text
  corpus.clean <- tm_map(corpus.trans, content_transformer(tolower))
  corpus.clean <- tm_map(corpus.clean, removeNumbers)
  corpus.clean <- tm_map(corpus.clean, removeWords, stopwords("en"))
  corpus.clean <- tm_map(corpus.clean, removeWords, redundant$sentence)
  corpus.clean <- tm_map(corpus.clean, removeWords, redundant2$word)
  corpus.clean <- tm_map(corpus.clean, removePunctuation)
  removeForeign <-  function(x) gsub("http[[:alnum:]]*'","", x)
  corpus.clean <- tm_map(corpus.clean, content_transformer(removeForeign))
  removeForeign2 <-  function(x) gsub(" *\\b(?<!-)\\p{L}{1,2}(?!-)\\b *", " ", 
                                  x, perl = TRUE)
  corpus.clean <- tm_map(corpus.clean, content_transformer(removeForeign2))
  removeForeign3 <-  function(x) iconv(x, from = "", to = "ASCII//TRANSLIT")
  corpus.clean <- tm_map(corpus.clean, content_transformer(removeForeign3))
  corpus.clean <- tm_map(corpus.clean, stripWhitespace)
  TDM <- TermDocumentMatrix(corpus.clean)
  TDM <- removeSparseTerms(TDM, sparse = 0.95)
  DTM <- DocumentTermMatrix(corpus.clean)
  DTM <- removeSparseTerms(DTM, sparse = 0.95)
  matrix_TDM <- as.matrix(TDM)
  rowsum_TDM <- sort(rowSums(matrix_TDM), decreasing = TRUE)
  df_TDM <- data.frame(word = names(rowsum_TDM), freq = rowsum_TDM )
  TDM_transp <- t(TDM)
  return(list(corpus_budget = corpus.clean,
              tdm_budget = TDM,
              dtm_budget = DTM,
              tdmDF_budget = df_TDM,
              tdmTransp_budget = TDM_transp))
}


#text_transform()

budgetMined <- text_transform(budget_chapters$text)

assocWords2022_res <- findAssocs(budgetMined$tdm_budget, 
                                 terms = "research", corlimit = 0.3)

researchDF2 <- as.data.frame(assocWords2022_res)

researchDF2 <- researchDF2 %>% 
  rownames_to_column(., var = "CorrWord")|>
  rename("CorrValue" = "research")

summary(researchDF2$CorrValue)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.3000  0.4600  0.5800  0.5817  0.7000  0.9400


researchDF2 <- researchDF2|>mutate(CorrStatus = case_when(
  CorrValue < 0.4 ~ "Weak_Corr",
  CorrValue >= 0.4 & CorrValue <= 0.7 ~ "ModerateCorr",
  CorrValue > 0.7 ~ "StrongCorr"))

table(researchDF2$CorrStatus)

researchDF2$CorrStatus <- factor(researchDF2$CorrStatus,
                                levels = rev(unique(researchDF$CorrStatus)), 
                                ordered = TRUE)

researchDF2|> filter(CorrValue > 0.87)|>
  ggplot()+
  geom_text(aes(reorder(CorrWord, CorrValue), CorrValue,
                label = CorrWord), 
           stat = "identity", color = "navyblue")+
  labs(#fill = "Corr_Status",
       title = "Most frequent terms in budget 1999-2021 that correlate with RESEARCH",
       y = "Correlation Coefficient", x = "",
       subtitle = "  Text mining of the Ghana Budget Statements")+
  theme(plot.title = element_text(hjust = 0.6, vjust = 2),
        plot.subtitle = element_text(color = "blue"),
        plot.margin = unit(c(2, 0.5, 2, 0.5), "lines"))+ 
  coord_flip()+
  papaja::theme_apa()
  

#####

#code here drew heavily on

#Introduction to tidytext
#Julia Silge and David Robinson
#2022-05-09
#https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
 
#Text Mining in R by Jan Kirenz
#Last updated on Sep 16, 2019
#https://www.kirenz.com/post/2019-09-16-r-text-mining/

#drew heavily on stackoverflow

#Fig 4

##budget chapters already in previous steps

tidy_sentence <- budget_chapters %>%select(-rowID)%>%
  mutate(text = str_replace_all(text, "\\n", " ")) %>% 
  tidytext::unnest_sentences(sentence, text)|> as_tibble() %>% 
  mutate(docID = row_number())

##names(tidy_sentence)[1] <- "author"

tidy_sentence$sentence <- gsub("[[:punct:][:blank:]]+", " ", tidy_sentence$sentence)
tidy_sentence$sentence<- gsub(" *\\b(?<!-)\\p{L}{1,2}(?!-)\\b *", " ", 
                                  tidy_sentence$sentence, perl = TRUE)
tidy_sentence$sentence <- iconv(tidy_sentence$sentence, from="", to="ASCII//TRANSLIT")
tidy_sentence$sentence <- gsub('[^[:alnum:] ]',  ' ', tidy_sentence$sentence)
tidy_sentence$sentence <- trimws(tidy_sentence$sentence, which = "both")

#Removing rows having all zeros −

tidy_sentence$sentence <- str_remove(tidy_sentence$sentence, "^0+")
tidy_sentence$sentence <- str_replace(tidy_sentence$sentence, "^0+" ,"")
tidy_sentence$sentence <- str_remove(tidy_sentence$sentence, "0+$")
tidy_sentence$sentence <- str_replace(tidy_sentence$sentence, "0+$" ,"")
tidy_sentence$sentence <- gsub("(?<![0-9])0+", "", 
                                   tidy_sentence$sentence, perl = TRUE)
tidy_sentence$sentence <- trimws(tidy_sentence$sentence, which = "both")

tidy_sentence$sentence <- gsub('[0-9.]', '', tidy_sentence$sentence)
tidy_sentence$sentence <- gsub("[[:digit:][:punct:]]", "", tidy_sentence$sentence)
tidy_sentence$sentence <- trimws(tidy_sentence$sentence, which = "both")

library(stopwords) 
#library(tibble)

#create a dictionary of words to remove

redundant <- data.frame(sentence = c("minister", "hon", "speaker", "mister", "per", "cent", "cedi", 
                                     "budget", "first", "use", "end", "madam", "lady", "gentlemen", "motherland", "parliament",
                                     "also", "percent", "president", "national", "mr", "deputy", "government", "republic", "presented",
                                     "statement",  "ghana", "honourable", "mister", "gh", "ministry", "million", "will", "thousand",
                                     "excellency", "chairman", "excellencies", "members", ".", ",", "your", "|", "year", "billion",
                                     "performance", "sector", "flt", "nana", "billions cedis", "introduction"))


zahlen <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
            "ten", "zero")

redundant3 <- data.frame(sentence = c("minister", "hon", "speaker", "mister", "per", "cent", "cedi", 
                                  "budget", "first", "use", "end", "madam", "lady", "gentlemen", "motherland", "parliament",
                                  "also", "percent", "president", "national", "mr", "deputy", "government", "republic", "presented",
                                  "statement",  "ghana", "honourable", "mister", "gh", "ministry", "million", "will", "thousand",
                                  "excellency", "chairman", "excellencies", "members", ".", ",", "your", "|", "year", "billions",
                                  "speech", "sector", "mun", "sessional", "appendix", "table", "flt", "nana", "trillion", "the",
                                  "introduction", "speaker", "beg", "move", "house", "approves", "budget", "statement",
                                  "billion", "billions", "for", "will", "and", "with", "conclusion", "introduction",
                                  "acronyms", "abbreviations", "theme", "minister", "hon", "speaker", "mister", "per", 
                                  "cent", "cedi", "budget", "first", "use", "end", "madam", "lady", "gentlemen", 
                                  "motherland", "parliament","also", "percent", "president", "national", "mr", 
                                  "deputy", "government", "republic", "presented","statement",  "ghana", "honourable",
                                  "mister", "gh", "ministry", "million", "will", "thousand", "excellency", "chairman",
                                  "excellencies", "members", ".", ",", "your", "|", "year", "billion",
                                  "performance", "sector", "flt", "nana", "billions cedis", "introduction", zahlen))


newstopwords2 <- tibble(sentence = c("eq", "co", "rc", "ac", "ak", "bn", 
                                "fig", "file", "cg", "cb", "cm",
                                "ab", "_k", "_k_", "_x"))



stopword_sent <- as_tibble(stopwords::stopwords("en")) 
stopword_sent <- rename(stopword_sent, sentence = value)


tidy_sentence <- anti_join(tidy_sentence, stopword_sent, by = 'sentence')
tidy_sentence <- anti_join(tidy_sentence, redundant, by = 'sentence')
tidy_sentence <- anti_join(tidy_sentence, redundant3, by = 'sentence')
tidy_sentence <- anti_join(tidy_sentence, newstopwords2, by = 'sentence')


tidy_sentence <- tidy_sentence|>
  mutate(sentence = stringr::str_remove_all(sentence, pattern = "[0-9]"))|>
  filter(sentence != "", sentence != " ")

tidy_sentence$sentence <- trimws(tidy_sentence$sentence, which = "both")

dim(tidy_sentence)
#5085    3

tidy_sentence <- tidy_sentence|>filter(sentence != "", sentence != " ")

library(forcats)
library(tidytext)


#Tokenizing by n-gram

#to tokenize into consecutive sequences of words, called n-grams. By 
#seeing how often word X is followed by word Y, one can build a 
#model of the relationships between them

library(dplyr)
library(tidytext)


#tidy_sentence[rowSums(tidy_sentence$sentence[])>0,]

budget_bigrams <- tidy_sentence %>% #filter(docID == "1") %>% 
  unnest_tokens(bigram, sentence, token = "ngrams", n = 2)


library(tidyr)

# seperate words
bigrams_separated <- budget_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filter stop words and NA

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!is.na(word1))

# new bigram counts:


bigram_research <- bigrams_filtered %>%
  filter(word2 == "research") %>%group_by(author, docID)|>
  count(word1, sort = TRUE)

sort(unique(bigram_research$word1))

#Network analysis
#We may be interested in visualizing all of the relationships among #

#referring to a “graph” not in the sense of a visualization, 
#but as a combination of connected nodes. 
  

library(igraph)


research_graph <- bigrams_filtered %>%
  filter(word2 == "research") %>%group_by(author)|>
  count(word1, sort = TRUE)%>%
  graph_from_data_frame()


library(ggraph)

set.seed(875539)


bigrams_filtered %>%
  filter(word2 == "research") %>%group_by(author)|>
  count(word1, sort = TRUE)|>
  graph_from_data_frame()|>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = 0.15) +
  geom_node_point(color = "brown", size = 2, alpha = 0.3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  labs(title = "Network of the term RESEARCH in Ghana budgets 1999 - 2022",
       subtitle = "minmum count of term is one", x = "", y = "",
       caption = "Author graph based on Ghana Budget Statements from
  https://mofep.gov.gh/sites/default/files/")+
  #theme_void()
  ggthemes::theme_economist()



###plot 5


#download from here
#"https://www.scimagojr.com/countryrank.php?order=itp&ord=desc&year=2020"

#Scimago Journal & Country Rank
#Metrics based on Scopus® data as of April 2022

##general world
world_publ <- readxl::read_excel("~/scimagojr country rank 2020.xlsx")
ghana_world_pub <- world_publ|>filter(Country == "Ghana")

#africa
africa_publi <- readxl::read_excel("~/scimagojr country rank 2020 (1).xlsx")
ghana_africa_publi <- africa_publi|>filter(Country == "Ghana")

##biol and agric sc
africa_biol_agric <- readxl::read_excel("~/scimagojr country rank 2020 (2).xlsx")
ghana_biol_agric <- africa_biol_agric|>filter(Country == "Ghana")


#arts and humanities
africa_arts_hum <- readxl::read_excel("~/scimagojr country rank 2020 (3).xlsx")
ghana_arts_hum <- africa_arts_hum|>filter(Country == "Ghana")

##earth and planetary scienxe
africa_earth_planet <- readxl::read_excel("~/scimagojr country rank 2020 (4).xlsx")
ghana_earth_planet <- africa_earth_planet|>filter(Country == "Ghana")

#economics econometrics and finance
africa_econs_fin <- readxl::read_excel("~/scimagojr country rank 2020 (5).xlsx")
ghana_econs_fin <- africa_econs_fin|>filter(Country == "Ghana")

#health professions
africa_health_prof <- readxl::read_excel("~/scimagojr country rank 2020 (6).xlsx")
ghana_health_prof <- africa_health_prof|>filter(Country == "Ghana")


### repeat above
#earth and planetary science
africa_planetary <- readxl::read_excel("~/scimagojr country rank 2020 (7).xlsx")
ghana_planetary <- africa_planetary|>filter(Country == "Ghana")

#physics and astronomy
africa_physics_astro <- readxl::read_excel("~/scimagojr country rank 2020 (8).xlsx")
ghana_physics_astro <- africa_physics_astro|>filter(Country == "Ghana")

#atomic, molecular physics and optics
africa_mol_physic <- readxl::read_excel("~/scimagojr country rank 2020 (9).xlsx")
ghana_mol_physic <- africa_mol_physic|>filter(Country == "Ghana")

#computer science
africa_computer <- readxl::read_excel("~/scimagojr country rank 2020 (10).xlsx")
ghana_computer <- africa_computer|>filter(Country == "Ghana")

#stats prob and uncertainty
africa_stats_prob <- readxl::read_excel("~/scimagojr country rank 2020 (14).xlsx")
ghana_stats_prob <- africa_stats_prob|>filter(Country == "Ghana")

#mgt sc and operations res
africa_mgt_operes <- readxl::read_excel("~/scimagojr country rank 2020 (13).xlsx")
ghana_mgt_operes <- africa_mgt_operes|>filter(Country == "Ghana")

#decision science
africa_dec_sc <- readxl::read_excel("~/scimagojr country rank 2020 (12).xlsx")
ghana_dec_sc <- africa_dec_sc|>filter(Country == "Ghana")


#AI
africa_ai <- readxl::read_excel("~/scimagojr country rank 2020 (11).xlsx")
ghana_ai <- africa_ai|>filter(Country == "Ghana")


#The index is based on the set of the scientist's most cited papers 
#and the number of citations that they have received in other publications.


ghana_research_pub <- bind_rows(world_publi = ghana_world_pub,
                                alfric_publ = ghana_africa_publi,
                                stats_prob = ghana_stats_prob,
                                mgtsc_operes = ghana_mgt_operes,
                                decision_sc = ghana_dec_sc,
                                art_int = ghana_ai,
                                comp_sc = ghana_computer,
                                phys_atom_mol = ghana_mol_physic,
                                phys_astron = ghana_physics_astro,
                                earth_planet = ghana_planetary,
                                health_prof = ghana_health_prof,
                                econs_ecotr_fin = ghana_econs_fin,
                                arts_hum = ghana_arts_hum,
                                biol_agric = ghana_biol_agric)

ghana_research_pub$Title <- c("World publication", "Africa publication", "Stats and Probability",
                              "Management Sc and Operations Res", "Decision Sciences", "Artificial Intelligence", 
                              "Computer Science",
                              "Phys Atomic and Molecular", "Phys Astronomy", "Earth and Planetary Sc",
                              "Health Professions", "Econs Econometrics Finance", "Arts and Humanities", 
                              "Science Biology and Agric")


ghana_research_pub <- ghana_research_pub[, c(10,1, 9, 4:8)]|>janitor::clean_names()



ghana_research_pub[-c(1:2),-2]|> ##the first two rows are on World and Africa in general, delete
  pivot_longer(-title, names_to = "description")|>
  ggplot()+
  geom_col(aes(factor(description), value, fill = factor(description)))+
  scale_fill_manual(values =  terrain.colors(6))+
  labs(title = "Ghana metrics of scientific publications in 2020",
       fill = "Feature", x = "", y = "",
       caption = "Author graph based on 
       https://www.scimagojr.com/countryrank.php?order=itp&ord=desc&year=2020")+
  facet_wrap(~title, scales = "free")+ theme_bw(base_size = 14)+
  theme(axis.text.x = element_blank(),
        legend.position = "top",
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank())



#######

##plot 6

#"https://www.scimagojr.com/countryrank.php"
#"https://www.scimagojr.com/countryrank.php?order=itp&ord=desc"   ##for 1996 - 2021  
#"https://www.scimagojr.com/countryrank.php?order=itp&ord=desc&year=2021"
#"https://www.scimagojr.com/countryrank.php?order=itp&ord=desc&year=2020"
#Scimago Journal & Country Rank
#Metrics based on Scopus® data as of April 2022


africa_19962021 <- readxl::read_excel("~/scimagojr country rank 1996-2021.xlsx")
ghana_19962021 <- africa_19962021|>filter(Country == "Ghana")  


#mathematics
africa_math19962021 <- readxl::read_excel("~/scimagojr country rank 1996-2021 (1).xlsx")
ghana_maths19962021 <- africa_math19962021|>filter(Country == "Ghana")  


##computer science
africa_computSc19962021 <- readxl::read_excel("~/scimagojr country rank 1996-2021 (2).xlsx")
ghana_compusc19962021 <- africa_computSc19962021 |>filter(Country == "Ghana")  

##AI
africa_AI19962021  <- readxl::read_excel("~/scimagojr country rank 1996-2021 (3).xlsx")
ghana_africa_AI19962021  <- africa_AI19962021 |>filter(Country == "Ghana")  


ghana_19962021 <- bind_rows(general = ghana_19962021,
                            ai9621 = ghana_africa_AI19962021,
                            comp_sc =  ghana_compusc19962021,
                            mathe = ghana_maths19962021)
ghana_19962021$Title <- c("All subjects", "Artificial Intelligence",
                          "Computer Science", "Mathematics")


ghana_19962021[-1, -c(1:3)]|>
  pivot_longer(-Title, names_to = "description")|>
  ggplot()+
  geom_col(aes(factor(description), value, fill = factor(description)))+
  scale_fill_manual(values =  terrain.colors(7))+
  labs(title = "Ghana metrics of scientific publications in 1996-2021",
       fill = "Feature", x = "", y = "",
       caption = "Author graph based on 
       https://www.scimagojr.com/countryrank.php")+
  facet_wrap(~Title, scales = "free")+ theme_bw(base_size = 14)+
  theme(axis.text.x = element_blank(),
        legend.position = "top",
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank())


##plot 7


#download data

#https://data.worldbank.org/indicator/IP.PAT.RESD?locations=GH

patent_gh <- readxl::read_excel("~/API_IP.PAT.RESD_DS2_en_excel_v2_4151722.xls")
names(patent_gh) <- patent_gh[3,] 
patent_gh <- patent_gh[-c(1:3),]

patent_gh|>filter(`Country Name` == "Ghana")|> 
  select(`2017`:`2021`)|>
  mutate_at(c(1:5), readr::parse_number)|>t()
  


#Trademark applications, total
#"API_IP.TMK.TOTL_DS2_en_excel_v2_4169242.xls"

tradeMark_gh <- readxl::read_excel("~/API_IP.TMK.TOTL_DS2_en_excel_v2_4169242.xls")
names(tradeMark_gh) <- tradeMark_gh[3,] 
tradeMark_gh <- tradeMark_gh[-c(1:3),]


tradeMark_gh <- tradeMark_gh|>filter(`Country Name` == "Ghana")|> 
  #select(`2017`:`2021`)|>
  mutate_at(c(5:66), readr::parse_number)|>
  select(-c(`Country Name`:`Indicator Code`))

tradeMark_gh <- data.frame(TM_val = t(tradeMark_gh),
                  Year = rownames(t(tradeMark_gh)))
rownames(tradeMark_gh) <- NULL


tradeMark_gh|>drop_na(TM_val)|>
  ggplot()+
  geom_col(aes(Year, TM_val), alpha = 0.4)+
  geom_text(aes(Year, TM_val,label = TM_val),
            vjust = -0.8)+
  labs(title = "Trade mark applications in Ghana 1980 - 2018",
       subtitle = "WIPO Patent Report as at January 2021",
       caption = "Author graph based on data from:
       https://data.worldbank.org/",
       x = "", y = "Trade mark applications")+
  ggpubr::theme_pubr()
  #ggthemes::theme_gdocs()



##this code implements all figures in my medium.com blog


