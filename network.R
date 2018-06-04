library(tidyverse)
library(stringr)
library(scales)

library(tidytext)
library(igraph)
library(ggraph)

library(treemap)

rm(list=ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"
fillColorLightCoral = "#F08080"

survey_results <- read_csv("input/survey_results_public.csv")

# glimpse(survey_results)
# 
# survey_country = survey_results %>%
#   group_by(Country) %>%
#   summarise(Count = n()) %>%
#   arrange(desc(Count)) %>%
#   ungroup() %>%
#   mutate(Country = reorder(Country,Count)) %>%
#   head(20) 
# 
# 
# treemap(survey_country, 
#         index="Country", 
#         vSize = "Count",  
#         title="Participation by Country", 
#         palette = "RdBu",
#         fontsize.title = 14 
# )
# 
# TotalNoofRows = nrow(survey_results)
# 
# survey_results %>%
#   group_by(Hobby) %>%
#   summarise(Count = n()/TotalNoofRows) %>%
#   arrange(desc(Count)) %>%
#   ungroup() %>%
#   mutate(Hobby = reorder(Hobby,Count)) %>%
#   
#   ggplot(aes(x = Hobby,y = Count)) +
#   geom_bar(stat='identity',fill= fillColor2) +
#   geom_text(aes(x = Hobby, y = .01, label = paste0("( ",round(Count*100,2)," %)",sep="")),
#             hjust=0, vjust=.5, size = 4, colour = 'black',
#             fontface = 'bold') +
#   scale_y_continuous(labels = percent_format()) +
#   labs(x = 'Hobby', 
#        y = 'Percentage', 
#        title = 'Hobby and Percentage') +
#   coord_flip() +
#   theme_bw()
# 
# survey_results %>%
#   filter(Hobby == "Yes") %>%
#   group_by(Country) %>%
#   summarise(Count = n()/TotalNoofRows) %>%
#   arrange(desc(Count)) %>%
#   ungroup() %>%
#   mutate(Country = reorder(Country,Count)) %>%
#   head(10) %>%
#   
#   ggplot(aes(x = Country,y = Count)) +
#   geom_bar(stat='identity',fill= fillColor) +
#   geom_text(aes(x = Country,y = .01, label = paste0("( ",round(Count*100,2)," %)",sep="")),
#             hjust=0, vjust=.5, size = 4, colour = 'black',
#             fontface = 'bold') +
#   scale_y_continuous(labels = percent_format()) +
#   labs(x = 'Country', 
#        y = 'Percentage', 
#        title = 'Country with Hobby as Coding and Percentage') +
#   coord_flip() +
#   theme_bw()
# 
# 
# DevType <- survey_results %>%
#   mutate(DevType = str_split(DevType, pattern = ";")) %>%
#   unnest(DevType) %>%
#   select(DevType)
# 
# TotalNoofRows = nrow(DevType)
# 
# DevType %>%
#   group_by(DevType) %>%
#   summarise(Count = n()/TotalNoofRows) %>%
#   arrange(desc(Count)) %>%
#   ungroup() %>%
#   mutate(DevType = reorder(DevType,Count)) %>%
#   head(10) %>%
#   
#   ggplot(aes(x = DevType,y = Count)) +
#   geom_bar(stat='identity',fill= fillColor) +
#   geom_text(aes(x = DevType, y = .01, label = paste0("( ",round(Count*100,2)," %)",sep="")),
#             hjust=0, vjust=.5, size = 4, colour = 'black',
#             fontface = 'bold') +
#   scale_y_continuous(labels = percent_format()) +
#   labs(x = 'DevType', 
#        y = 'Percentage', 
#        title = 'DevType and Percentage') +
#   coord_flip() +
#   theme_bw()
# 
# 
# 
# survey_results %>%
#   select(DevType,YearsCoding) %>%
#   mutate(DevType = str_split(DevType, pattern = ";")) %>%
#   unnest(DevType) %>%
#   group_by(DevType,YearsCoding) %>%
#   summarise(Count = n()) %>%
#   arrange(desc(Count)) %>%
#   ungroup() %>%
#   mutate(YearsCoding = as.character(YearsCoding),
#          DevType = as.character(DevType)) %>%
#   mutate(DevType_YearsOfCoding = paste(DevType,YearsCoding)) %>%
#   mutate(DevType_YearsOfCoding = reorder(DevType_YearsOfCoding,Count)) %>%
#   head(10) %>%
#   
#   ggplot(aes(x = DevType_YearsOfCoding,y = Count)) +
#   geom_bar(stat='identity',fill= fillColor2) +
#   geom_text(aes(x = DevType_YearsOfCoding, y = 1, label = paste0("( ",Count," )",sep="")),
#             hjust=0, vjust=.5, size = 4, colour = 'black',
#             fontface = 'bold') +
#   labs(x = 'DevType_YearsOfCoding', 
#        y='Count', 
#        title = 'DevType_YearsOfCoding and Count') +
#   coord_flip() +
#   theme_bw()
# 
# 
# 
# 
# 
# 
# 
# TotalNoofRows = nrow(survey_results) 
# 
# plotFormalEducation <- function(survey_results,TotalNoofRows) {
#   survey_results %>%
#     filter(!is.na(FormalEducation )) %>%
#     select(FormalEducation ) %>%
#     group_by(FormalEducation ) %>%
#     summarise(Count = n()/TotalNoofRows ) %>%
#     arrange(desc(Count)) %>%
#     ungroup() %>%
#     mutate(FormalEducation  = reorder(FormalEducation ,Count)) %>%
#     head(10) %>%
#     
#     ggplot(aes(x = FormalEducation ,y = Count)) +
#     geom_bar(stat='identity',fill= fillColorLightCoral) +
#     geom_text(aes(x = FormalEducation , y = .01, label = paste0("( ",round(Count*100,2)," %)",sep="")),
#               hjust=0, vjust=.5, size = 4, colour = 'black',
#               fontface = 'bold') +
#     scale_y_continuous(labels = percent_format()) +
#     labs(x = 'FormalEducation ', 
#          y = 'Percentage', 
#          title = 'FormalEducation  and Percentage') +
#     coord_flip() +
#     theme_bw()
# }
# 
# plotFormalEducation(survey_results,TotalNoofRows)
# 
# 



count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, LanguageWorkedWith, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, word2, sort = TRUE)
}


visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
}

# visualize_bigrams_individual <- function(bigrams) {
#   set.seed(2016)
#   a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
#   
#   bigrams %>%
#     graph_from_data_frame() %>%
#     ggraph(layout = "fr") +
#     geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a,end_cap = circle(.07, 'inches')) +
#     geom_node_point(color = "lightblue", size = 5) +
#     geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
#     theme_void()
# }



survey_results %>%
  count_bigrams() %>%
  filter(word1 == "r" | word2 == "r") %>%
  filter( n > 50) %>%
  visualize_bigrams()
