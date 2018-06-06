
library(dplyr)
library(highcharter)
library(tidyr)
library(viridisLite)
library(countrycode)
library(ggplot2)
library(gdata)
library(purrr)
library(rworldmap)
library (igraph)
library(visNetwork)
library(randomcoloR)
library(readr)
library(stringr)
library(scales)
library(reshape2)




survey_results_public <- read.csv("input/survey_results_public.csv",stringsAsFactors =FALSE)













df <- survey_results_public %>% select(Respondent,EducationTypes)
df2 <- df %>% 
  mutate(EducationTypes = strsplit(as.character(EducationTypes), ";")) %>% 
  unnest(EducationTypes)

df2_edges <- df2 %>% group_by(Respondent) %>%
  filter(n()>=2) %>%
  do(data.frame(t(combn((.)[["EducationTypes"]], 2)), stringsAsFactors=FALSE)) %>% 
  ungroup() %>%
  rename(source = X1, target = X2) %>%
  select(-Respondent)

df2_edges <- df2_edges %>% group_by(source,target) %>% summarise(weight=n())

names(df2_edges) <- c("from","to","weight")
df2_edges$weight <- df2_edges$weight/1500

df2_edges$width <- 1+df2_edges$weight # line width
df2_edges$color <- "gray"    # line color  
#df2_edges$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
df2_edges$smooth <- FALSE    # should the edges be curved?
df2_edges$shadow <- FALSE    # edge shadow

df2_nodes <- df2 %>% filter(!is.na(EducationTypes)) %>% group_by(EducationTypes) %>% summarise(n = n()/1000) %>% arrange(desc(n))
names(df2_nodes) <- c("id","size")

n <- nrow(df2_nodes)
palette <- distinctColorPalette(n)

df2_nodes$shape <- "dot"  
df2_nodes$shadow <- TRUE # Nodes will drop shadow
df2_nodes$title <- df2_nodes$id # Text on click
df2_nodes$label <- df2_nodes$id # Node label
df2_nodes$size <- df2_nodes$size # Node size
df2_nodes$borderWidth <- 2 # Node border width

df2_nodes$color.background <- palette[as.numeric(as.factor(df2_nodes$id))]
df2_nodes$color.border <- "black"
df2_nodes$color.highlight.background <- "orange"
df2_nodes$color.highlight.border <- "darkred"

df2_nodes <- df2 %>% filter(!is.na(EducationTypes)) %>% group_by(EducationTypes) %>% summarise(n = n()/1000) %>% arrange(desc(n))
names(df2_nodes) <- c("id","size")

n <- nrow(df2_nodes)
palette <- distinctColorPalette(n)
# df2_nodes$title <- NULL
df2_nodes$shape <- "dot"  
df2_nodes$shadow <- TRUE # Nodes will drop shadow
df2_nodes$title <- df2_nodes$id # Text on click
df2_nodes$label <- df2_nodes$id # Node label
df2_nodes$size <- df2_nodes$size # Node size
df2_nodes$borderWidth <- 2 # Node border width

df2_nodes$color.background <- palette[as.numeric(as.factor(df2_nodes$id))]
df2_nodes$color.border <- "black"
df2_nodes$color.highlight.background <- "orange"
df2_nodes$color.highlight.border <- "darkred"

visNetwork(df2_nodes, df2_edges, height = "500px", width = "100%") %>% visIgraphLayout(layout = "layout_with_lgl") %>% 
  visEdges(shadow = TRUE,
           color = list(color = "gray", highlight = "orange"))
