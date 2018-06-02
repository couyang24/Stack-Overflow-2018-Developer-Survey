if(!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, highcharter, ggrepel, scales, plotly, cowplot, knitr, skimr)

pacman::p_load(tidyverse, DT, lubridate, leaflet, leaflet.extras, maps, data.table, ggthemes, rebus, clue, skimr, plotly)


survey18 <- read_csv("input/survey_results_public.csv")
glimpse(survey18)

survey18 %>% skim() %>% kable()

survey18 %>% 
  filter(!is.na(Country)) %>% 
  group_by() %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  #head(10) %>% 
  hchart('treemap',hcaes(x = Country, value = n, color = n)) %>% 
  hc_title(text = 'Countries from where overall respondents come from') 












survey18 %>%
  select(DevType, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  filter(!is.na(DevType)) %>%
  mutate(DevType = case_when(str_detect(DevType, "Data scientist") ~ "Data scientist",
                             str_detect(DevType, "academic") ~ "Academic researcher",
                             TRUE ~ DevType)) %>% 
  filter(!is.na(DevType)) %>% 
  count(DevType) %>% 
  hchart('treemap',hcaes(x = DevType, value = n, color = n)) %>% 
  hc_title(text = 'Respondents\' Job Title') 



survey18 %>% select(Salary) %>% summary()


survey18 %>% select(Salary) %>% na.omit() %>% filter(Salary>9.000e+50) %>% ggplot(aes(x = "", Salary))+geom_boxplot()






ggplotly(survey18 %>%
  select(DevType, Salary, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>% 
  na.omit(Salary) %>% 
  filter(Salary<1e+6) %>% 
  ggplot(aes(x = DevType, Salary, fill = factor(DevType)))+geom_boxplot() + 
  coord_flip())
  
  


  
survey18 %>%
  select(DevType, Salary, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>% 
  na.omit(Salary) %>% 
  filter(Salary<1e+7) %>% 
  group_by(DevType) %>% 
  summarise(avg_salary = mean(Salary)) %>%
  ggplot(aes(reorder(DevType, avg_salary), avg_salary, fill = DevType)) + 
  geom_col() +
  coord_cartesian(ylim = c(25000, 50000))+ 
  scale_fill_brewer()+
  theme(legend.position="none") +
  coord_flip()
