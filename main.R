if(!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, highcharter, ggrepel, scales, plotly, cowplot, knitr, skimr)

pacman::p_load(tidyverse, DT, lubridate, leaflet, leaflet.extras, maps, data.table, ggthemes, rebus, clue, skimr, plotly)


survey18 <- read_csv("input/survey_results_public.csv")
glimpse(survey18)

survey18 %>% skim() %>% kable()

survey18 %>% 
  filter(!is.na(Country)) %>% 
  count(Country) %>% 
  arrange(desc(n)) %>% 
  hchart('treemap',hcaes(x = Country, value = n, color = n)) %>% 
  hc_title(text = 'Countries from where overall respondents come from') 



survey18 %>% 
  filter(!is.na(Age)) %>% 
  count(Age) %>% 
  arrange(desc(n)) %>% 
  hchart('treemap',hcaes(x = Age, value = n, color = n)) %>% 
  hc_title(text = 'Respondents\' Age Range') 

survey18 %>% 
  filter(!is.na(UndergradMajor)) %>% 
  count(UndergradMajor) %>% 
  arrange(desc(n)) %>% 
  hchart('treemap',hcaes(x = UndergradMajor, value = n, color = n)) %>% 
  hc_title(text = 'Respondents\' UndergradMajor Range')






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









ggplotly(survey18 %>%
  select(DevType, Salary, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>% 
  na.omit(Salary) %>% 
  filter(Salary<1e+6) %>% 
  ggplot(aes(x = DevType, Salary, fill = factor(DevType)))+geom_boxplot() +
  theme(legend.position="none") + 
  labs(x = "", y = "", title = "Annual Salary by Job Title Analysis", subtitle = "Interactive Boxplot") +
  coord_flip())
  

  
survey18 %>%
  select(DevType, Salary, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>% 
  na.omit(Salary) %>% 
  group_by(DevType) %>% 
  summarise(avg_salary = median(Salary)) %>% 
  ggplot(aes(reorder(DevType, avg_salary), avg_salary, fill = avg_salary)) + 
  geom_col() +
  coord_cartesian(ylim = c(25000, 50000))+ 
  theme(legend.position="none") +
  labs(x = "", y = "", title = "Annual Salary by Job Title Analysis", caption = "source: Stack Overflow 2018 Developer Survey") +
  coord_flip()


survey18 %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>% 
  na.omit(Salary) %>% 
  group_by(DevType) %>% 
  summarise(avg_salary = median(Salary)) %>% 
  ggplot(aes(reorder(DevType, avg_salary), avg_salary, fill = Gender)) + 
  geom_col() +
  coord_cartesian(ylim = c(25000, 50000))+ 
  theme(legend.position="none") +
  labs(x = "", y = "", title = "Annual Salary by Job Title Analysis", caption = "source: Stack Overflow 2018 Developer Survey") +
  coord_flip()




