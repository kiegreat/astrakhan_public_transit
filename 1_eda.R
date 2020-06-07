
library(tidyverse)
library(openxlsx)

df <- read.xlsx('data/list_of_operators.xlsx', sheet = 2)

# - Select important columns -

df2 <- df %>% select(1, 2, 6, 9, 10, 11, 12)
names(df2) <- c('id', 'route', 'len', 'eco_standard', 'max_num_of_buses', 'start_of_operation', 'operators')

glimpse(df2)

# - Try to deal with missing rows -

df3 <- df2 %>% 
  filter(!is.na(id) | !is.na(route) | !is.na(len) | !is.na(eco_standard) | !is.na(max_num_of_buses) | !is.na(start_of_operation) | !is.na(operators)) %>% 
  fill(id, route, .direction = 'down')

glimpse(df3)

s <- df3 %>% 
  group_by(route) %>% 
  summarise(n_len = n_distinct(len), n_start = n_distinct(start_of_operation),
            n_eco = n_distinct(eco_standard), n_max = n_distinct(max_num_of_buses), n_op = n_distinct(operators))

n_distinct(df3$id)

ch <- df3 %>% filter(route == '92н')
ch <- df3 %>% distinct(id, route)

# - Final handling of missing data -

# a.info about routes
df4 <- df2 %>% 
  filter(!is.na(route) | !is.na(len) | !is.na(start_of_operation)) %>% 
  fill(route, .direction = 'down') %>% 
  group_by(route) %>% 
  fill(len, start_of_operation, .direction = 'down') %>% 
  fill(len, start_of_operation, .direction = 'up') %>% 
  ungroup() %>% 
  mutate(
    len = str_replace_all(len, pattern = '[:space:]', replacement = '') %>% 
      str_replace_all(pattern = ',', replacement = '.') %>% 
      as.numeric(),
    start_of_operation = str_replace_all(start_of_operation, pattern = '.*([0-9]{4}).*', replacement = '\\1')
  ) %>%  
  group_by(route, start_of_operation) %>% 
  summarise(len = mean(len))

summary(df4$len)
summary(df4$start_of_operation)

ggplot(df4, aes(x = start_of_operation)) + geom_bar()
ggplot(df4, aes(x = len)) + geom_histogram()

# b. info about operators
df5 <- df2 %>% 
  filter(!is.na(route) | !is.na(len) | !is.na(eco_standard) | !is.na(max_num_of_buses) | !is.na(start_of_operation) | !is.na(operators)) %>% 
  fill(route, .direction = 'down') %>% 
  filter(!is.na(operators)) %>% 
  mutate(
    inn12 = str_replace_all(operators, pattern = '.*([0-9]{12}).*', replacement = '\\1') %>% as.numeric(),
    inn10 = str_replace_all(operators, pattern = '.*([0-9]{10}).*', replacement = '\\1') %>% as.numeric(),
    inn = ifelse(is.na(inn12), inn10, inn12)
  ) %>% 
  filter(!is.na(inn)) %>% 
  distinct(route, inn)

n_distinct(df5$inn)

df_inn <- df5 %>% 
  group_by(inn) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

head(df_inn)
ggplot(df_inn, aes(x = inn)) + geom_bar()

df_inn2 <- df5 %>% 
  group_by(route) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

head(df_inn2)

# c. info about number of buses
df6 <- df2 %>% 
  filter(!is.na(route) | !is.na(len) | !is.na(eco_standard) | !is.na(max_num_of_buses) | !is.na(start_of_operation) | !is.na(operators)) %>% 
  fill(route, .direction = 'down') %>% 
  filter(!is.na(max_num_of_buses)) %>% 
  mutate(
    buses1 = str_replace_all(max_num_of_buses, pattern = '.*([0-9]{1}).*', replacement = '\\1') %>% as.numeric(),
    buses2 = str_replace_all(max_num_of_buses, pattern = '.*([0-9]{2}).*', replacement = '\\1') %>% as.numeric(),
    buses = ifelse(is.na(buses2), buses1, buses2),
    buses_class = tolower(max_num_of_buses) %>% 
      str_replace_all(pattern = '.*(малого класса|малый класс|мк).*', replacement = 'МК') %>% 
      str_replace_all(pattern = '.*(среднего класса|средний класс|ск).*', replacement = 'СК') %>% 
      str_replace_all(pattern = '.*(большого класса|большой класс|бк).*', replacement = 'БК')
  ) %>% 
  filter(!is.na(buses)) %>% 
  select(route, buses, buses_class)

n_distinct(df6$route)

df6 %>% 
  group_by(buses_class) %>% 
  summarise(n = sum(buses))

saveRDS(df6, 'data/buses_per_route.rds')













