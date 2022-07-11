library(tidyverse)
library(factoextra)
library(babynames)
library(ggfortify)
library(cowplot)
library(ggrepel)

# Babynames ---------------------------------------------------------------
babynames <- babynames

# Create matrix of top 5 male names
males <- babynames %>% 
  filter(sex == "M") %>% 
  select(-sex,-n) %>% 
  group_by(year) %>% 
  top_n(5, prop) %>% 
  ungroup() %>% 
  spread(year, prop, fill = 0) %>%
  remove_rownames() %>% 
  column_to_rownames(var = "name") %>% 
  as.matrix()

# K means
set.seed(15)
km_males <- kmeans(scale(males), 5, nstart = 25) # 8 clusters, 25 random sets
km_males 

# Make into data frame
males_df <- males %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "name") %>% 
  mutate(group = as.factor(km_males$cluster))

# Plot 1990 vs 2010
males_df %>%
  ggplot(aes(x =`1990` , y = `2010`, color = group)) +
  geom_point()+
  geom_text(aes(label = name),hjust=0, vjust=0, alpha = 0.6)+
  labs(title="Plot 1990 vs 2010",
       x ="1990", y = "2010")


# Visualize Clustering results
fviz_cluster(km_males, data = males,
             main = "Clustering of Top 5 male Names\n From 1880-2017")

# Compare
interesting <- c("Michael", "John","Liam", "Robert", "Christopher")

babynames %>%
  filter(sex == "M" &
           name %in% interesting ) %>% 
  ggplot(aes(x = year, y = prop, color = name)) +
  geom_line()+
  labs(title = "Compare popular male names in \n different clusters and generations")


for(i in 1:5){
  print(names(km_males$cluster[km_males$cluster==i]))
  i = i +1
}

# Compare Jennifer to Mary
interesting2 <- c("Jennifer", "Mary","Helen","Sophia")

babynames %>%
  filter(sex == "F" &
           name %in% interesting2 ) %>% 
  ggplot(aes(x = year, y = prop, color = name)) +
  geom_line()+
  labs(title = "Compare popular female names in \n different clusters and generations")


males_2000s <- babynames %>% 
  filter(sex == "M", year %in% c(2000:2017)) %>% 
  select(-sex,-n) %>% 
  group_by(year) %>% 
  top_n(5, prop)

males_2000s%>%
  ggplot(aes(x = year, y = prop, color = name)) +
  geom_line()+
  geom_dl(aes(label = name), method = list(dl.trans(x = x + .01), "last.points")) +
  geom_dl(aes(label = name), method = list(dl.trans(x = x - .01), "first.points"))+
  expand_limits(x = c(1995, 2020))+
  labs(title = "Popular male names \n in 21st Centurty comparison")



# Create sum of propotion of top 5 male names 2000s
males_2000s_totalprop <- babynames %>% 
  filter(sex == "M", year %in% c(2000:2017)) %>% 
  select(-sex,-n) %>% 
  group_by(year) %>% 
  top_n(5, prop) %>%
  summarise(top5 = sum(prop))

# Create matrix of top 5 female names
females <- babynames %>% 
  filter(sex == "F") %>% 
  select(-sex,-n) %>% 
  group_by(year) %>% 
  top_n(5, prop)

females_2000s <- babynames %>% 
  filter(sex == "F", year %in% c(2000:2017)) %>% 
  select(-sex,-n) %>% 
  group_by(year) %>% 
  top_n(5, prop)

females_2000s%>%
  ggplot(aes(x = year, y = prop, color = name)) +
  geom_line()+
  geom_dl(aes(label = name), method = list(dl.trans(x = x + .01), "last.points")) +
  geom_dl(aes(label = name), method = list(dl.trans(x = x - .01), "first.points"))+
  expand_limits(x = c(1995, 2020))+
  labs(title = "Popular female names \n in 21st Centurty comparison")

# Create sum of propotion of top 5 female names 2000s
females_2000s_totalprop <- babynames %>% 
  filter(sex == "F", year %in% c(2000:2017)) %>% 
  select(-sex,-n) %>% 
  group_by(year) %>% 
  top_n(5, prop) %>%
  summarise(top5 = sum(prop))

# T test if concentrations of top 5 male name and top 5 female names are different

t.test(females_2000s_totalprop$top5, males_2000s_totalprop$top5, paired = TRUE, alternative = "two.sided")

# No significant

