install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#Loading the dataset
horror_movies_df <- read.csv("horror_movies.csv")

#Filtering out missing data and low-budge, low-revenue outliers
horror_movies <- horror_movies_df %>%
  filter(budget > 1000) %>%
  filter(revenue > 1000) %>%
  filter(runtime > 0) %>%  
  filter(vote_count > 1)

#Mutation: splitting the data by English/Non-English and adding a bool column is_english
horror_movies<-horror_movies %>%
  mutate(is_english = (original_language=="en") )

#Number of movies by English and non-English language.
horror_movies %>%
  group_by(is_english) %>%
  count()

#Calculating the revenue median for English and non-English movies
horror_movies %>%
  group_by(is_english)%>%
  summarise( median(revenue, na.rm=TRUE))

#Figure 1, Plotting the revenue of english vs non-english movies
ggplot(data = horror_movies, mapping = aes(x=is_english, y=revenue, group=is_english)) +
  geom_boxplot() +
  ggtitle("revenue of english vs non-english movies") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y= "revenue", x = "is_english") +
  scale_y_continuous(labels = scales::comma)

#Excluding non-English language records
horror_movies <- horror_movies_df %>%
  filter(budget > 1000) %>%
  filter(revenue > 1000) %>%
  filter(runtime > 0) %>%  
  filter(vote_count > 1) %>%
  filter(original_language == "en") %>%
  select(title,release_date,budget,vote_count,vote_average,revenue,runtime,genre_names)

#Some descriptive stats about revenue
horror_movies %>%
  summarize(avg_revenue = median(revenue, na.rm=TRUE), minimum = min(revenue, na.rm=TRUE), maximum = max(revenue), iqRange=IQR(revenue))

#Top 10 revenue movies
horror_movies %>% 
  arrange(desc(revenue)) %>% 
  top_n(5, revenue) %>%
  select(title, release_date, revenue)

#Figure 2, Plotting the distribution of revenue of English horror movies
horror_movies%>%
  ggplot(aes(x=revenue)) + 
  ggtitle("Distribution of revenue for english language horror movies")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_histogram(color="black", fill="white")+
  scale_x_continuous(labels = scales::comma)

#Getting the no of genres each movie belongs to and adding a column number of genres
horror_movies <- horror_movies %>%
  mutate(no_of_genres=sapply(strsplit(horror_movies$genre_names," ") , length))

#Figure 3, Rating of horror movies by no of genre affiliation
ggplot(data = horror_movies, mapping = aes(x=no_of_genres, y=vote_average, group=no_of_genres)) +
  geom_boxplot()+
  ggtitle("Rating of horror movies by number of genre affiliation") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "Rating", x = "Number of genres") +
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(n.breaks=10)

#Figure 4, Revenue of horror movies by no of genre affiliation
ggplot(data = horror_movies, mapping = aes(x=no_of_genres, y=revenue, group=no_of_genres)) +
  geom_boxplot()+
  ggtitle("Revenue of horror movies by no of genre affiliation") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "Revenue", x = "Number of genres") +
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(n.breaks=10)

#Figure 5 Runtime by revenue using a logarithmic transform on revenue due to skewed distribution
ggplot(data = horror_movies, mapping = aes(x=runtime, y=revenue)) +
  geom_point()+
  ggtitle(" Runtime by revenue ") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "revenue", x = "runtime") +
  scale_y_continuous(labels = scales::comma, trans='log10')

#Get month
horror_movies <- horror_movies %>%
  mutate(month = format(as.Date(release_date, format="%Y-%m-%d"), "%m"))

#Figure 6, Number of movies released per month
ggplot(horror_movies, aes(month)) + geom_bar()+
  ggtitle(" Movies released by month ") +
  theme(plot.title = element_text(hjust = 0.5))

#Generating monthly average revenue by median , to avoid the effect of outliers.
monthly_mean_revenue <- horror_movies %>%
  group_by(month) %>%
  summarize(avg_revenue = median(revenue, na.rm=TRUE))

#Figure 7, plotting average revenue by month
ggplot(data=monthly_mean_revenue, mapping = aes(month, avg_revenue)) +
  geom_col() +
  ggtitle(" Revenue of movies released by month ") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::comma, limits=c(0,50000000))

#Figure 8 revenue  by rating
ggplot(data = horror_movies, mapping = aes(x=vote_average, y=revenue)) +
  geom_point()+
  ggtitle(" Revenue  by Rating ") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "revenue", x = "rating") +
  scale_y_continuous(labels = scales::comma, trans='log10')


