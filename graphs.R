# single singer history

# First select top music directors working with artist

kkmusic= songcol %>% mutate(music_director= str_replace_all(music_director," ","")) %>% 
  mutate(singer= str_replace_all(singer," ","")) %>% 
  select(music_director,rank,year,singer) %>%
  filter(year>1953,year<1970) %>%  # can select year here
  unnest_tokens(word,music_director) %>% 
  #filter(word%in%toplyrics) %>% 
 # group_by(year) %>% 
  #count(word) %>% 
  #arrange(desc(n)) %>% 
  #arrange(year) %>% 
  rename(music_director=word) %>% 
  unnest_tokens(word,singer) %>% 
  rename(singer=word) %>% 
  filter(singer=="mohammedrafi") %>%  # can select singer here
  group_by(music_director) %>% 
  count(music_director) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  select(music_director)
  
  

# plot collaboration ,stat_smooth wont work.

songcol %>% mutate(music_director= str_replace_all(music_director," ","")) %>% select(music_director,singer,rank,year) %>% unnest_tokens(word,music_director)%>% 
   rename(music_director=word) %>%
  unnest_tokens(word,singer) %>% 
  rename(singer=word) %>% 
  filter(singer=="latamangeshkar") %>% 
  filter(music_director %in% kkmusic$music_director) %>% 
  group_by(year) %>% count(music_director) %>% arrange(desc(n)) %>% arrange(year) %>%
  filter(year>1953,year<1965) %>% 
  ggplot(aes(x=year,y=n,color=music_director,group=music_director))+
  geom_line()  # group=singer important
 

# slopegraph
source('slopegraph.R') # loads function
   
source_df = songcol %>% mutate(music_director= str_replace_all(music_director," ","")) %>% select(music_director,singer,rank,year) %>% unnest_tokens(word,music_director)%>% 
  rename(music_director=word) %>%
  unnest_tokens(word,singer) %>% 
  rename(singer=word) %>% 
  filter(singer=="mohammedrafi") %>% 
  filter(music_director %in% kkmusic$music_director) %>% 
  group_by(year) %>% count(music_director) %>% arrange(desc(n)) %>% arrange(year) %>%
  filter(year>1953,year<1970)


df <- tufte_sort(source_df, 
                x="year", 
                y="n", 
                group="music_director", 
                method="tufte", 
                min.space=0.05)

df <- transform(df, 
                x=factor(x), 
                y=round(y))

## Plot
plot_slopegraph(df) + labs(title="Estimates of % survival rates") + 
  theme(axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  family = "American Typewriter",
                                  face="bold"),
        axis.text = element_text(family = "American Typewriter",
                                 face="bold"))