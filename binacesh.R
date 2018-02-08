library(tidyverse)
library(rvest)
library(tidytext)

a= c(1953:1983)
binacat= function(x){
  url = paste("http://www.hindigeetmala.net/binaca_geetmala_",x,".htm",sep="")

songlist = read_html(url) 

# i made an error initially dont push empty strings



Tablepref = songlist %>%
  html_nodes("table.b1.w760.pad2.allef") %>% html_table()


Table11 = as.data.frame(Tablepref[1])
Table12 = as.data.frame(Tablepref[2])

total <- rbind(Table11,Table12) 

total$year = rep(x,length(total$X1))
total
}

abc= map(a,~binacat(.))

fgh = bind_rows(def)

fgh= fgh %>% rename(rank = X1, song = X2,singer=X3,music_director=X4,movie=X7,actors=X7)

songcol = fgh

songcol = songcol %>% rename(lyricist=X5,movie=X6)

songcol$singer= str_replace_all(songcol$singer," ","") # we do it so that each singer name combined

songcol$movie = songcol$movie %>%  str_replace_all(" *\\(.*?\\) *","") # replace contents within parenthesis year eg movie(year)


library(tidytext)

songcol %>% select(singer,rank,year) %>% unnest_tokens(word,singer) %>% count(word) %>% arrange(desc(n))

scol1 = songcol %>% select(singer,rank,year) %>% unnest_tokens(word,singer) %>% count(word) %>% top_n(5) %>% arrange(desc(n))

topsing = scol1$word # vector of top singers

str(songcol$singer)

# So we see lata ,md.rafi,asha,mukesh,mahendra sang most popular songs

# year wise

songcol %>% select(singer,rank,year) %>% unnest_tokens(word,singer) %>% group_by(year) %>% count(word) %>% arrange(desc(n)) %>% arrange(year) %>% View()

songcol %>% select(singer,rank,year) %>% unnest_tokens(word,singer) %>% group_by(rank) %>% count(word) %>% arrange(desc(n)) %>% View()

# topsinger analysis by year multiple time series

songcol  %>% select(singer,rank,year) %>% unnest_tokens(word,singer) %>% filter(word%in%topsing) %>% 
group_by(year) %>% count(word) %>% arrange(desc(n)) %>% arrange(year) %>% rename(singer=word) %>% 
  ggplot(aes(x=year,y=n,color=singer,group=singer))+#geom_line() + # group=singer important
  stat_smooth(method = "loess", formula = y ~ x, size = 1,se=FALSE)

song_animate = songcol  %>% select(singer,rank,year) %>% unnest_tokens(word,singer) %>% filter(word%in%topsing) %>% 
  group_by(year) %>% count(word) %>% arrange(desc(n)) %>% arrange(year) %>% rename(singer=word) %>% 
  ggplot(aes(x=year,y=n,frame = year,cumulative=TRUE,color=singer,group=singer))+geom_line(size = 2.5)
library(gganimate)
gganimate(song_animate,ani.width = 800, ani.height = 500)

###lyricist

scol2 = songcol %>% mutate(lyricist= str_replace_all(lyricist," ","")) %>% select(lyricist,rank,year) %>% unnest_tokens(word,lyricist) %>% count(word) %>% top_n(5) %>% arrange(desc(n))

toplyrics = scol2$word # vector of lyricist

songcol %>% mutate(lyricist= str_replace_all(lyricist," ","")) %>% select(lyricist,rank,year) %>% unnest_tokens(word,lyricist) %>% filter(word%in%toplyrics) %>% 
  group_by(year) %>% count(word) %>% arrange(desc(n)) %>% arrange(year) %>% rename(lyricist=word) %>% 
  ggplot(aes(x=year,y=n,color=lyricist,group=lyricist))+#geom_line() + # group=singer important
  stat_smooth(method = "loess", formula = y ~ x, size = 1,se=FALSE)

####music_director

scol3 = songcol %>% mutate(music_director= str_replace_all(music_director," ","")) %>% select(music_director,rank,year) %>% unnest_tokens(word,music_director) %>% count(word) %>% top_n(5) %>% arrange(desc(n))

topmusicd = scol3$word # vector of lyricist

songcol %>% mutate(music_director= str_replace_all(music_director," ","")) %>% select(music_director,rank,year) %>% unnest_tokens(word,music_director)  %>% filter(word%in%topmusicd) %>% 
  group_by(year) %>% count(word) %>% arrange(desc(n)) %>% arrange(year) %>% rename(music_director=word) %>% 
  ggplot(aes(x=year,y=n,color=music_director,group=music_director))+#geom_line() + # group=singer important
  stat_smooth(method = "loess", formula = y ~ x, size = 1,se=FALSE)


###actors

scol4 = songcol %>% mutate(actors= str_replace_all(actors," ","")) %>% select(actors,rank,year) %>% unnest_tokens(word,actors) %>% count(word) %>% top_n(7) %>% arrange(desc(n))

topact = scol4$word # vector of lyricist

songcol %>% mutate(actors= str_replace_all(actors," ","")) %>% select(actors,rank,year) %>% unnest_tokens(word,actors)  %>% filter(word%in%topact) %>% 
  group_by(year) %>% count(word) %>% arrange(desc(n)) %>% arrange(year) %>% rename(actors=word) %>% 
  ggplot(aes(x=year,y=n,color=actors,group=actors))+#geom_line() + # group=singer important
  stat_smooth(method = "loess", formula = y ~ x, size = 1,se=FALSE)

# decades

seventies= songcol %>% filter(year>1970,year<1981) %>% mutate(actors= str_replace_all(actors," ","")) %>% select(actors,rank,year) %>% unnest_tokens(word,actors) %>% count(word) %>% top_n(6)
seventy = seventies$word

songcol %>% filter(year>1970,year<1981) %>% mutate(actors= str_replace_all(actors," ","")) %>% select(actors,rank,year) %>% unnest_tokens(word,actors) %>% filter(word%in%seventy) %>% 
  group_by(year) %>% count(word) %>% arrange(desc(n)) %>% arrange(year) %>% rename(actors=word)  %>% # at least greater than five songs a year bad,
  ggplot(aes(x=year,y=n,color=actors,group=actors))+#geom_line() + # group=singer important
  stat_smooth(method = "loess", formula = y ~ x, size = 1,se=FALSE)

# mean rank

songcol %>% select(singer,rank,year) %>% unnest_tokens(word,singer) %>% group_by(word) %>% summarize (mean_rank=mean(rank))  %>% arrange() %>% View()


########

g1984  = read_html("http://indpaedia.com/ind/index.php/Binaca_Geet_Mala_1984:_annual_list") %>% 
  html_node('table.MsoNormalTable') %>% html_table()

g1985  = read_html("http://indpaedia.com/ind/index.php/Binaca_Geet_Mala_1985:_annual_list") %>% 
  html_node('table.MsoNormalTable') %>% html_table()

g1986 =  read_html("http://indpaedia.com/ind/index.php/Cibaca_Geet_Mala_1986:_annual_list") %>% 
  html_node('table.MsoNormalTable') %>% html_table()

g1987 =  read_html("http://indpaedia.com/ind/index.php/Cibaca_Geet_Mala_1987:_annual_list") %>% 
  html_node('table.MsoNormalTable') %>% html_table()


g1987=g1987 %>% slice(-1) %>% mutate(rank=1:length(X1),year=rep(1987,length(X1)))%>% select(-X1) %>% select(rank,X2,X6,X4,X5,X3,year) %>% 
  rename(song=X2,singer=X6,music_director=X4,lyricist=X5,movie=X3)

g1984=g1984  %>% slice(-1) %>% mutate(rank=1:length(X1),year=rep(1984,length(X1)))%>% select(-X1) %>% select(rank,X2,X6,X4,X5,X3,year) %>% 
  rename(song=X2,singer=X6,music_director=X4,lyricist=X5,movie=X3)

g1985=g1985  %>% slice(-1) %>% mutate(rank=1:length(X1),year=rep(1985,length(X1)))%>% select(-X1) %>% select(rank,X2,X6,X4,X5,X3,year) %>% 
  rename(song=X2,singer=X6,music_director=X4,lyricist=X5,movie=X3)

g1986=g1986  %>% slice(-1) %>% mutate(rank=1:length(X1),year=rep(1986,length(X1)))%>% select(-X1) %>% select(rank,X2,X6,X4,X5,X3,year) %>% 
  rename(song=X2,singer=X6,music_director=X4,lyricist=X5,movie=X3)

g1988=read_html("http://indpaedia.com/ind/index.php/Cibaca_Geet_Mala_1988:_annual_list") %>% 
  html_node('table.MsoTableGrid') %>% html_table() %>% slice(-1) %>% mutate(rank=1:length(X1),year=rep(1988,length(X1)))%>% select(-X1) %>% select(rank,X2,X6,X4,X5,X3,year) %>% 
  rename(song=X2,singer=X6,music_director=X4,lyricist=X5,movie=X3)

g1989=read_html("http://indpaedia.com/ind/index.php/Cibaca_Sangeet_Mala_1989:_annual_list") %>% 
  html_node('table.MsoNormalTable') %>% html_table() %>% slice(-1) %>% mutate(rank=1:length(X1),year=rep(1989,length(X1)))%>% select(-X1) %>% select(rank,X2,X6,X4,X5,X3,year) %>% 
  rename(song=X2,singer=X6,music_director=X4,lyricist=X5,movie=X3)

g1990=read_html("http://indpaedia.com/ind/index.php/Cibaca_Sangeet_Mala_1990:_annual_list") %>% 
  html_node('table.MsoNormalTable') %>% html_table() %>% slice(-1) %>% mutate(rank=1:length(X1),year=rep(1990,length(X1)))%>% select(-X1) %>% select(rank,X2,X6,X4,X5,X3,year) %>% 
  rename(song=X2,singer=X6,music_director=X4,lyricist=X5,movie=X3)


g1991=read_html("http://indpaedia.com/ind/index.php/Cibaca_Sangeet_Mala_1991:_annual_list") %>% 
  html_node('table.MsoTableGrid') %>% html_table() %>% slice(-1) %>% mutate(rank=1:length(X1),year=rep(1991,length(X1)))%>% select(-X1) %>% select(rank,X2,X6,X4,X5,X3,year) %>% 
  rename(song=X2,singer=X6,music_director=X4,lyricist=X5,movie=X3)

g1992=read_html("http://indpaedia.com/ind/index.php/Cibaca_Geet_Mala_1992:_annual_list") %>% 
  html_node('table.MsoTableGrid') %>% html_table() %>% slice(-1) %>% mutate(rank=1:length(X1),year=rep(1992,length(X1)))%>% select(-X1) %>% select(rank,X2,X6,X4,X5,X3,year) %>% 
  rename(song=X2,singer=X6,music_director=X4,lyricist=X5,movie=X3)

g1993=read_html("http://indpaedia.com/ind/index.php/Cibaca_Geet_Mala_1993:_annual_list") %>% 
  html_node('table.MsoNormalTable') %>% html_table() %>% slice(-1) %>% mutate(rank=1:length(X1),year=rep(1993,length(X1)))%>% select(-X1) %>% select(rank,X2,X6,X4,X5,X3,year) %>% 
  rename(song=X2,singer=X6,music_director=X4,lyricist=X5,movie=X3)






eighties = list(g1984,g1985,g1986,g1987,g1988,g1989,g1990,g1991,g1992,g1993)

eighty =bind_rows(eighties)


### regex to clean columns

ex = '\'john-dayal/tom s.janaki etc. ? (movies) \''
ex
pattern= "/"

str_replace_all(ex,pattern," ") %>%  # replace pattern /
  str_replace_all("\\.","") %>%  # replace .
str_replace_all("\'","") %>% # replace '
  str_replace_all("-","") %>% #"-" replace
  str_replace_all("etc","") %>%  # replace etc.
  str_replace_all("\\?","") %>%  # ? replace
  str_replace_all(" *\\(.*?\\) *","")  # replace contents within parenthesis
# *? finds 0 or more spaces before (and after) the parentheses.
# Since ( and ) are special symbols in a regex, you need to escape these, i.e. (\\(
# .*? wildcard find to find all characters, where the ? means to find in a non-greedy way.


  songcol$movie %>%  str_replace_all(" *\\(.*?\\) *","") 
  
  putt="['-'/?]"

eighty$singer= eighty$singer %>% str_replace_all(putt,"") %>% str_replace_all("\\.","") %>% 
  str_replace_all("etc","") %>% 
  str_replace_all("\\\n","") %>%  # replace /n 
  str_replace_all(" *\\(.*?\\) *","") %>%  # replace contents in parenthesis
  str_replace_all(" ","")

eighty$lyricist= eighty$lyricist %>%  str_replace_all(putt,"") %>% str_replace_all("\\.","") %>% 
  str_replace_all("etc","") %>% 
  str_replace_all("\\\n","") %>%  # replace /n 
  str_replace_all(" *\\(.*?\\) *","") %>%  # replace contents in parenthesis
  str_replace_all(" ","")

eighty$movie = eighty$movie  %>%  str_replace_all(putt,"") %>% str_replace_all("\\.","") %>% 
  str_replace_all("etc","") %>% 
  str_replace_all("\\\n","") %>%  # replace /n 
  str_replace_all(" *\\(.*?\\) *","") %>%  # replace contents in parenthesis
  str_replace_all(" ","")

eighty$music_director=eighty$music_director  %>%  
  
  str_replace_all(putt,"") %>% str_replace_all("\\.","") %>% 
  str_replace_all("etc","") %>% 
  str_replace_all("\\\n","") %>%  # replace /n 
  str_replace_all(" *\\(.*?\\) *","") %>%  # replace contents in parenthesis
  str_replace_all(" ","") %>% 
  str_replace_all("-","") # to remove _



             
scol80 = eighty %>% select(singer,rank,year) %>% unnest_tokens(word,singer) %>% count(word) %>% top_n(5) %>% arrange(desc(n))

topsing80 = scol80$word # vector of top singers


eighty  %>% select(singer,rank,year) %>% unnest_tokens(word,singer) %>% filter(word%in%topsing80) %>% 
  group_by(year) %>% count(word) %>% arrange(desc(n)) %>% arrange(year) %>% rename(singer=word) %>% 
  ggplot(aes(x=year,y=n,color=singer,group=singer))+#geom_line() + # group=singer important
  stat_smooth(method = "loess", formula = y ~ x, size = 1,se=FALSE)


###lyricist

scol82 = eighty %>% mutate(lyricist= str_replace_all(lyricist," ","")) %>% select(lyricist,rank,year) %>% unnest_tokens(word,lyricist) %>% count(word) %>% top_n(5) %>% arrange(desc(n))

toplyrics1 = scol82$word # vector of lyricist

eighty %>% mutate(lyricist= str_replace_all(lyricist," ","")) %>% select(lyricist,rank,year) %>% unnest_tokens(word,lyricist) %>% filter(word%in%toplyrics1) %>% 
  group_by(year) %>% count(word) %>% arrange(desc(n)) %>% arrange(year) %>% rename(lyricist=word) %>% 
  ggplot(aes(x=year,y=n,color=lyricist,group=lyricist))+#geom_line() + # group=singer important
  stat_smooth(method = "loess", formula = y ~ x, size = 1,se=FALSE)



##association music_director n singer
songcol %>% select(year,singer,music_director) %>% unnest_tokens(word,singer) %>% rename(singer=word) %>% group_by(singer,music_director) %>% count(collaboration=n()) %>% arrange(desc(n)) %>% arrange(music_director) %>% View()

#

songcol %>% mutate(actors= str_replace_all(actors," ","")) %>% select(year,singer,actors) %>% unnest_tokens(word,singer) %>% rename(singer=word) %>%
  unnest_tokens(word,actors) %>% rename(actor=word) %>% 
  group_by(singer,actor) %>% count(collaboration=n()) %>% arrange(desc(n))  %>% View()


songcol %>% filter(lyricist=="guljar")
?str_detect

patty = "[gG]ul[zg]ar" # gulzar or gulzar or guljar or Gulajar
pattr= "[gG](.+)r" # will match starting with g (anything in between a) r , so gulzar,auhar,gulshan bawra etc

pattri= "\\b[gG](.+)r\\b" #here word boundary specified independent word hence will match either gulzar or gauhar not in between sentences
##\\b boundary word starts
#[gG] g or G
# (.+) any character 1 or more
# r 
# \\b word ends

str_subset(songcol$lyricist,patty)

songcol %>% filter(str_detect(lyricist,pattri)) %>% # using regex on filter 
  select(song,lyricist,music_director,singer,rank,year,movie) %>% View()
