# Binaca geetmala scrape
# http://www.hindigeetmala.net/binaca_geetmala_1958.htm
library(rvest)


songlist = read_html("http://www.hindigeetmala.net/binaca_geetmala_1958.htm") # i made an error initially dont push empty strings

# html_text is different from html_table
# https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/

# on inspecting elements we see two table element .w185 and .w105
# on inspecting element i found w760

Text1 = songlist %>%
  html_nodes(".w185") %>% html_text()  # Put a comma between elements

Text1

Text2 = songlist %>%
  html_nodes(".w105") %>% html_text()

Text2

xpath ='//*[@id="c1"]/table[2]/tbody/tr/td[4]/table[4]/tbody/tr/td[2]/table[1]/tbody/tr/td/table[4]'
Tablenu = songlist %>%
  html_nodes("w760 fwb") %>% html_table()

Tablepref = songlist %>%
  html_nodes("table.b1.w760.pad2.allef") %>% html_table()




# Removing a column
#http://stackoverflow.com/questions/4605206/drop-data-frame-columns-by-name
#within(df, rm(x, y))
# DT[, c('a','b') := NULL]
# drop.cols <- c('Sepal.Length', 'Sepal.Width')
# iris %>% select(-one_of(drop.cols))
# dropping row
# myData <- myData[-c(2, 4, 6), ]
# Renaming headers
# colnames(DF) = DF[1, ]



str(Tablepref)
Tablepref[1]

Tablepref[2]
Table11 = as.data.frame(Tablepref[1])
Table12 = as.data.frame(Tablepref[2])

Tablepref[3]

# Merge data frames having same column how ?
# http://www.statmethods.net/management/merging.html
# here we bind by rows
total <- rbind(Table11,Table12) 

Tableheader = songlist %>%
  html_nodes("table.w760.fwb") %>% html_table()

Header = as.data.frame(Tableheader[1])

# Removing a column
#http://stackoverflow.com/questions/4605206/drop-data-frame-columns-by-name
#within(df, rm(x, y))
# DT[, c('a','b') := NULL]
# drop.cols <- c('Sepal.Length', 'Sepal.Width')
# iris %>% select(-one_of(drop.cols))
# dropping row
# myData <- myData[-c(2, 4, 6), ]
# Renaming headers
# colnames(DF) = DF[1, ]




Header = subset(Header,select =-c(X1,X9))

colnames(total) = Header[1,]

total$`Song Heading`

# our goal is to do a regex and remove last column 4.28 - 186 votes
# Here we can remove all characters after first number appears
# http://stackoverflow.com/questions/7185071/removing-certain-string-pattern-in-r
# https://www.r-bloggers.com/regular-expressions-in-r-vs-rstudio/


H = sapply(strsplit(total$`Song Heading`,"/d",fixed = TRUE),"[[",1)
H = gsub("votes","",H)
H =  gsub ("-","",H)

H = gsub("\\d", "", H) # gsub('[[:digit:]]+', '', x) , gsub('[0-9]+', '', x)

H
# Extract first four characters
# http://stat545.com/block022_regular-expression.html
# http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
library(stringr)
G = word(H,1,5) # Extract from 1 to 5, read sanchez guide very good
# using youtube API
# https://www.analyticsvidhya.com/blog/2014/09/mining-youtube-python-social-media-analysis/
library(devtools)
devtools::install_github("soodoku/tuber", build_vignettes = TRUE)
apikey = c("AIzaSyCppcUGi2VqhT8hVMkAgaw10KXXmGj6k64")
appid = c("binacayoutube-147314")
clientid = c("1087391996437-lbu9k1hokr4qk9c8c0pqhrk7ktanlepo.apps.googleusercontent.com")
secret = c("Ye31S_-mMiCOiRHnj0dNihZd")
yt_oauth("1087391996437-lbu9k1hokr4qk9c8c0pqhrk7ktanlepo.apps.googleusercontent.com", "Ye31S_-mMiCOiRHnj0dNihZd")
G
a = yt_search(term="Obama", max_results = 2)
# url=“https://www.googleapis.com/youtube/v3/videos?q=ukrainian+protests&alt=json”


#https://www.googleapis.com/youtube/v3/search?part=id&q=ukraine&type=video&key=AIzaSyCppcUGi2VqhT8hVMkAgaw10KXXmGj6k64
# GET https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=10&order=date&type=video&key={AIzaSyCppcUGi2VqhT8hVMkAgaw10KXXmGj6k64}

# source code of function
# http://stackoverflow.com/questions/19226816/how-can-i-view-the-source-code-for-a-function

library(tuber)
getMethod(tuber::get_stats)
get_comments
tuber_GET
#Rcurl method
library(RCurl)
#query to get video id
#Make G vector amenable for query
songvec = gsub("","+",G)
songvec[1]

library(stringr)
#  http://stackoverflow.com/questions/13985215/replace-special-characters-along-with-the-space-in-list-of-strings)
song = gsub('([[:punct:]])|\\s+','+',G)
str(song)
song[2]

length(song)


for(i in length(song))
{print(song[i])}

for ( i in length(song) )
{
target4 = paste0("https://www.googleapis.com/youtube/v3/search?part=id&q=",song,"&maxResults=1&type=video&key=AIzaSyCppcUGi2VqhT8hVMkAgaw10KXXmGj6k64")


}
#http://stackoverflow.com/questions/37576798/r-jsonlite-loop-issue


doc = lapply(target4,fromJSON)

b = fromJSON(target1)
b$items
C = b$items$id
D = C$videoId

doc2 = unlist(doc)
str(doc2)
jsonlist = function (x)
{
 
  a1 = fromJSON(x)
  
  a2 = a1$items$id
  a2 = a2$videoId
  
}

song2id = function(x){
target5 = paste0("https://www.googleapis.com/youtube/v3/search?part=id&q=",x,"&maxResults=1&type=video&key=AIzaSyCppcUGi2VqhT8hVMkAgaw10KXXmGj6k64")
g1 = fromJSON(target5)

g2 = g1$items$id
g3 = g2$videoId

}

id2stats = function {
target6 = paste0("https://www.googleapis.com/youtube/v3/videos?part=statistics&id=",g3,"&key=AIzaSyCppcUGi2VqhT8hVMkAgaw10KXXmGj6k64")
E1 = fromJSON(target6)

# Make a new data frame
E2 = E$items
# Drop few columns

E2 = within(E2, rm(kind, etag))

# Rename rest
#http://stackoverflow.com/questions/20987295/rename-multiple-columns-by-names



}

miscell1 = lapply(song, song2id)
miscell2 = lapply(miscell1,get_stats)

miscell3 = as.data.frame(miscell2)

# converting lists of lists to data frame
# http://stackoverflow.com/questions/29674661/r-list-of-lists-to-data-frame
miscell4  <-  as.data.frame(matrix(unlist(miscell2), nrow=length(unlist(miscell2[1]))))

# Now transpose row to columns
# https://www.r-statistics.com/tag/transpose/

miscell5 = t(miscell4)

colnames(miscell5) = c("views","Likes","Dislikes","Favorites","comments") 

# https://www.google.co.in/search?q=place+a+column+at+first+in+data+frame&ie=utf-8&oe=utf-8&client=firefox-b-ab&gfe_rd=cr&ei=PEHeV_LxPIjT8gey4oqgBw
miscell1 = unlist(miscell1)

miscell6 = cbind(miscell1,miscell5)
miscell7 = cbind(total$`Song Heading`,miscell6)
miscell7 = as.data.frame(miscell7)

row.names(miscell7)
nrow(miscell7)
`miscell8 = row.names<-`(miscell7,1:nrow(miscell7))) # doesnt work

#http://stackoverflow.com/questions/18803354/ending-prompt-in-r

miscell8 = as.data.frame(miscell7,row.names = 1:nrow(miscell7))

# renmaing again
#http://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame-in-r
names(data)[names(data) == "oldVariableName"] <- "newVariableName"
library(dplyr)
miscell8 = rename(miscell8, Songheading = V1, ID = miscell1)

# miscell8 has old song headings replace with H
miscell8$Songheading<- H # Total rplaces

names(iris)



str(miscell4)


str(mis1)





hrh =jsonlist(target4[1])
hrh


target = paste0("https://www.googleapis.com/youtube/v3/videos?part=statistics&id=",D,"&key=AIzaSyCppcUGi2VqhT8hVMkAgaw10KXXmGj6k64")
target

E = fromJSON(target)
fromJSON(rd)
# Make a new data frame
F = E$items
# Drop few columns

F = within(F, rm(kind, etag))

# Rename rest
#http://stackoverflow.com/questions/20987295/rename-multiple-columns-by-names
str(F)
F1 = F$id
F2 = F$statistics
#F2
#str(F2)
F2$id = F1
F2
# reorder column names
#http://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame
F2 <- F2[c(6,1,2,3,4,5)]
colnames(F2) = c("id","views","Likes","Dislikes","Favorites","comments") 
F2[i]

}





get_stats(video_id = D)

get_stats(video_id = "cvorg5tCmlQ" )
# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html



#


stock = read_html("http://www.moneycontrol.com/stocks/hist_index_result.php?indian_indices=4")

stocktab = stock %>%
  html_nodes("div.MT12") %>% html_table()

