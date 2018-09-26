

for (libreria in c("stringr","tm", "R.utils")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}


TwitterData <- "./data/en_US.twitter.txt"
Twitter <- scan(TwitterData, character(0), sep = "\n") 
lineasTwitter<-as.numeric(countLines(TwitterData))


NewsData <- "./data/en_US.news.txt"
Twitter <- scan(NewsData, character(0), sep = "\n") 
lineasNews<-as.numeric(countLines(NewsData))


BlogsData <- "./data/en_US.blogs.txt"
Twitter <- scan(BlogsData, character(0), sep = "\n") 
lineasBlogs<-as.numeric(countLines(BlogsData))



