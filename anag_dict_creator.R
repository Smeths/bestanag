# Opening file and reading data
library(stringr)
setwd("/home/smeths/Documents/programming/rProgramming/BestAnagram")
words_con <- file("Web2.txt","r")
words <- readLines(words_con)
close(words_con)
# Number of words in the dictionary
length(words)
# listing all words in alphabetical order
norms <- lapply(str_split(words,""),sort)
norms <- lapply(norms,str_c,collapse="")
norms <- unlist(norms)
count <- rep(1,length(norms))
# Finding all norms that occur more than once and creating an anag data frame
anag <- data.frame(norms,words,count)
anag_count <- aggregate(count ~ norms, data = anag, sum)
anag_index <- anag_count$count > 1
anag_count <- anag_count[anag_index,]
norms_index <- anag$norms %in% anag_count$norms
anag <- anag[norms_index,]
ord <- order(anag$norms)
anag <- anag[ord,]
anag$length <- nchar(as.character(anag$words))
# relevelling the factors and removing the count column
anag$norms <- as.factor(as.character(anag$norms))
anag <- anag[,c(1,2,4)]
# finding the number of each anagram and adding to anag data frame
anag_num <- as.data.frame(table(anag$norms))
names(anag_num) <- c("norms","count")
anag <- merge(anag_num,anag)
num_anags <- c(sum(anag$count==2),
               sum(anag$count==3),
               sum(anag$count==4),
               sum(anag$count==5),
               sum(anag$count==6),
               sum(anag$count==7),
               sum(anag$count==8),
               sum(anag$count==9),
               sum(anag$count==10))
plot(c(2,3,4,5,6,7,8,9,10),num_anags)

# finding more than 2 anagrams
anag_3plus_unique <- unique(as.character(anag[anag$count>2,]$norms))
anag_pairs <- data.frame(norms=factor(),
                         count=integer(),
                         words=factor(),
                         length=integer())
length(anag_3plus_unique)
for (i in 1:length(anag_3plus_unique)) {
  print(i)
  anags <- anag[anag$norms==anag_3plus_unique[i],]
  loopinnerend <- nrow(anags) 
  loopouter <- loopinnerend - 1
  for(j in 1:loopouter){
    loopinnerstart <- j + 1
    for(k in loopinnerstart:loopinnerend){
      anag_pairs <- rbind(anag_pairs, anags[j,])
      anag_pairs <- rbind(anag_pairs, anags[k,])
    }
  }
}
anag_dict <- rbind(anag[anag$count==2,],anag_pairs)
write.csv(anag_dict,"anag_dict.csv")
























