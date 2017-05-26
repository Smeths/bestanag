anags <- read.csv("anag_dict.csv")
anags$seccount <- 0
anags$sec <- ""
number_pairs <- dim(anags)[1]

for(i in seq(1,number_pairs,2)){
  wordA <- as.character(anags$words[i])
  wordB <- as.character(anags$words[i+1])
  sec <- section_count(wordA,wordB)
  anags$sec[i] <- sec$section
  anags$seccount[i] <- sec$count
  anags$sec[i+1] <- sec$section
  anags$seccount[i+1] <- sec$count
}

write.csv(anags,"anag_dict_seccount.csv")