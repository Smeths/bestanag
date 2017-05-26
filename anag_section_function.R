section_count <- function(wordA = "evilnextdoors",wordB = "nextlivedoors"){
  
  lenstr <- nchar(wordA)
  
  # Converting character string to integer vector
  
  alpha <- "abcdefghijklmnopqrstuvwxyz"
  vecA <- c(30,str_locate(alpha,unlist(str_split(wordA,"")))[,1],31)
  vecB <- c(40,str_locate(alpha,unlist(str_split(wordB,"")))[,1],41)
  lenvec <- length(vecA)
  countmax <- matrix(rep(0,lenvec),rep(0,lenvec),nrow=2,ncol=lenvec)
  seccount <- 0
  insert <- 50
  sec <- ""
  
  # Cyclying over first word
  while (lenstr > 0) {
    for (i in 1:lenvec) {
      vectest <- c(vecA[i:lenvec], vecA[1:i - 1])
      vecdiff <- abs(vectest - vecB)
      count <- 0
      for (j in 1:lenvec) {
        if (vecdiff[j] == 0) {
          count = count + 1
          if (count == 1) pos <- j
          if (j == length(lenvec) && count > countmax[1,i]) {
            countmax[1,i] <- count
          }
        }
        else {
          if (count > countmax[1,i]) {
            countmax[,i] <- c(count,pos)
          }
          count <- 0
          pos <- 0
        }
      }
    }
    
    # finding matching section
    
    offset <- which(countmax[1,] == max(countmax[1,]))[1]
    vectest <- c(vecA[offset:lenvec], vecA[1:offset - 1])
    match1 <- 1:(countmax[2,offset]-1)
    match2 <- (countmax[2,offset] + countmax[1,offset]):lenvec
    match <- countmax[2,offset]:(countmax[2,offset] + countmax[1,offset]-1)
    secnew <- str_c(str_sub(alpha,vecB[match],vecB[match]),collapse ="")
    
    # updating variable  
    
    vecA <- c(vectest[match1],insert,vectest[match2])
    insert <- insert + 1
    vecB <- c(vecB[match1],insert,vecB[match2])
    insert <- insert + 1
    lenvec <- length(vecA)
    lenstr <- lenstr - countmax[1,offset]
    countmax <- matrix(rep(0,lenvec),rep(0,lenvec),nrow=2,ncol=lenvec)
    seccount <- seccount + 1
    
    # generating section string
    sec <- str_c(sec,as.character(seccount),c(" "),secnew,c(" "))
  }
  sections <- list(section = sec,count = seccount)
  return(sections)
}
