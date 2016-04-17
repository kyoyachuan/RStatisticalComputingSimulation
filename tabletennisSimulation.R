onegame<-function(A,B,C,D,E,F,G,H,serving){
  scoresA<-0
  scoresB<-0
  i<-NULL
  while(scoresA < 21 & scoresB < 21){
    if(serving == "A"){
      userv1<-runif(1)
      if(userv1 > C){scoresB<-scoresB+1; i<-c(i,1)
      if(length(i) == 2){i<-NULL; serving<-"B"}}
      else{
        repeat{
          u1<-runif(1)
          u2<-runif(1)
          usmash1<-runif(1)
          usmash2<-runif(1)
          if(usmash1 <= E){
            if(u2 > H){scoresA<-scoresA+1; i<-c(i,1)
            if(length(i) == 2){i<-NULL; serving<-"B"}
            break}
          }
          else{
            if(u2 > B){scoresA<-scoresA+1; i<-c(i,1)
            if(length(i) == 2){i<-NULL; serving<-"B"}
            break}
          }
          if(usmash2 <= F){
            if(u1 > G){scoresB<-scoresB+1; i<-c(i,1)
            if(length(i) == 2){i<-NULL; serving<-"B"}
            break}
          }
          else{
            if(u1 > A){scoresB<-scoresB+1; i<-c(i,1)
            if(length(i) == 2){i<-NULL; serving<-"B"}
            break}
          }
        }
      }
    }
    else if(serving == "B"){
      userv2<-runif(1)
      if(userv2 > D){scoresA<-scoresA+1; i<-c(i,1)
      if(length(i) == 2){i<-NULL; serving<-"A"}}
      else{
        repeat{
          u1<-runif(1)
          u2<-runif(1)
          usmash1<-runif(1)
          usmash2<-runif(1)
          if(usmash2 <= F){
            if(u1 > G){scoresB<-scoresB+1; i<-c(i,1)
            if(length(i) == 2){i<-NULL; serving<-"A"}
            break}
          }
          else{
            if(u1 > A){scoresB<-scoresB+1; i<-c(i,1)
            if(length(i) == 2){i<-NULL; serving<-"A"}
            break}
          }
          if(usmash1 <= E){
            if(u2 > H){scoresA<-scoresA+1; i<-c(i,1)
            if(length(i) == 2){i<-NULL; serving<-"A"}
            break}
          }
          else{
            if(u2 > B){scoresA<-scoresA+1; i<-c(i,1)
            if(length(i) == 2){i<-NULL; serving<-"A"}
            break}
          }
        }
      }
    }
  }
  if(scoresA == 20 | scoresB == 20){
    while(scoresA < scoresB+2 & scoresB < scoresA+2){
      if(serving == "A"){
        userv1<-runif(1)
        if(userv1 > C){scoresB<-scoresB+1; i<-c(i,1)
        if(length(i) == 1){i<-NULL; serving<-"B"}}
        else{
          repeat{  
            u1<-runif(1)
            u2<-runif(1)
            usmash1<-runif(1)
            usmash2<-runif(1)
            if(usmash1 <= E){
              if(u2 > H){scoresA<-scoresA+1; i<-c(i,1)
              if(length(i) == 1){i<-NULL; serving<-"B"}
              break}
            }
            else{
              if(u2 > B){scoresA<-scoresA+1; i<-c(i,1)
              if(length(i) == 1){i<-NULL; serving<-"B"}
              break}
            }
            if(usmash2 <= F){
              if(u1 > G){scoresB<-scoresB+1; i<-c(i,1)
              if(length(i) == 1){i<-NULL; serving<-"B"}
              break}
            }
            else{
              if(u1 > A){scoresB<-scoresB+1; i<-c(i,1)
              if(length(i) == 1){i<-NULL; serving<-"B"}
              break}
            }
          }
        }
      }
      else if(serving == "B"){
        userv2<-runif(1)
        if(userv2 > D){scoresA<-scoresA+1; i<-c(i,1)
        if(length(i) == 1){i<-NULL; serving<-"A"}}
        else{
          repeat{
            u1<-runif(1)
            u2<-runif(1)
            usmash1<-runif(1)
            usmash2<-runif(1)
            if(usmash2 <= F){
              if(u1 > G){scoresB<-scoresB+1; i<-c(i,1)
              if(length(i) == 1){i<-NULL; serving<-"A"}
              break}
            }
            else{
              if(u1 > A){scoresB<-scoresB+1; i<-c(i,1)
              if(length(i) == 1){i<-NULL; serving<-"A"}
              break}
            }
            if(usmash1 <= E){
              if(u2 > H){scoresA<-scoresA+1; i<-c(i,1)
              if(length(i) == 1){i<-NULL; serving<-"A"}
              break}
            }
            else{
              if(u2 > B){scoresA<-scoresA+1; i<-c(i,1)
              if(length(i) == 1){i<-NULL; serving<-"A"}
              break}
            }
          }
        }
      }
    }
  }
  return(c(scoresA,scoresB))
}

simNgames<-function(n,A,B,C,D,E,F,G,H){
  winsA<-0
  winsB<-0
  for(i in 1:n){
    if(i%%2 == 0){
      serving<-"A"
      mark<-onegame(A,B,C,D,E,F,G,H,serving)
      if (mark[1] > mark[2]){winsA<-winsA+1}
      else {winsB<-winsB+1}
    }
    else{
      serving<-"B"
      mark<-onegame(A,B,C,D,E,F,G,H,serving)
      if (mark[1] > mark[2]){winsA<-winsA+1}
      else {winsB<-winsB+1}
    }
  }
  return(c((winsA/n),(winsB/n)))
}


