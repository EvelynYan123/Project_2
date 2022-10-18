# question1-write a function Pone to estimate the probility of a prisoner succeeding in finding a card has his own number.
Pone <- function(n, k, strategy, nreps){
  N <- 2*n
  results = rep(0,nreps)
  if(strategy == 1){
    for(i in 1:nreps) {
      boxes = sample(1:N,N)
      prisoners = 1:N
      foundIt = 0
      for(prisoner in prisoners) {
        path = c(prisoner)
        tries = 1
        inBox = boxes[prisoner]
        while(tries < n) { 
          path = c(path, inBox) 			 			
          if(inBox == prisoner) { 				
            foundIt = foundIt + 1 				
            break; 			} 
          else { 					
            inBox = boxes[inBox] 			} 			
          tries = tries+1 		} 		 		
      }
      results[i] = foundIt
    }
  }else if(strategy == 2){
    for(i in 1:nreps) {
      boxes = sample(1:N,N)
      prisoners = 1:N
      foundIt = 0
      for(prisoner in prisoners) {
        path = c(sample(1:N,1))
        tries = 1
        inBox = boxes[path] 
        while(tries < n) { 
          path = c(path, inBox) 			 			
          if(inBox == prisoner) { 				
            foundIt = foundIt + 1 				
            break; 			} 
          else { 			
            inBox = boxes[inBox] 			} 			
          tries = tries+1 		} 		 		
      }
      results[i] = foundIt
    }
  }else if(strategy == 3){
    for(i in 1:nreps) {
      boxes = sample(1:N,N)
      prisoners = 1:N
      foundIt = 0
      for(prisoner in prisoners) {
        path = c(sample(1:N,n))
        tries = 1
        inbox = boxes[path]
        for(i in  1:n){
          if(inbox[i] == prisoner){
            foundIt = foundIt+1
          }
        }
      }
      results[i] = foundIt
    }  
  }
  prob = sum(results)/(2*n*nreps) 
  return(prob)
}

#question2-write a function Pall to estimate to estimate the probility of 2*n prisioners finding their cards have their own numbers.
Pall <- function(n, strategy, nreps){
  N <- 2*n
  results = rep(0,nreps)
  if(strategy == 1){
    for(i in 1:nreps) {
      boxes = sample(1:N,N)
      prisoners = 1:N
      foundIt = 0
      for(prisoner in prisoners) {
        path = c(prisoner)
        tries = 1
        inBox = boxes[prisoner]  
        while(tries < n) { 
          path = c(path, inBox) 			 			
          if(inBox == prisoner) { 				
            foundIt = foundIt + 1 				
            break; 			} 
          else { 					
            inBox = boxes[inBox] 			} 			
          tries = tries+1 		} 		 		
      }
      results[i] = foundIt
    }
  }else if(strategy == 2){
    for(i in 1:nreps) {
      boxes = sample(1:N,N)
      prisoners = 1:N
      foundIt = 0
      for(prisoner in prisoners) {
        path = c(sample(1:N,1))
        tries = 1
        inBox = boxes[path]
        while(tries < n) { 
          path = c(path, inBox) 			 			
          if(inBox == prisoner) { 				
            foundIt = foundIt + 1 				
            break; 			} 
          else { 			
            inBox = boxes[inBox] 			} 			
          tries = tries+1 		} 		 		
      }
      results[i] = foundIt
    }
  }else if(strategy == 3){
    for(i in 1:nreps) {
      boxes = sample(1:N,N)
      prisoners = 1:N
      foundIt = 0
      for(prisoner in prisoners) {
        path = c(sample(1:N,n))
        tries = 1
        inbox = boxes[path]
        for(i in  1:n){
          if(inbox[i] == prisoner){
            foundIt = foundIt+1
          }
        }
      }
      results[i] = foundIt 
    }
  }
  prob <- sum(results[which(results == N)])/(2*n*nreps)
  return(prob)
}

#question3-
#While n=5,the probability that a prisoner finds his own card when the first box he opened has a card number 2, and the probability that all 10 prisoners find their own card, under different strategies.
#strategy1
Pone(5,2,1,10000)
Pall(5,1,10000)
#strategy2
Pone(5,2,2,10000)
Pall(5,2,10000)
#strategy3
Pone(5,2,3,10000)
Pall(5,3,10000)
#while n=50,the probability that a prisoner finds his own card when the first box he opened has a card number 50, and the probability that all 100 prisoners find their own card, under different strategies.
#strategy1
Pone(50,50,1,10000)
Pall(50,1,10000)
#strategy2
Pone(50,50,2,10000)
Pall(50,2,10000)
#strategy3
Pone(50,50,3,10000)
Pall(50,3,10000)
#question4

# 5----------------------
n <- 50
N <- 2*n
# How many times should we run this experiment?
iters = 10
results = rep(0,iters) # 10个0

exp <- numeric(N)
mexp <- matrix(0,nrow=N,ncol = iters)
# strategy 1
for(i in 1:iters) {
  boxes = sample(1:N,N)

  prisoners = 1:N
  for(prisoner in prisoners) {

    path = c(prisoner)
    
    tries = 1

    inBox = boxes[prisoner]
    
    while(tries <= N) { 
      path = c(path, inBox) 			 			
      if(inBox == prisoner) { 				
        break; 			} 
      else { 				
        inBox = boxes[inBox] 			} 			
      tries = tries+1 		
      
      exp[prisoner] <- tries
      } 		 		
    
  }
  mexp[,i] <- exp


}
mexp

apply(mexp,2,mean)# k=1 - 10的时候length是多少








Pone <- function(n, k, strategy, nreps){ 
  N <- 2*n 
  results = rep(0,nreps) 
  if(strategy == 1){ 
    for(i in 1:nreps) {
      boxes = sample(1:N,N) 
      prisoners = 1:N 
      foundIt = 0 
      for(k in prisoners) { 
        path = c(k) 
        tries = 1 
        inBox = boxes[k]

while(tries < n) { 
  path = c(path, inBox) 			 			
  if(inBox == k) { 				
    foundIt = foundIt + 1 				
    break; 			} 
  else { 					
    inBox = boxes[inBox] 			} 			
  tries = tries+1 		} 		 		
}


results[i] = foundIt
}
}else if(strategy == 2){
  for(i in 1:nreps) {
  boxes = sample(1:N,N) 
  prisoners = 1:N 
  foundIt = 0 
  for(k in prisoners) { 
    path = c(sample(1:N,1)) 
    tries = 1 
    inBox = boxes[path]

while(tries < n) { 
  path = c(path, inBox) 			 			
  if(inBox == k) { 				
    foundIt = foundIt + 1 				
    break; 			} 
  else { 			
    inBox = boxes[inBox] 			} 			
  tries = tries+1 		} 		 		
}
results[i] = foundIt
}
}else if(strategy == 3){ 
  for(i in 1:nreps) { 
    boxes = sample(1:N,N) 
    prisoners = 1:N 
    foundIt = 0 
    for(prisoner in prisoners){ 
      path = c(sample(1:N,n)) 
      tries = 1 
      inbox = boxes[path] 
      for(i in 1:n){ 
        if(inbox[i] == prisoner){ 
          foundIt = foundIt+1 }
        } 
      } 
    results[i] = foundIt

}
}

prob = sum(results)/(2n*nreps)

return(prob) 
}

Pone(50,50,1, 1000) 
Pone(50,50,2, 1000) 
Pone(50,50,3, 1000)

Pall <- function(n, strategy, nreps){ 
  N <- 2*n 
  results = rep(0,nreps) 
  if(strategy == 1){ 
    for(i in 1:nreps) { 
    boxes = sample(1:N,N) 
    prisoners = 1:N 
    foundIt = 0 
    for(k in prisoners) { 
      path = c(k) 
      tries = 1 
      inBox = boxes[k]

while(tries < n) { 
  path = c(path, inBox) 			 			
  if(inBox == k) { 				
    foundIt = foundIt + 1 				
    break; 			} 
  else { 					
    inBox = boxes[inBox] 			} 			
  tries = tries+1 		} 		 		
}


results[i] = foundIt
}
} if(strategy == 2){ 
  for(i in 1:nreps) { 
  boxes = sample(1:N,N) 
  prisoners = 1:N 
  foundIt = 0 
  for(k in prisoners){ 
    path = c(sample(1:N,1)) 
    tries = 1 
    inBox = boxes[path]

while(tries < n) { 
  path = c(path, inBox) 			 			
  if(inBox == k) { 				
    foundIt = foundIt + 1 				
    break; 			} 
  else { 			
    inBox = boxes[inBox] 			} 			
  tries = tries+1 		} 		 		
}
results[i] = foundIt
}
} if(strategy == 3){ 
  for(i in 1:nreps) { 
    boxes = sample(1:N,N) 
    prisoners = 1:N 
    foundIt = 0 
    for(k in prisoners) { 
      path = c(sample(1:N,n))
      tries = 1 
      inbox = boxes[path] 
      for(i in 1:n){ 
        if(inbox[i] ==k){ 
          foundIt = foundIt+1 
          } 
        } 
      } 
    results[i] = foundIt

}
}

prob <- sum(results[which(results == N)])/(2n*nreps) 
return(prob) 
}

Pall(50,1, 1000) 
Pall(50,2, 1000) 
Pall(50,3, 1000)



