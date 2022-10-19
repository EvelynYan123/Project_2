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
#question4-some interesting finds
#according to our example in question 3, we find that in both functions Pone and Pall, the probability relationship of different strategies is P(strategy1) > P(strategy2) > P(strategy3)
#for a single prisoner, the larger n is, the greater the probility that he can find the corresponding number.
#The larger n is , the greater the probality that all 2*n prisoners can succeed in finding their number.




#function Pone
#use three functions to define 3 strategies first? We can simplify our probability functions. Some bugs exist, Pone function does not work.
strategy1 <- function(n,boxes,prisoner) {
  path <- prisoner
  for(i in 1:n) {
    if(i==1) {
      next.box <- prisoner
    } else {
      next.box <- boxes[last.box]
    }
    if(boxes[next.box]==i) return(TRUE)
    last.box <- next.box
  }
  return(FALSE) 
}

strategy2 <- function(n,boxes,prisoner) {
  path = c(sample(1:2*n,1))
  for(i in 1:n) {
    if(i==1) {
      next.box <- prisoner
    } else {
      next.box <- boxes[last.box]
    }
    if(boxes[next.box]==i) return(TRUE)
    last.box <- next.box
  }
  return(FALSE) 
}

strategy3 <- function(n,boxes,prisoner){
  selected_boxes <- sample(2*n,n)
  inbox >- boxes[selected_boxes]
  inbox
  if (prisoner %in% inbox) return(TRUE)
  return(FALSE)
}

strategy_selected <- function(strategy){
  if (strategy == 1){
    strategy = strategy1
  } 
  else if (strategy == 2){
    strategy = strategy2
  }
  else {
    strategy = strategy3
  }
  return(strategy)
}

Pone <- function(n,k,strategy,nerps){
  strategy = strategy_selected(strategy)
  results = rep(0,nreps)
  N <- 2*n
  boxes = sample(1:N,N)
  prisoners = 1:N
  foundIt = 0
  for(i in 1:nreps) {
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
  prob = sum(results)/(2*n*nreps) 
  return(prob)
}



dloop <- function(n, nreps){
  count_0 = rep(0, n*2)
  
  for (reps in 1:nreps){
    count_1 = rep(0, n*2)
    boxes = sample(n*2,n*2)
    numbered_boxes <- boxes
    
    iteration_num = c()
    for (k in 1:(2*n)){
      cycles = c()
      if ((k %in% iteration_num) == FALSE){
        cycles = append(cycles, k) 
        next_box = boxes[k]
        while (next_box != k){
          cycles = append(cycles, next_box)
          next_box =numbered_boxes[next_box]
        }
        count_1[length(cycles)]=count_1[length(cycles)]+1
        iteration_num = append(iteration_num, cycles)
      }
    }
    index_0 = grep(0, count_1)
    count_0[index_0]=count_0[index_0]+1
  }
  prob_0 <- count_0/nreps
  prob_1 <- 1- prob_0 #probability of an each loop 1:2n at least once
  return(prob_1)
}
dloop(100, 10000)       
