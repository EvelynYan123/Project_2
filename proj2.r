Pall <- function(n, strategy, nreps){
  N <- 2*n
  results = rep(0,nreps)
  if(strategy != 3){
    for(i in 1:nreps) {
      boxes = sample(1:N,N)
      prisoners = 1:N
      foundIt = 0
      for(prisoner in prisoners) {
        tries = 1
        
        if(strategy == 1){
        path = c(prisoner)
        inBox = boxes[prisoner]
        }else if(strategy == 2){
          path = c(sample(1:N,1))
          inBox = boxes[path]
        }
        
        while(tries <= n) { 
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
      prisoners <- 1:N
      temp <- numeric(N)
      
      for(j in prisoners){
        inboxes <- sample(1:N,N)[1:n]
        temp[j] <- match(j,inboxes)
      }
      temp <- na.omit(temp)
      results[i] <- N-length(attributes(temp)$na.action)
    }
    
    
  }
  
  prob = length(which(results==2*n))/nreps
  
  return(prob)
}


Pall(50,1,1000)
Pall(50,2,1000)
Pall(50,3,1000)

Pall(5,1,1000)
Pall(5,2,1000)
Pall(5,3,1000)

   ty of a single prisoner succeeding in finding their number
  return(prob)
}


success_prob <- function(n,nerps){
  for (strategy in 1:3){
    cat('In strategy', strategy,':',fill = TRUE)
    cat(' P(a single prisoner success)=', {Pone(n,1,strategy,nerps)},fill = TRUE)
    cat(' P(all prisoner success)=', {Pall(n,strategy,nerps)},fill = TRUE)
  }
}
success_prob(5,10000)
success_prob(50,10000)
# In strategy 1, the probability of all prisoners get free is 30.63%, but in other two strategies, the probabilities that they will be free are 0.





dloop <- function(n, nreps){ #creating the function dloop that takes number of boxes and number of iterations
  count_0 = rep(0, n*2) # initialise vector of length 2*n
  
  for (reps in 1:nreps){ #loop over nreps
    count_1 = rep(0, n*2) #initialise vector counting length of a loop 
    boxes = sample(n*2,n*2) #sample boxes 
    numbered_boxes <- boxes 
    
    iteration_num = c() #initialising vector of numbers we will perform iterations over
    for (k in 1:(2*n)){ #loop over 2*n boxes 
      closed_loops = c() 
      if ((k %in% iteration_num) == FALSE){  #check if k coincides with the numbers over which we perform iterations
        closed_loops = append(closed_loops, k) #if not then append this value to that loop
        next_box = boxes[k] #follow the prisoner's number in next box
        while (next_box != k){ #we append the next boxes to the cycle until in one of the boxes we find the prisoner's number
                               #if we find the number the cycle is closed and we are done
          closed_loops = append(closed_loops, next_box)
          next_box =numbered_boxes[next_box]
        }
        count_1[length(closed_loops)]=count_1[length(closed_loops)]+1 # count  number of boxes in cycles
        iteration_num = append(iteration_num, closed_loops)
      }
    }
    index_0 = grep(0, count_1)
    count_0[index_0]=count_0[index_0]+1
  }
  prob_0 <- count_0/nreps #probability of a non-repeated loop 
  prob_1 <- 1- prob_0 #probability of an each loop 1:2n at least once
  return(prob_1)
}

prob <- dloop(50, 10000) 
barplot(prob,ylab = 'probability',xlab = 'loop length',col = 'blue',main = 'probability of each loop length from 1 to
        2n occurring at least once') #displaying histogram of probabilities

1-sum(prob[51:100])


Pone <- function(n,k, strategy, nreps){
  N <- 2*n
  results = rep(0,nreps)
  if(strategy != 3){
    for(i in 1:nreps){
      boxes = sample(1:N,N)
      foundIt = 0
      tries = 1
      if(strategy == 1){
      path = c(k)
      inBox = boxes[k]
      }else if(strategy == 2){
      path = c(sample(1:N,1))
      inBox = boxes[path]
      }
      while(tries < n) { 			 			
        path = c(path, inBox) 			 			
        if(inBox == k) { 				
          foundIt = foundIt + 1 				
          break; 		
        } else { 				
          inBox = boxes[inBox] 			} 		
        tries = tries+1 		} 		 		
      results[i] = foundIt
      
    }
  }else if(strategy == 3){
    for(i in 1:nreps) {
      prisoners <- 1:N
      temp <- numeric(N)
      
      for(j in prisoners){
        inboxes <- sample(1:N,N)[1:n]
        temp[j] <- match(j,inboxes)
      }
      temp <- na.omit(temp)
      results[i] <- N-length(attributes(temp)$na.action)
    }
    
    
  }
  
  prob = sum(results)/(2*n)
  
  return(prob)
}






