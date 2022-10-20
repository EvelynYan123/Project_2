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




set.seed(1)
Pone <- function(n, k, strategy, nreps){  #A function to calculate the individual probability of a prisoner finding his number given number of tries, the prisoner number,the strategy and number of iterations
  N <- 2*n #number of prisoners
  results = rep(0,nreps)  #initialise results
  if(strategy == 1){ #The prisoner starts at the box with their number on it, opens it and reads the number on the card: k, say. If k
                     #is not their prisoner number, they go to box number k, open it and repeat the process until they have either
                     #found the card with their number on it, or opened n boxes without finding it.
    for(i in 1:nreps) {
      boxes = sample(1:N,N) #sample boxes
      prisoners = 1:N  #we have N =2*n prisoners
      foundIt = 0    #initialise number of prisoners finding their number to 0
      for(k in prisoners) {  #loop over the prisoners  
        path = c(k)  
        tries = 1
        inBox = boxes[k]  #a prisoner chooses the box with his number on it
        while(tries <= n) { #prisoner can open at most 50 boxes
          path = c(path, inBox) 			 			
          if(inBox == k) { 		#if the number in the box coincides with the number of a prisoner before opening more than 50 boxes he is done 		
            foundIt = foundIt + 1 	#we count how many prsioners found their numbers			
            break; 			} 
          else { 		               #if the number of a prisoner does not coincide with the number in the box			
            inBox = boxes[inBox] 			} 	#the prisoner opens the next box with the number on it equal to the card number in current box 	
          tries = tries+1 		} 	#since the prisoner did not find his number yet we add to tries	 		
      }
      results[i] = foundIt 	#display overall results of prioners finding their numbers
    }
  }else if(strategy == 2){   
    for(i in 1:nreps) {
      boxes = sample(1:N,N) #sample boxes
      prisoners = 1:N #we have N =2*n prisoners
      foundIt = 0 #initialise number of prisoners finding their number to 0
      for(k in prisoners) { #loop over the prisoners
        path = c(sample(1:N,1)) #The prisoner starts with a randomly selected box
        tries = 1 
        inBox = boxes[path] #prisoner opens a randomly selected box
        while(tries <= n) { #track tries of a prisoner (the prisoner should find his number before opening at most 50 boxes)
          path = c(path, inBox)   #track the path of a prisoner			 			
          if(inBox == k) { 	#check if the number of a card inside	of a box coincides with the prisoner's number		
            foundIt = foundIt + 1 	#if yes add one to the number of prisoners who found their number before opening more than 50 boxes			
            break; 			} 
          else { 			
            inBox = boxes[inBox] 			} 	#if not then pass this number to the next box		
          tries = tries+1 		} 		 		#adding one to tries
      }
      results[i] = foundIt #keep track of all prisoners finding their numbers
    }
  }else if(strategy == 3){ 
    for(i in 1:nreps) {
      boxes = sample(1:N,N) #sample boxes
      prisoners = 1:N #we have N = 2*n prisoners
      foundIt = 0   #initialise number of prisoners finding their number to 0
      for(k in prisoners) { #loop over prisoners
        path = c(sample(1:N,n)) #sample of n randomly selected  boxes 
        tries = 1
        inbox = boxes[path] #prisoner opens a randomly selected box 
        for(i in  1:n){ #loop over 50 random boxes
          if(inbox[i] == k){  #check if the card number inside of the box coincides with the prisoner's number
            foundIt = foundIt+1 #if yes add one to the number of prisoners who found their number before opening more than 50 boxes	
          }
        }
      }
      results[i] = foundIt #keep track of all prisoners finding their numbers
    }  
  }
  prob = sum(results)/(2*n*nreps) #probability of a single prisoner succeeding in finding their number
  return(prob)
}

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
  }else if(strategy == 2){
    for(i in 1:nreps) {
      boxes = sample(1:N,N)
      prisoners = 1:N
      foundIt = 0
      for(k in prisoners) {
        path = c(sample(1:N,1))
        tries = 1
        inBox = boxes[path]
        while(tries <= n) { 
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
      for(k in prisoners) {
        path = c(sample(1:N,n))
        tries = 1
        inbox = boxes[path]
        for(i in  1:n){
          if(inbox[i] == k){
            foundIt = foundIt+1
          }
        }
      }
      results[i] = foundIt 
    }
  }
  prob <- mean(results == N) #probability that all prisoners find their numbers
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
l <- dloop(50, 10000) 
l

a <- hist(l, col = 'blue', main = paste("Histogram of Probabilities") ) #displaying histogram of probabilities

prob <- dloop(50, 10000) 
barplot(prob,ylab = 'probability',xlab = 'loop length',col = 'blue',main = 'probability of each loop length from 1 to
        2n occurring at least once') #visualise the probabilities sensibly


Pall <- function(n, k, strategy, nreps){# this one decreases the running time, but not so much:(
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
  }else{
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
  
  prob = length(which(results==2*n))/nreps
  
  return(prob)
}



