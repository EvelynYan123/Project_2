


#running time check
start <- Sys.time()

# Question 1

Pone <- function(n,k,strategy, nreps){ #A function to calculate the probability that all prisoners find their numbers 
  #given number of tries, the strategy and number of iterations
  N <- 2*n 
  results = rep(0,nreps)  #initialise vector of results
  if(strategy != 3){  #for strategies except strategy 3 (that is, 1 and 2)
    for(i in 1:nreps) {  #loop over nreps
      boxes = sample(1:N,N) #sample boxes
     #we have 2*n prisoners
      foundIt = 0 #initialise number of prisoners who found their numbers
      #loop over prisoners
        tries = 1
        if(strategy == 1){  #consider a strategy 1
          path = c(k) #keep track of the path of a prisoner
          inBox = boxes[k] # prisoner opens a box with his number
        }else if(strategy == 2){ #consider a strategy 2
          path = c(sample(1:N,1)) #sample random box  
          inBox = boxes[path]  #firstly prisoner opens a random box
        }
        while(tries <= n) { #prisoners can open at most 50 boxes
          path = c(path, inBox) 		#keep track of a path	 			
          if(inBox == k) { 	#check if the number inside of the box coincides with the prisoner's number		
            foundIt = foundIt + 1   #if yes then update foundIt
            break; 			} 
          else { 					
            inBox = boxes[inBox] 			} #if no, then follow this number in the next box
          tries = tries+1 		} 		 		#update number of tries
      
      results[i] = foundIt  #how many prisoners found their numbers
    }
  }else{ #strategy3
    for(i in 1:nreps) {
      inboxes <- sample(1:N,N)[1:n]
      results[i] <- match(k,inboxes)
      results <- length(results[which(is.na(results)== FALSE)])
    }       
  }
  
  prob = sum(results)/(nreps)
  
  return(prob)
}


# Question 2

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
  }else{
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


# Question 3

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



# Question 5 

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

#question 6
prob <- dloop(50, 10000) 
barplot(prob,ylab = 'probability',xlab = 'loop length',col = 'blue',main = 'probability of each loop length from 1 to
        2n occurring at least once') #displaying histogram of probabilities

1-sum(prob[51:100])

# running time check
end <- Sys.time()
runningtime1 <- end - start
cat(runningtime)






