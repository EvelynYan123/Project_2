
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


Pall <- function(n, strategy, nreps){ #A function to calculate the probability that all prisoners find their numbers 
                                      #given number of tries, the strategy and number of iterations
  N <- 2*n 
  results = rep(0,nreps)  #initialise vector of results
  if(strategy != 3){  #for strategies except strategy 3 (that is, 1 and 2)
    for(i in 1:nreps) {  #loop over nreps
      boxes = sample(1:N,N) #sample boxes
      prisoners = 1:N  #we have 2*n prisoners
      foundIt = 0 #initialise number of prisoners who found their numbers
      for(prisoner in prisoners) { #loop over prisoners
        tries = 1
        if(strategy == 1){  #consider a strategy 1
          path = c(prisoner) #keep track of the path of a prisoner
          inBox = boxes[prisoner] # prisoner opens a box with his number
        }else if(strategy == 2){ #consider a strategy 2
          path = c(sample(1:N,1)) #sample random box  
          inBox = boxes[path]  #firstly prisoner opens a random box
        }
        while(tries <=n) { #prisoners can open at most 50 boxes
          path = c(path, inBox) 		#keep track of a path	 			
          if(inBox == prisoner) { 	#check if the number inside of the box coincides with the prisoner's number		
            foundIt = foundIt + 1   #if yes then update foundIt
            break; 			} 
          else { 					
            inBox = boxes[inBox] 			} #if no, then follow this number in the next box
          tries = tries+1 		} 		 		#update number of tries
      }
      results[i] = foundIt  #how many prisoners found their numbers
    }
  }else{ #strategy3
    for(i in 1:nreps) { #loop over number of iterations
      boxes = sample(1:N,N) # sample boxes
      prisoners = 1:N #we have 2*n prisoners
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

prob <- dloop(50, 10000) 
barplot(prob,ylab = 'probability',xlab = 'loop length',col = 'blue',main = 'probability of each loop length from 1 to
        2n occurring at least once') #displaying histogram of probabilities

1-sum(prob[51:100])








# check
# strategy 3----------------
iters = 10
results = rep(0,iters)

k <- 1
n <- 5
N <- 2*n


for(i in 1:iters) {
  prisoners <- 1:N
  temp <- numeric(N)
  
  for(j in prisoners){
    inboxes <- sample(1:N,N)[1:n]
    temp[j] <- match(j,inboxes)
  }
  temp <- na.omit(temp)
  results[i] <- N-length(attributes(temp)$na.action)
}

