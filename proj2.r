
#Aysu Ismayilova (s2295782)
#Xiaoxian Zhang (s2287323)
#Xiaolin Yan (s2326461)

# the link: https://github.com/EvelynYan123/Project_2.git
# Contributions: each of us contributed 1/3 of the job.


Pone <- function(n,k,strategy, nreps){ #A function to calculate the probability that one prisoner finds his number 
  #given n- number of tries(max number of boxes he can open), k-prisoner's number, the strategy and number of iterations
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
        while(tries <= n) { #prisoners can open at most n boxes
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
      inboxes <- sample(1:N,N)[1:n]  # we randomly choose n boxes with n cards in them
      results[i] <- match(k,inboxes) #check if the prisoner finds his number and repeat it nrep number of times and store each result in results[i]
                                     #if a prisoner finds his number it will be an integer, otherwise NA 
      results <- length(results[which(is.na(results)== FALSE)])#we count how many results are integers. In other words in nreps times of simulation how many times  
                                                               #a prisoner with number k found his number
    }       
  }
  
  prob = sum(results)/(nreps)
  
  return(prob)
}




Pall <- function(n, strategy, nreps){#A function to calculate the probability of all prisoners finding their numbers given #given n -number of tries(max of boxes they can open), the strategy and number of iterations
  N <- 2*n #number of prsioners
  results = rep(0,nreps) #initialising results to 0
  if(strategy != 3){ #consider a strategy different from 3, that is, strategies 1 and 2
    for(i in 1:nreps) { #loop over iterations
      boxes = sample(1:N,N) #sample boxes
      prisoners = 1:N #we have N prisoners
      foundIt = 0 #initialising number of prisoners who found their numbers to zero
      for(prisoner in prisoners) {#loop over all prisoners
        tries = 1 
        
        if(strategy == 1){ #consider strategy1
        path = c(prisoner) #keep track of a path of a prisoner
        inBox = boxes[prisoner] #prisoner opens the box with his number on it
        }else if(strategy == 2){ #consider strategy2
          path = c(sample(1:N,1)) #sample boxes
          inBox = boxes[path]#keep track of a path of a prisoner
        }
        
        while(tries <= n) { #while prisoner does not open at most n boxes
          path = c(path, inBox) #keep track of the path of a prisoner			 			
          if(inBox == prisoner) { 	#check if the card inside of a box coincides with the prisoner's number			
            foundIt = foundIt + 1 	#if yes, update	foundIt		
            break; 			} 
          else { 					
            inBox = boxes[inBox] 			} #if no, follow this number in the next box				
          tries = tries+1 		} 		#update tries 		
      }
      
      
      results[i] = foundIt #gives overall result of all prisoners who found their numbers 
    }
  }else{
    for(i in 1:nreps) {#loop over iterations
      prisoners <- 1:N #we have N prisoners
      temp <- numeric(N)#temporary
      
      for(j in prisoners){
        inboxes <- sample(1:N,N)[1:n]#we randomly choose n boxes with n cards in them
        temp[j] <- match(j,inboxes)#check if the jth prisoner finds his number and repeat it nreps times and store the result in temp
                                   #if the prisoner finds his number temp[j] will be an integer otherwise NA 
      }
      temp <- na.omit(temp) #drop all NA's 
      results[i] <- N-length(attributes(temp)$na.action) #N indicates the number of prisoners who did not find their number
                                                         #results shows how many prisoners found their numbers in ith simulation 
    }
    
    
  }
  
  prob = length(which(results==2*n))/nreps
  
  return(prob)
}



success_prob <- function(n,nreps){ 
  for (strategy in 1:3){ #loop over the strategies
    cat('In strategy', strategy,':',fill = TRUE)
    cat(" P(a single prisoner's success)=", {Pone(n,1,strategy,nreps)},fill = TRUE)
    cat(" P(all prisoners' success)=", {Pall(n,strategy,nreps)},fill = TRUE)
  }
}
success_prob(5,10000)# In case of n=5, we have:
#In strategy 1 :
# P(a single prisoner's success)= 0.4923
# P(all prisoners' success)= 0.3534
#In strategy 2 :
# P(a single prisoner's success)= 0.4056
# P(all prisoners' success)= 3e-04
#In strategy 3 :
# P(a single prisoner's success)= 1e-04
# P(all prisoners' success)= 0.0014

success_prob(50,10000)# In case of n=50, we have:
#In strategy 1 :
# P(a single prisoner's success)= 0.4995
# P(all prisoners' success)= 0.3089
#In strategy 2 :
# P(a single prisoner's success)= 0.3719
# P(all prisoners' success)= 0
#In strategy 3 :
# P(a single prisoner's success)= 1e-04
# P(all prisoners' success)= 0

# In strategy 1, the probability of all prisoners getting free is roughly 31%, but in other two strategies, the probabilities that they will be free are 0.




dloop <- function(n, nreps){ #creating the function dloop that takes number of boxes and number of iterations
  count_0 = rep(0, n*2) # initialise vector of length 2*n
  
  for (reps in 1:nreps){ #loop over nreps
    count_1 = rep(0, n*2) #initialise vector counting length of a loop 
    boxes = sample(n*2,n*2) #sample boxes 
   
    
    iteration_num = c() #initialising vector of numbers we will perform iterations over
    for (k in 1:(2*n)){ #loop over 2*n boxes 
      cycles = c() 
      if ((k %in% iteration_num) == FALSE){  #check if k coincides with the numbers over which we perform iterations
        cycles = append(cycles, k) #if not then append this value to that loop
        next_box = boxes[k] #follow the prisoner's number in next box
        while (next_box != k){ #we append the next boxes to the cycle until in one of the boxes we find the prisoner's number
                               #if we find the number the cycle is closed and we are done
          cycles = append(cycles, next_box)  #add next box to the cycle
          next_box =boxes[next_box] 
        }
        count_1[length(cycles)]=count_1[length(cycles)]+1 # count  number of boxes in cycles
        iteration_num = append(iteration_num, cycles) 
      }
    }
    index_0 = grep(0, count_1)
    count_0[index_0]=count_0[index_0]+1
  }
  prob_0 <- count_0/nreps #probability of a non-repeated loops (0 time)
  prob_1 <- 1- prob_0 #probability of an each loop 1:2n at least once
  return(prob_1)
}

prob <- dloop(50, 10000) 
barplot(prob,ylab = 'probability',xlab = 'loop length',col = 'blue',main = 'probability of each loop length from 1 to
        2n occurring at least once') #displaying barplot of probabilities

1-sum(prob[51:100]) #probability that there is no loop length greater than 50 


#Since the prisoner starts on the box of their own number they are, on the cycle that contains their card 

#By following the cycle around they will eventually end up at their card.

#The only issue is if prisoners can find their numbers before n boxes are opened.
#In fact, the maximum cycle length should be less than or equal to 50, and there can only be one cycle in any set that has a cycle greater than fifty and 
#after calculating the probability we get 0.319
# about 30 percent of the time, the length of the maximum chains formed will be less than 50 boxes and so all prisoners will be able to find their numbers before they open more than 50 boxes





