setwd("C:/Users/jakeo/Documents/School/Grad School/Research/data")

## Code used to count the number of bills in each two year period for the 
## latent space model

for(congress in 101:111){
  i <- 1 #initiate bill number
  j <- 1 #initiate year of congressional term (there will only be j=1 or j=2)
  # congress <- 102 #initiate congress; we are currently using the 101st congress as our earliest 
  
  # Convert bill number to string 
  # and convert to 5 digit number with sufficient number of preceding zeros
  number <- as.character(i)
  # Add in preceding zeros
  for(num in 1:(5-nchar(number))){
    number <- paste0(0, number)
  }
  
  # Read in first file
  d <- readRDS(paste0("./vote_",congress,"_",j,"_",number,".rds"))
  
  ##########################################################################################
  # Get all the names of the senators for the two year term to account for special elections
  ##########################################################################################
  senators <- c()
  
  error <- FALSE
  end <- FALSE
  while(isFALSE(end)){
    while(isFALSE(error)){
      i <- i + 1
      
      # Convert iteration index/bill number to string 
      # and convert to 5 digit number with sufficient number of preceding zeros
      number <- as.character(i)
      # Add in preceding zeros
      for(num in 1:(5-nchar(number))){
        number <- paste0(0, number)
      }
      
      # Try to read in file
      file <- try(readRDS(paste0("./vote_",congress,"_",j,"_",number,".rds")))
      if("try-error" %in% class(file)){
        # If file doesn't exist and an error is generated, 
        # need to end loop and update j or end loop entirely if j = 2
        error <- TRUE
        
        ## Code used to count number of bills per two year period for the latent space model
        # if(j==1){
        #   tmp<-i-1
        # } else {
        #   print(tmp+i-1)
        #   trials <- append(trials, tmp+i-1)
        # }
      } else {
        file_senators <- as.character(file$member_full)
        
        if(sum(!(file_senators %in% senators)) > 0){
          
          #If there are new senators, they will be concatenated to our senator list here
          senators <- append(senators, 
                                  file_senators[which(!(file_senators %in% senators))])
        } 
      }
      print(i)
    }
    if(j == 2){
      # If all of the files for the 2nd year of the congressional term are finished
      # being processed, end the loop
      end <- TRUE
    }
    if(j == 1){
      # If all of the files for the 1st year of the congressional term are finished
      # being processed, update j=2 for the second year of the congressional term
      i <- 0
      j <- 2
      error <- FALSE
    } 
  }
  
  #alphabetize list of senators
  senators <- senators[order(senators)]
  # initialize y to count how many votes in which each senator participated
  y <- data.frame(row.names = senators)
  y[senators] <- 0

  ##########################################################################################

  # Convert lists to character arrays
  vote_cast <- data.frame(name = as.character(d$member_full), 
                          vote = as.character(d$vote_cast))

  # Initiate "Yea" Adjacency Matrix and "Nay" Adjacency Matrix
  yea_votes <- data.frame(row.names = senators)
  yea_votes[senators] <- 0
  nay_votes <- data.frame(row.names = senators)
  nay_votes[senators] <- 0

  # Set up Adjacency Matrix
  # Count common "yea" votes
  yea_votes[vote_cast$name[which(vote_cast$vote == "Yea")],] <- 1
  yea_votes[,vote_cast$name[which(vote_cast$vote == "Yea")]] <- 1
  yea_votes[vote_cast$name[which(vote_cast$vote != "Yea")],] <- 0
  yea_votes[,vote_cast$name[which(vote_cast$vote != "Yea")]] <- 0
  # Make rows and columns of non-participating senators 0
  yea_votes[senators[which(!(senators %in% vote_cast$name))],] <- 0
  yea_votes[,senators[which(!(senators %in% vote_cast$name))]] <- 0
  
  # Count common "nay" votes
  nay_votes[vote_cast$name[which(vote_cast$vote == "Nay")],] <- 1
  nay_votes[,vote_cast$name[which(vote_cast$vote == "Nay")]] <- 1
  nay_votes[vote_cast$name[which(vote_cast$vote != "Nay")],] <- 0
  nay_votes[,vote_cast$name[which(vote_cast$vote != "Nay")]] <- 0
  # Make rows and columns of non-participating senators 0
  nay_votes[senators[which(!(senators %in% vote_cast$name))],] <- 0
  nay_votes[,senators[which(!(senators %in% vote_cast$name))]] <- 0

  # Combine common "yea" and "nay" vote data to matrix of common votes
  votes <- yea_votes + nay_votes

  # Need to re-initialize these indeces
  i <- 1 #initiate bill number
  j <- 1 #initiate year of congressional term (there will only be j=1 or j=2)
  # congress <- 101 #initiate congress; we are currently using the 101st congress as our earliest
  # initial loop control variables
  error <- FALSE
  end <- FALSE
  while(isFALSE(end)){
    while(isFALSE(error)){
      i <- i + 1

      # Convert iteration index/bill number to string
      # and convert to 5 digit number with sufficient number of preceding zeros
      number <- as.character(i)
      # Add in preceding zeros
      for(num in 1:(5-nchar(number))){
        number <- paste0(0, number)
      }

      # Try to read in file
      file <- try(readRDS(paste0("./vote_",congress,"_",j,"_",number,".rds")))
      if("try-error" %in% class(file)){
        # If file doesn't exist and an error is generated,
        # need to end loop and update j or end loop entirely if j = 2
        error <- TRUE
      } else {
        file_votes_cast <- data.frame(name = as.character(file$member_full), 
                                      vote = as.character(file$vote_cast))
        file_senators <- as.character(file$member_full)
        # update vote participation matrix y
        file_y <- senators %in% file_senators
        y <- y + file_y %*% t(file_y)

        # Initialize common "Yea" vote data frame
        yea_v <- data.frame(row.names = senators)
        yea_v[senators] <- 0

        # Initialize common "Nay" vote data frame
        nay_v <- data.frame(row.names = senators)
        nay_v[senators] <- 0

        # Count common "yea" votes
        yea_v[file_votes_cast$name[which(file_votes_cast$vote == "Yea")],] <- 1
        yea_v[,file_votes_cast$name[which(file_votes_cast$vote == "Yea")]] <- 1
        yea_v[file_votes_cast$name[which(file_votes_cast$vote != "Yea")],] <- 0
        yea_v[,file_votes_cast$name[which(file_votes_cast$vote != "Yea")]] <- 0
        # Make rows and columns of non-participating senators 0
        yea_v[senators[which(!(senators %in% file_votes_cast$name))],] <- 0
        yea_v[,senators[which(!(senators %in% file_votes_cast$name))]] <- 0
        
        # Count common "nay" votes
        nay_v[file_votes_cast$name[which(file_votes_cast$vote == "Nay")],] <- 1
        nay_v[,file_votes_cast$name[which(file_votes_cast$vote == "Nay")]] <- 1
        nay_v[file_votes_cast$name[which(file_votes_cast$vote != "Nay")],] <- 0
        nay_v[,file_votes_cast$name[which(file_votes_cast$vote != "Nay")]] <- 0
        # Make rows and columns of non-participating senators 0
        nay_v[senators[which(!(senators %in% file_votes_cast$name))],] <- 0
        nay_v[,senators[which(!(senators %in% file_votes_cast$name))]] <- 0

        # update votes
        votes <- votes + yea_v + nay_v
      }
      print(i)
    }
    if(j == 2){
      # If all of the files for the 2nd year of the congressional term are finished
      # being processed, end the loop
      end <- TRUE
    }
    if(j == 1){
      # If all of the files for the 1st year of the congressional term are finished
      # being processed, update j=2 for the second year of the congressional term
      i <- 0
      j <- 2
      error <- FALSE
    }
  }

  saveRDS(votes, paste0("./adjacency_matrices/adj_mat_",congress,".rds"))
  saveRDS(y, paste0("./adjacency_matrices/common_vote_",congress,".rds"))
  closeAllConnections()
}


