#Libraries
library(truncnorm)
library(ggplot2)
library(dplyr)
library(tidyr)
# Declare global variables. 
# Forecast period in months
fp <- 16
# First month in the forecast period that a annual pay increase will be received.
first_pr <- 2
# Number of months you must have been with the company prior to the annual pay rise to be eligible. 
eligble_pr <- 6
# Pay rise percentage. 
incrs <- 1.02
# Expected value for the number of months and employees stays in the role.
m <- 36
# standard deviations for employees time in role (sd)
sd <- 3
# Total number of positions available
tPos <- 12
# Mean recruits per year
m_rcts <- 3
# Starting salary
strt_slry <- 159000

# List of current employees in the specified role including current salary and 
# the number of months they have been with the company. 
emplys <- list(list(159000, 1),list(159000, 23),list(159000, 11),list(159000, 13),
               list(159000, 12),list(170909, 34),
               list(145455, 2),list(167541, 24),
               list(165793, 53),list(189082, 108),list(159000, 17),list(182000, 59))

# Function to forecast salary costs for current employees
curr_emplys <- function(emplys,m,sd,fp,first_pr){
  # Makes a prediction for the number of months each employee will stay in their current role.
  # This prediction is created by sampling from a normal distribution using the expected value and sd.
  pred <- vector(mode = "numeric")
  for(i in 1:length(emplys)){
    # rtruncnorm restricts the sample to numbers greater than the current days 
    # that the employee has been with the company
    # round to nearest month
    pred[[i]] <- round(rtruncnorm(n=1,a=emplys[[i]][[2]],mean = m, sd = sd),0)-emplys[[i]][[2]]
  }
  
  # Use the predictions to calculate final salary costs for each employee. 
  pred_cost <- vector(mode = "numeric", length(emplys))
  for(i in 1:length(emplys)){
    # If prediction is greater than forecast period make pred[[i]] equal the forecast period
    if(pred[[i]]>fp){pred[[i]]<-fp }
    
    # Check if the employee will have been with the company long enough to be eligible for
    # a pay rise at the next review. If not eligible make the next pay review 12 months later.
    ifelse(emplys[[i]][[2]]<eligble_pr,nxt_pr<-first_pr++12,nxt_pr<-first_pr)
    
    # If the predicted time remaining in the role is less than or equal to the time remaining until a pay rise
    # No pay rise needs to be applied, just multiply the number of months remaining by the current salary per month. 
    ifelse(pred[[i]]<= nxt_pr,pred_cost[[i]]<-pred[[i]]*(emplys[[i]][[1]]/12),{
      
      # else break down the predicted time in the current role into the periods where a pay rise applies 
      # Find the number of whole years
      no_whl_yrs <- floor((pred[[i]]/12)-(nxt_pr/12))
      # Find the fraction of a year that is remaining excluding the period prior to a pay rise.
      remainder_yrs <- (pred[[i]]/12)-floor((pred[[i]]/12))-(nxt_pr/12)
      # If number of whole years is 0, the employee has stayed long enough to 
      # get the first pay rise and left before the 2nd
      # Therefore salary cost is the addition of the period prior to the pay rise 
      # and a fraction of the first years pay rise.
      ifelse(no_whl_yrs==0,pred_cost[[i]]<-nxt_pr*(emplys[[i]][[1]]/12)+(pred[[i]]-nxt_pr)*((emplys[[i]][[1]]*incrs)/12),{
        # Else the number of whole years is 1 or more and the salary needs to be increase more than once. 
        # Generate a sequence of exponentials for each whole year.
        x <- seq(1,no_whl_yrs,1)
        pred_cost[[i]] <- ifelse(remainder_yrs>0,
                                 # Calculate the salary cost for each whole year. Add the salary cost for the final fraction of a  year.
                                 sum(emplys[[i]][[1]] * incrs^ (x)) + emplys[[i]][[1]]* remainder_yrs * incrs^(tail(x, n=1)+1),
                                 # If no fraction of year exists calculate salary costs for whole years only.
                                 # Add the salary cost for the period prior to a pay rise after the ifelse .
                                 sum(emplys[[i]][[1]] * incrs^ (x)))+nxt_pr*(emplys[[i]][[1]]/12)
      })
    })
  }
  return(c(sum(pred_cost),pred))
}

# Forecast recruiting and salary cost for future employees
recruit <- function(first_pr,emplys_pred,tPos,m,sd,fp,m_rcts){
  
  # Generate a matrix.
  mtrx <- matrix(nrow = 0, ncol = fp)
  # Remove all 0s so that matrix is numeric and can be summed.
  emplys_pred <- emplys_pred[emplys_pred!=0]
  for (i in emplys_pred){
    # If prediction is greater than forecast period create vectors of length fp containing 1s
    # Else create a vector with 1s for the employed period and then 0s for the remaining months
    #Add row to the matrix. 
    ifelse(i>=fp,mtrx <- rbind(mtrx,c(replicate(fp,1))),mtrx <- rbind(mtrx,c(replicate(i,1),replicate((fp-i),0))))
  }
  # Mean number of recruits in one year * forecast period 
  # gives mean number of recruits in the forecast period (fp_rcts) in years.
  # Round to get an integer. 
  fp_rcts <- round(m_rcts*(fp/12))
  # Generate a number of people recruited during the forecast period.
  # It is assumed that recruiting follows a poisson distribution,
  # use the mean to generate distribution and sample from the distribution.
  no_rcts <- rpois(1,fp_rcts)
  # Check if we have recruited anyone before proceeding.
  # Create a vector salary costs
  slry_vctr <- vector(mode = "numeric")
  if(no_rcts>0){
    # Generate a list of random joining month for each recruit.
    # Reorder to ascending
    jng_mnths <- sort(sample(1:fp,no_rcts,replace = TRUE),decreasing = FALSE)
    for (x in jng_mnths){
      # Check if there are any vacancies that month.
      Vno <- tPos-sum(mtrx[,x])
      if(Vno>0){
        # Predicted how long they will be employed
        pred <- round(rnorm(n=1,mean = m, sd = sd))
        # If an employee is predicted to stay for less than a month round up to 1 month.
        if(pred<1){pred <-1}
        # Add the new employee to the matrix. Starting from the month they were recruited.
        # If in month (col) 1, add a row starting with 1s, Else add in 0s before month of employment.
        if(x==1){ifelse(pred>=fp,new_row <- c(replicate(fp,1)), new_row <-c(replicate(pred,1),replicate((fp-pred),0)))}
        # Logic for the row above - If the predicted period of employment is greater than or equal to the forecast period.
        # Create a list of 1s the length of the forecast period.
        # Else create a list of 1s for the number of months predicted,
        # followed by a list of 0s for the remainder of the forecast period.
        else {ifelse(pred>=fp-x,new_row <- c(replicate(x-1,0),replicate(fp-(x-1),1)),new_row <- c(replicate(x-1,0),replicate(pred,1),replicate((fp-pred-(x-1)),0)))}
        # Logic for the rows above - The first else tells us that the joining month for the recruit,
        # is after the first month. If the predicted period of employment is greater than or
        # equal to the time remaining in the forecast period after the joining month
        # that is fp-x then create a list of 0s followed by 1s
        # Else there is a period with an empty position before and after the predicted period.
        # Therefore create a list of 0s up to 1 month before the first month of employment.
        # then a list of 1s for the length of employment or pred. Then a list of 0s
        # for the time after employment up until the end of the forecast period.
        
        # Sum the row for salary calculations
        mnts_empyd <- sum(new_row)
        # Bind the row to the matrix to be used in next iteration of the loop.
        mtrx <- rbind(mtrx,new_row)
        
        # The final part of the function calculates salary cost for the recruit and adds to slry_cst a vector.
        nxt_pr<-first_pr
        while(nxt_pr<x) {nxt_pr<-nxt_pr++12}
        # Check if the employee will have been with the company long enough to be eligible for
        # a pay rise at the next review. If not eligible make the next pay review 12 months later.
        if(nxt_pr-x<eligble_pr){nxt_pr<-nxt_pr++12}
        mnths_to_pr <- nxt_pr-x 
        
        # If the months employed (mnths_emplyed) months to pay rise (mnth_to_pr) is
        # less than or equal to months to pay rise (mnths_to_pr)
        # No pay rise needs to be applied, just multiply the number of months employed by the salary per month.
        ifelse(mnts_empyd <= mnths_to_pr,slry_csts <-mnts_empyd*(strt_slry/12),{
          
          # else break down the months employed in the current role into the periods where a pay rise applies
          # Find the number of whole years after the first pay rise.
          no_whl_yrs <- floor((mnts_empyd-mnths_to_pr)/12)
          # Find the fraction of a year that is remaining excluding the period prior to a pay rise.
          remainder_yrs <- (mnts_empyd/12)-no_whl_yrs-(mnths_to_pr/12)
          # If number of whole years is 0, the employee has stayed long enough to
          # get the first pay rise and left before the 2nd
          # Therefore salary cost is the addition of the period prior to the pay rise
          # and a fraction of the first years pay rise.
          ifelse(no_whl_yrs==0,slry_csts <-mnths_to_pr*(strt_slry/12)+(mnts_empyd-mnths_to_pr)*((strt_slry*incrs)/12),{
            # Else the number of whole years is 1 or more and the salary needs to be increase more than once.
            # Generate a sequence of exponentials for each whole year.
            x <- seq(1,no_whl_yrs,1)
            # Calculate the salary cost for each whole year. Add the salary cost for the final fraction of a  year.
            # If no fraction of year exists calculate salary costs for whole years only.
            # Add the salary cost for the period prior to a pay rise after the ifelse .
            slry_csts <- ifelse(remainder_yrs>0, sum(strt_slry * incrs^ (x)) + strt_slry* remainder_yrs * incrs^(tail(x, n=1)+1),sum(strt_slry * incrs^ (x)))+mnths_to_pr*(strt_slry/12)
          })
        })
        slry_vctr<-c(slry_vctr, slry_csts)
        
      }
    }
  }
  return(list(sum(slry_vctr),sum(colSums(mtrx))))
  #ifelse(sum(slry_vctr)>1200000,return(list(mtrx,no_rcts,slry_vctr)),return(100))
}


# Run Monte Carlo Simulations
ul <- replicate(n=100000, expr ={
  a <- curr_emplys (emplys,m,sd,fp,first_pr)
  b <-recruit(first_pr,a[-1],tPos,m,sd,fp,m_rcts)
  return(list(a[1] + unlist(b[1]),unlist(b[2])))
})


revenue_cc <- unlist(lapply(cc[2,],"*", 18333))
ttl_cc <- unlist(cc[1,])

cc_qtls <- quantile(t_slry_cst ,c(.025,.975))
cc_qtls <- as.data.frame(qtls)
cc_qtls

ggplot() + aes(t_slry_cst) + geom_density (fill = "#76B7B2",color = "#76B7B2", alpha = 0.5) + 
  geom_vline(xintercept = c(cc_qtls[2:1,1])) + 
  annotate(geom = "text", label = c("2.5%","97.5%"), x = c(cc_qtls[1,1]-40000,cc_qtls[2,1]+40000), y = c(0e-03,0e-03)) + 
  theme_minimal() + scale_y_continuous("density")