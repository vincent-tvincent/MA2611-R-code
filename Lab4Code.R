q1a1 = F
q1a2 = F
q1b1 = F
q1b2 = F
q2 = F
q3 = F
q4 = T
#question 1
#given code
DrawDistribution <- function(n, sampleN){
  xBar = array(NA, sampleN)
  for(i in 1:sampleN){
    aSample = rnorm(n, 0, 1)
    xBar[i] = mean(aSample)
  }
  hist(xBar,freq = F)
  x = seq(-1, 1, 0.005)
  lines(x, dnorm(x,0,1/sqrt(n)),  col = "red")
}

#function for draw the q-q plot
DrawQq <- function(n,sampleN){
  #generate random sample's mean by given code
  xBar = array(NA, sampleN)
  for(i in 1:sampleN){
    aSample = rnorm(n, 0, 1)
    xBar[i] = mean(aSample)
  }
  qqnorm(xBar)
  qqline(xBar,col = "red")
}


#a 
if(q1a1){
  DrawDistribution(100,1000)
}
if(q1a2){
  DrawDistribution(1000,1000)
}
#b
if(q1b1){
  DrawQq(100,1000)
}

if(q1b2){
  DrawQq(1000,1000)
}

#question2
#generate the samples
weights = read.csv("bagWeight.csv",sep = ",",header = F)
if(q2){
  print(t.test(weights, conf.level = 0.9, mu = 454))
}

#question3
if(q3){
  print(prop.test(x = 45, n = 371, p = 0.05, correct = FALSE, conf.level = 0.99))
}

#question4  
if(q4){
  prices = read.csv("homeprices.csv", sep = ",")
  #seperating two sets of data 
  homeWith4Bedroom = c()
  homeWith3Bedroom = c()
  for(i in prices$Home){
    if(prices$Bedrooms[i] == 4){
      homeWith4Bedroom[length(homeWith4Bedroom) + 1] = prices$Price[i] 
    }else{
      homeWith3Bedroom[length(homeWith3Bedroom) + 1] = prices$Price[i]
    }
  }
  print(t.test(homeWith4Bedroom,homeWith3Bedroom,var.equal = T, anternative = "greater"))
}