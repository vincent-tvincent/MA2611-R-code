#which question's code to activate
q1a = F
q1b = F
q2 = F
q3a = F
q3b = F
q3c = F
q4a = F
q4b = F
q4c = F
q4d = F
q4e = F
q5 = T
#question 1 
#a
toss.coin <- function(Prob, times){ # perform "tossing" for unfair weight coin, Prob is the probability for get head, times is the total tossing times
  result = c()
  head.sum = 0
  probabilities = c(Prob, 1 - Prob) #(probability for head, probability for tail)
  for (toss.time in 1:times){
    head.sum = head.sum + sample(c(1,0), 1,T,probabilities)
    result[length(result) + 1]  =  head.sum / toss.time
  }
  return(result)
}

if(q1a){ # for activate code only for this question, no meaning. 
  #codes for do tossing and plot for question 1a
  toss.outcome = toss.coin(1/5, 3)
  plot(toss.outcome,type = "l",col = "red",xlab = "nth toss", ylab = "frequency", main = "toss outcome")
}

#b
if(q1b){
  toss.outcome = toss.coin(Prob = 1/5,times = 1000)
  plot(toss.outcome,type = "l",col = "red",xlab = "nth toss", ylab = "frequency", main = "toss outcome")
}

#question 2
if(q2){
  s = sample(c(0,2,4,6),1000,T,c(0.3,0.2,0.2,0.3))
  m = mean(s)
  v = var(s)
  sd = sqrt(v)
  print(paste("mean: ", m))
  print(paste("variance: ", v, " standard deviation: ", sd))
  
}

#question 3 
#a 
if(q3a){
  print(paste("p(Y=5) = ", dbinom(5, 8, 0.5)))
}
#b 
if(q3b){
  p1 = pbinom(3, 8, 0.5) - dbinom(3, 8, 0.5)#probability for question 1, p(Y <= 3) - p(y = 3)
  p2 = 1 - pbinom(6, 8, 0.5)#probability for question 2, 1 - p(y <= 6)
  print(paste("p(Y < 3) = ", p1))
  print(paste("p(Y > 6) = ", p2))
}
#c
if(q3c){
  probabilities = c()
  for(n in 0:8){ #calculate the probabilities for get certain amount of successes in 8 trial
    probabilities[n+1] = dbinom(n,8,0.5)
  }
  sample.set = table(sample(c(0:8),5000,T,probabilities))
  barplot(sample.set, main="binomial(8,0.5)")
  
}

#question 4
#a
if(q4a){
  print(paste("p(x < 50) = ", pnorm(50,74,7)))
}
#b
if(q4b){
  print(paste("p(x > 90) = ", pnorm(90,74,7,F)))
}
#c
if(q4c){
  print(paste("p(60 < x < 80) = ", pnorm(80,74,7) - pnorm(60,74,7)))
}
#d
if(q4d){
  print(paste("while p(x) = 0.75, x = ", qnorm(0.75,74,7)))
}
#e
if(q4e){
  print(paste("while p(x) = 0.90, x = ", qnorm(0.90,74,7)))
}

#question 5
if(q5){
  q5.number.set = rnorm(200,74,49)
  hist(q5.number.set,main = "N(74,49)",freq = F)
  x = seq(min(q5.number.set),max(q5.number.set),0.5) #all the numbers covered by the histogram
  lines(x,dnorm(x,74,49),col = "blue")
}

