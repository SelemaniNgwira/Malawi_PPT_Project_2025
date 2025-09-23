# The formula below is used to calcualte the sample size of the population 
(1.5*10000*1.96^2*0.5*(1-0.5))/(0.05^2*(10000-1)+1.96^2*0.5*(1-0.5))




# for a school survey with a population of 10,000 students, a confidence level of 95%,

install.packages("pwr")

pwr::pwr.2p.test(h = 0.5, sig.level = 0.05,  power = 0.85, alternative = "two.sided")
#>



