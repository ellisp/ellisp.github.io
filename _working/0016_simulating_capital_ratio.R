beta

capital_income_ratio = .12 / .02




capital stock = 6 * income

savings        = 0.12 * income

incomegrowth


capital/income = savings / incomegrowth

income = savings / 0.12
capital stock = 6 * savings / 0.12



capital <- 0
income <- 100000

for(i in 1:1000){
   capital <- capital + .12 *income
   income <- income * 1.02
   cat(i, round(income), round(capital),  round(capital / income, 1), "\n")
   
}


