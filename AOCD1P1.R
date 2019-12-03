# Advent of Code, Day 1 Part 2
# Iterated fuel calculation
# modules object defined in part 1

iterfuelcalc <- function(mass){
  fuelcalc <- function(mass){
    out <- ifelse(
      ((floor(mass/3)-2)<0),
      0,
      floor(mass/3)-2)
    return(out)
  }
  totalfuel <- c(rep(0,length(mass)))
  repeat{
    fuel <- fuelcalc(mass)
    totalfuel <- totalfuel + fuel
    if(sum(fuel) <= 0){
      return(totalfuel)
      break
    }
    mass <- fuel
  }
return(totalfuel)  
}

sum(iterfuelcalc(modules))
