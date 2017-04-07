computeSofi <- function(data, Fruit, Vegetables, Legumes, Cereals, Fish, Meat, Dairy, Alcohol, OliveOil,
                        output = "percent", rm.na = FALSE, frequency = NULL){
  # Please note that Olive Oil is not the consumption amount, but rather if it is '0 = ocasionally used', '1 = somehow frequent' or '2 = habitual'

  arguments <- as.list(match.call())
  Fruit <- eval(arguments$Fruit, data)
  Vegetables <- eval(arguments$Vegetables, data)
  Legumes <- eval(arguments$Legumes, data)
  Cereals <- eval(arguments$Cereals, data)
  Fish <- eval(arguments$Fish, data)
  Meat <- eval(arguments$Meat, data)
  Dairy <- eval(arguments$Dairy, data)
  Alcohol <- eval(arguments$Alcohol, data)
  OliveOil <- eval(arguments$OliveOil, data)


  # this code chunk tests if data has not been introduced in a daily fashion, and if so, transform data to daily consumption
  if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

  if(frequency == "weekly" || frequency == "monthly"){
    Vars <- list(Fruit = Fruit, Vegetables = Vegetables, Legumes = Legumes, Cereals = Cereals,
                 Fish = Fish, Meat = Meat, Dairy = Dairy, Alcohol = Alcohol)

    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

    Fruit <- Vars$Fruit
    Vegetables <- Vars$Vegetables
    Legumes <- Vars$Legumes
    Cereals <- Vars$Cereals
    Fish <- Vars$Fish
    Meat <- Vars$Meat
    Dairy <- Vars$Dairy
    Alcohol <- Vars$Alcohol

  } else {
    if(frequency != "daily"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
  }


  Frs <- ifelse(Fruit >= 2, 2,
                ifelse(Fruit >= 1.5, 1, 0))

  Vs <- ifelse(Vegetables > 2.5, 2,
               ifelse(Vegetables >= 1, 1, 0))

  Legumes <- 7 * Legumes # questionnaire scores it as weekly consumption
  Ls <- ifelse(Legumes > 2, 2,
               ifelse(Legumes >= 1, 1, 0))

  Cs <- ifelse(Cereals > 1.5, 2,
               ifelse(Cereals >= 1, 1, 0))

  Fish <- 7 * Fish
  Fis <- ifelse(Fish > 2.5, 2,
                ifelse(Fish >= 1, 1, 0))

  Ms <- ifelse(Meat < 1, 2,
               ifelse(Meat <= 1.5, 1, 0))

  Ds <- ifelse(Dairy < 1, 2,
               ifelse(Dairy <= 1.5, 1, 0))

  As <- ifelse(Alcohol >= 1 & Alcohol <= 2, 2,
               ifelse(Alcohol < 1, 1, 0))

  OOs <- ifelse(OliveOil == 2, 2,
                ifelse(OliveOil == 1, 1, 0))

  score <- data.frame(Frs, Vs, Ls, Cs, Fis, Ms, Ds, As, OOs)
  score$absolute <- apply(score, 1, function(x) sum(x, na.rm = rm.na))
  score$percent <- round(100 * score$absolute/18, 1)


  if(missing(output) || output == "percent") {return(score$percent)
  } else {
    if(output == "absolute") {return(score$absolute)
    } else {
      if(output == "data.frame") {return(score)
      } else {
        stop("please, select a valid output argument, admited values are 'percent' -default-, 'absolute' and 'data.frame' " )
      }
    }
  }

}
