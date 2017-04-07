computePredimed <- function(data, OliveOil, OOmeasure = "gr", OOprincipal, Vegetables, Fruit,
                              RedMeat, Butter, SoftDrinks, Wine, Legumes, Fish, Pastries, Nuts, WhiteMeat, Sofritos,
                              output = "percent", rm.na = FALSE, frequency = NULL){

  arguments <- as.list( match.call() )
  OliveOil <- eval(arguments$OliveOil, data)
  OOprincipal <- eval(arguments$OOprincipal, data)
  Vegetables <- eval(arguments$Vegetables, data)
  Fruit <- eval(arguments$Fruit, data)
  RedMeat <- eval(arguments$RedMeat, data)
  Butter <- eval(arguments$Butter, data)
  SoftDrinks <- eval(arguments$SoftDrinks, data)
  Wine <- eval(arguments$Wine, data)
  Legumes <- eval(arguments$Legumes, data)
  Fish <- eval(arguments$Fish, data)
  Pastries <- eval(arguments$Pastries, data)
  Nuts <- eval(arguments$Nuts, data)
  WhiteMeat <- eval(arguments$WhiteMeat, data)
  Sofritos <- eval(arguments$Sofritos, data)


if(OOmeasure == "gr") {OliveOil <- OliveOil/0.918/10}
  else {
  if(OOmeasure == "ml") {OliveOil <- OliveOil/10}
    else {
    if(OOmeasure == "serving") {"no conversion needed"}
      else {stop("check units of Olive Oil")
    }
  }
}


  # this code chunk tests if data has not been introduced in a daily fashion, and if so, transform data to daily consumption
  if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

  if(frequency == "weekly" || frequency == "monthly"){
    Vars <- list(OliveOil = OliveOil, OOprincipal = OOprincipal, Vegetables = Vegetables, Fruit = Fruit, RedMeat = RedMeat,
                 Butter = Butter, SoftDrinks = SoftDrinks, Wine = Wine, Legumes = Legumes, Fish = Fish, Pastries = Pastries,
                 Nuts = Nuts, WhiteMeat = WhiteMeat, Sofritos = Sofritos)
    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

    OliveOil <- Vars$OliveOil
    OOprincipal <- Vars$OOprincipal
    Vegetables <- Vars$Vegetables
    Fruit <- Vars$Fruit
    RedMeat <- Vars$RedMeat
    Butter <- Vars$Butter
    SoftDrinks <- Vars$SoftDrinks
    Wine <- Vars$Wine
    Legumes <- Vars$Legumes
    Fish <- Vars$Fish
    Pastries <- Vars$Pastries
    Nuts <- Vars$Nuts
    Sofritos <- Vars$Sofritos
  } else {
    if(frequency != "daily"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
  }


OOs <- c()
  OOs <- ifelse(OliveOil >= 4, 1, 0)

OOps <- c()
  OOps <- ifelse(OOprincipal == 1, 1, 0)

Vs <- c()
  Vs <- ifelse(Vegetables >= 2, 1, 0)

Frs <- c()
  Frs <- ifelse(Fruit >= 3, 1, 0)

RMs <- c()
  RMs <- ifelse(RedMeat < 1, 1, 0)

Bs <- c()
  Bs <- ifelse(Butter < 1, 1, 0)

SDs <- c()
  SDs <- ifelse(SoftDrinks < 1, 1, 0)

# consumption has been set as daily, but several foods are scored in a weekly fashion
Wine <- Wine * 7
Ws <- c()
  Ws <- ifelse(Wine >= 7, 1, 0)

Legumes <- Legumes * 7
Ls <- c()
  Ls <- ifelse(Legumes >= 3, 1, 0)

Fish <- Fish * 7
Fis <- c()
  Fis <- ifelse(Fish >= 3, 1, 0)

Pastries <- Pastries * 7
Pts <- c()
  Pts <- ifelse(Pastries < 3, 1, 0)

Nuts <- Nuts * 7
Ns <- c()
  Ns <- ifelse(Nuts >= 1, 1, 0)

WMs <- c()
  WMs <- ifelse(WhiteMeat == 1, 1, 0)

Sofritos <- Sofritos * 7
Ss <- c()
  Ss <- ifelse(Sofritos >= 2, 1, 0)


score <- data.frame(OOs, OOps, Vs, Frs, RMs, SDs, Ws, Ls, Fis, Pts, Ns, WMs, Ss)
  score$absolute <- apply(score, 1, function(x) sum(x, na.rm = rm.na))
  score$percent <- round(100 * score$absolute/14, 1)


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
