computeMSDPS <- function(data, WholeCereals, Fruit, Vegetables, Dairy, Wine,
                         Fish, Poultry, LegumesAndMore, Potatoes, Eggs, Sweets,
                         Meat, OOprincipal,
                         WholeCerealsK, FruitK, VegetablesK, DairyK, WineK,
                         FishK, PoultryK, LegumesAndMoreK, PotatoesK, EggsK, SweetsK,
                         MeatK, OliveOilK, Kcal,
                         Sex, men = "male", women = "female",
                         output = "percent", frequency = "daily", rm.na = FALSE){

  arguments <- as.list( match.call() )
  WC <- eval(arguments$WholeCereals, data)
  Fr <- eval(arguments$Fruit, data)
  Ve <- eval(arguments$Vegetables, data)
  Da <- eval(arguments$Dairy, data)
  Wi <- eval(arguments$Wine, data)
  Fs <- eval(arguments$Fish, data)
  Pl <- eval(arguments$Poultry, data)
  LNO <- eval(arguments$LegumesAndMore, data)
  Pt <- eval(arguments$Potatoes, data)
  Eg <- eval(arguments$Eggs, data)
  Sw <- eval(arguments$Sweets, data)
  Me <- eval(arguments$Meat, data)
  OO <- eval(arguments$OOprincipal, data)
  Sex <- eval(arguments$Sex, data)

  WCK <- eval(arguments$WholeCerealsK, data)
  FrK <- eval(arguments$FruitK, data)
  VeK <- eval(arguments$VegetablesK, data)
  DaK <- eval(arguments$DairyK, data)
  WiK <- eval(arguments$WineK, data)
  FsK <- eval(arguments$FishK, data)
  PlK <- eval(arguments$PoultryK, data)
  LNOK <- eval(arguments$LegumesAndMoreK, data)
  PtK <- eval(arguments$PotatoesK, data)
  EgK <- eval(arguments$EggsK, data)
  SwK <- eval(arguments$SweetsK, data)
  MeK <- eval(arguments$MeatK, data)
  OOK <- eval(arguments$OliveOilK, data)
  Kcal <- eval(arguments$Kcal, data)

  # This code chunk checks the 'Sex' argument to identify if it contents enough information to understand sex labels, or if it needs the 'men' and 'women' arguments
  if((missing(men) || missing(women)) && is.numeric(Sex)) {stop("'Sex' argument is numeric, and the function knows not how to handle it, please, provide 'men' and 'women' arguments (v.gr. men=1, women=2)")}

  if(missing(men) || missing(women) && (is.factor(Sex) || is.character(Sex))) {
    if(any(levels(Sex)) %in% c("man", "male", "MAN", "Male", "MALE")) {men <- levels(Sex)[which(levels(Sex)) %in% c("man", "male", "MAN", "Male", "MALE")]
    } else {
      if(any(levels(Sex)) %in% c("woman", "female", "WOMAN", "Female", "FEMALE")) {women <- levels(Sex)[which(levels(Sex)) %in% c("woman", "female", "WOMAN", "Female", "FEMALE")]
      } else {
        if(any(names(table(Sex)) %in% c("man", "male", "MAN", "Male", "MALE"))) {men <- names(table(Sex))[which(names(table(Sex)) %in% c("man", "male", "MAN", "Male", "MALE"))]
        } else {
          if(any(names(table(Sex)) %in% c("woman", "female", "WOMAN", "Female", "FEMALE"))) {women <- names(table(Sex))[which(names(table(Sex)) %in% c("woman", "female", "WOMAN", "Female", "FEMALE"))]
          } else {stop("function knows not to handle the 'Sex' argument, please, set values for men and women identification with 'men' and 'women' arguments")}
        }
      }
    }
  }


  # this code chunk tests if data has not been introduced in a monthly fashion, and if so, transform data to montly consumption
  if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

  if(frequency == "monthly" || frequency == "weekly"){
    Vars <- list(WC = WC, Fr = Fr, Ve = Ve, Da = Da, Wi = Wi, Fs = Fs, Pl = Pl, LNO = LNO,
                 Pt = Pt, Eg = Eg, Sw = Sw, Me = Me, OO = OO)
    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

    WC <- Vars$WC
    Fr <- Vars$Fr
    Ve <- Vars$Ve
    Da <- Vars$Da
    Wi <- Vars$Wi
    Fs <- Vars$Fs
    Pl <- Vars$Pl
    LNO <- Vars$LNO
    Pt <- Vars$Pt
    Eg <- Vars$Eg
    Sw <- Vars$Sw
    Me <- Vars$Me
    OO <- Vars$OO

  } else {
    if(frequency != "daily"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
  }


  Item <- function(food, Servings){
    De <- 100 * sqrt((Servings - food)^2) / Servings # Percent of deviation from the recomended amount of servings
    S <- 10 - (De / 10) # 10 is the maximun item Score, from it, deviation is sustracted (as tens)
    S[S < 0] <- 0 # if
    return(S)
  }

  WCs <- Item(WC, 8)
  Frs <- Item(Fr, 3)
  Ves <- Item(Ve, 6)
  Das <- Item(Da, 2)

  Wis <- numeric(length = nrow(data))
  Wis[Sex == men] <- Item(Wi[Sex == men], 3)
  Wis[Sex == women] <- Item(Wi[Sex == women], 1.5)

  # Some items are scored in a weekly fashion
  Fss <- Item(7 * Fs, 6)
  Pls <- Item(7 * Pl, 4)
  LNOs <- Item(7 * LNO, 4)
  Pts <- Item(7 * Pt, 3)
  Egs <- Item(7 * Eg, 3)
  Sws <- Item(7 * Sw, 3)
  Mes <- Item(7 * Me, 1)

  OOs <- ifelse(OO == 2, 10,
                ifelse(OO == 1, 5, 0))


  score <- data.frame(WCs, Frs, Ves, Das, Wis, Fss, Pls, LNOs, Pts, Egs, Sws, Mes, OOs)
  score <- round(score, 1) # otherwise output 'data.frame' is hard to read

  score$absolute <- apply(score, 1, function(x) sum(x, na.rm = rm.na))
  score$percentBrute <- round(100 * score$absolute / 130, 1)

  # Corection factor "P"
  P <- (WCK + FrK + VeK + DaK + WiK + FsK + PlK + LNOK + PtK + EgK + SwK + MeK + OOK) / Kcal
  score$percent <- round(score$percentBrute * P, 1)

  if(missing(output) || output == "percent") {return(score$percent)
  } else {
    if(output == "data.frame") {return(score)
    } else {
      stop("please, select a valid output argument, admited values are 'percent' -default- and 'data.frame' " )
    }
  }

}
