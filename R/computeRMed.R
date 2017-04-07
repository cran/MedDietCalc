computeRMED <- function (data, FruitAndNuts, Vegetables, Legumes, Cereals, Fish,
                         OliveOil, OOmeasure = "gr", Meat, Dairy, Alcohol,
                         Kcal, Sex, men="male", women="female",
                         frequency = NULL, output = "percent", rm.na = FALSE) {

arguments <- as.list( match.call() )
  FruitAndNuts <- eval(arguments$FruitAndNuts, data)
  Vegetables <- eval(arguments$Vegetables, data)
  Legumes <- eval(arguments$Legumes, data)
  Cereals <- eval(arguments$Cereals, data)
  Fish <- eval(arguments$Fish, data)
  OliveOil <- eval(arguments$OliveOil, data)
  Meat <- eval(arguments$Meat, data)
  Dairy <- eval(arguments$Dairy, data)
  Alcohol <- eval(arguments$Alcohol, data)
  Kcal <- eval(arguments$Kcal, data)
  Sex <- eval(arguments$Sex, data)


  if(OOmeasure == "gr") {OliveOil <- OliveOil/0.918/15}
  else {
    if(OOmeasure == "ml") {OliveOil <- OliveOil/15}
    else {
      if(OOmeasure == "serving") {"no conversion needed"}
      else {stop("check units of Olive Oil")
      }
    }
  }


  # this code chunk tests if data has not been introduced in a daily fashion, and if so, transform data to daily consumption
  if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

  if(frequency == "weekly" || frequency == "monthly"){
    Vars <- list(FruitAndNuts = FruitAndNuts, Vegetables = Vegetables, Legumes = Legumes, Cereals = Cereals,
                 Fish = Fish, OliveOil = OliveOil, Meat = Meat, Dairy = Dairy, Alcohol = Alcohol, Kcal = Kcal)

    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

    FruitAndNuts <- Vars$FruitAndNuts
    Vegetables <- Vars$Vegetables
    Legumes <- Vars$Legumes
    Cereals <- Vars$Cereals
    Fish <- Vars$Fish
    OliveOil <- Vars$OliveOil
    Meat <- Vars$Meat
    Dairy <- Vars$Dairy
    Alcohol <- Vars$Alcohol
    Kcal <- Vars$Kcal

  } else {
    if(frequency != "daily"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
  }


tertile1 <- function(x) {stats::quantile(x, probs=0.3333, na.rm=T)}
tertile2 <- function(x) {stats::quantile(x, probs=0.6666, na.rm=T)}


FN <- 1000*FruitAndNuts/Kcal
  FNscore <- numeric()
  FNscore[FN < tertile1(FN)] <- 0
  FNscore[FN >= tertile1(FN) & FN < tertile2(FN)  ] <- 1
  FNscore[FN >= tertile2(FN)] <- 2


V <- 1000*Vegetables/Kcal
  Vscore <- numeric()
  Vscore[V < tertile1(V)] <- 0
  Vscore[V >= tertile1(V) & V < tertile2(V)] <- 1
  Vscore[V >= tertile2(V)] <- 2

L <- 1000*Legumes/Kcal
  Lscore <- numeric()
  Lscore[L < tertile1(L)] <- 0
  Lscore[L >= tertile1(L) & L < tertile2(L)] <- 1
  Lscore[L >= tertile2(L)] <- 2

Ce <- 1000*Cereals/Kcal
  Cescore <- numeric()
  Cescore[Ce < tertile1(Ce)] <- 0
  Cescore[Ce >= tertile1(Ce) & Ce < tertile2(Ce)] <- 1
  Cescore[Ce >= tertile2(Ce)] <- 2

Fi <- 1000*Fish/Kcal
  Fscore <- numeric()
  Fscore[Fi < tertile1(Fi)] <- 0
  Fscore[Fi >= tertile1(Fi) & Fi < tertile2(Fi)] <- 1
  Fscore[Fi >= tertile2(Fi)] <- 2

OO <- 1000*OliveOil/Kcal
  Oscore <- numeric()
  Oscore[OO < tertile1(OO)] <- 0
  Oscore[OO >= tertile1(OO) & OO < tertile2(OO)] <- 1
  Oscore[OO >= tertile2(OO)] <- 2

# please, note inverse scoring of meat and dairy
M <- 1000*Meat/Kcal
  Mscore <- numeric()
  Mscore[M < tertile1(M)] <- 2
  Mscore[M >= tertile1(M) & M < tertile2(M)] <- 1
  Mscore[M >= tertile2(M)] <- 0


Da <- 1000*Dairy/Kcal
  Dscore <- numeric()
  Dscore[Da < tertile1(Da)] <- 2
  Dscore[Da >= tertile1(Da) & Da < tertile2(Da)] <- 1
  Dscore[Da >= tertile2(Da)] <- 0

# Alcohol
# for men:
  #Alscore[Alcohol >= 10 & Alcohol <= 50] <- 2
  #Alscore[Alcohol < 10 | Alcohol > 50] <- 0
# for women:
  #Alscore[Alcohol >= 5 & Alcohol < 25] <- 2
  #Alscore[Alcohol < 5 | Alcohol > 25] <- 0

  Alscore <- numeric(length = nrow(data))
  Alscore[ifelse(Sex == men, Alcohol >= 10 & Alcohol <= 50, Alcohol >= 5 & Alcohol < 25)] <- 2
  Alscore[ifelse(Sex == men , Alcohol < 10 | Alcohol > 50, Alcohol < 5 | Alcohol > 25)] <- 0
  Alscore[Sex != men & Sex != women] <- NA

score <- data.frame(FNscore, Vscore, Lscore, Cescore, Fscore, Oscore, Mscore, Dscore, Alscore)
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
