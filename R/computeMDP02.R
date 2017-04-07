computeMDP02 <- function (data, OliveOil, OOmeasure = "gr", Fiber, Fruit, Vegetables, Fish,
                          Alcohol, Meat, RefinedCereals,
                          output = "percent", rm.na = FALSE, frequency = "daily") {

  arguments <- as.list( match.call() )
  OO <- eval(arguments$OliveOil, data)
  Fi <- eval(arguments$Fiber, data)
  Fr <- eval(arguments$Fruit, data)
  Ve <- eval(arguments$Vegetables, data)
  Fs <- eval(arguments$Vegetables, data)
  Al <- eval(arguments$Alcohol, data)
  Me <- eval(arguments$Meat, data)
  RC <- eval(arguments$RefinedCereals, data)

  # This code chunk just tries to improve legibility reducing typing needs.
  q1 <- function(x) {stats::quantile(x, probs=0.2, na.rm = TRUE)}
  q2 <- function(x) {stats::quantile(x, probs=0.4, na.rm = TRUE)}
  q3 <- function(x) {stats::quantile(x, probs=0.6, na.rm = TRUE)}
  q4 <- function(x) {stats::quantile(x, probs=0.8, na.rm = TRUE)}


  # this code chunk tests if data has not been introduced in a daily fashion, and if so, transform data to daily consumption
  if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

  if(frequency == "weekly" || frequency == "monthly"){
    Vars <- list(OO = OO, Fi = Fi, Fr = Fr, Ve = Ve, Fs = Fs, Al = Al, Me = Me, RC = RC)

    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

    OO <- Vars$OO
    Fi <- Vars$Fi
    Fr <- Vars$Fr
    Ve <- Vars$Ve
    Fs <- Vars$Fs
    Al <- Vars$Al
    Me <- Vars$Me
    RC <- Vars$RC

  } else {
    if(frequency != "daily"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
  }



  if(OOmeasure == "gr") {OO <- OO/0.918/15}
  else {
    if(OOmeasure == "ml") {OO <- OO/15}
    else {
      if(OOmeasure == "serving") {"no conversion needed"}
      else {stop("check units of Olive Oil")
      }
    }
  }

  OOs <- c()
    OOs[OO < q1(OO)] <- 1
    OOs[OO >= q1(OO) & OO < q2(OO)] <- 2
    OOs[OO >= q2(OO) & OO < q3(OO)] <- 3
    OOs[OO >= q3(OO) & OO < q4(OO)] <- 4
    OOs[OO >= q4(OO)] <- 5


  Fis <- c()
    Fis[Fi < q1(Fi)] <- 1
    Fis[Fi >= q1(Fi) & Fi < q2(Fi)] <- 2
    Fis[Fi >= q2(Fi) & Fi < q3(Fi)] <- 3
    Fis[Fi >= q3(Fi) & Fi < q4(Fi)] <- 4
    Fis[Fi >= q4(Fi)] <- 5

  Frs <- c()
    Frs[Fr < q1(Fr)] <- 1
    Frs[Fr >= q1(Fr) & Fr < q2(Fr)] <- 2
    Frs[Fr >= q2(Fr) & Fr < q3(Fr)] <- 3
    Frs[Fr >= q3(Fr) & Fr < q4(Fr)] <- 4
    Frs[Fr >= q4(Fr)] <- 5

  Ves <- c()
    Ves[Ve < q1(Ve)] <- 1
    Ves[Ve >= q1(Ve) & Ve < q2(Ve)] <- 2
    Ves[Ve >= q2(Ve) & Ve < q3(Ve)] <- 3
    Ves[Ve >= q3(Ve) & Ve < q4(Ve)] <- 4
    Ves[Ve >= q4(Ve)] <- 5

  Fss <- c()
    Fss[Fs < q1(Fs)] <- 1
    Fss[Fs >= q1(Fs) & Fs < q2(Fs)] <- 2
    Fss[Fs >= q2(Fs) & Fs < q3(Fs)] <- 3
    Fss[Fs >= q3(Fs) & Fs < q4(Fs)] <- 4
    Fss[Fs >= q4(Fs)] <- 5

  Als <- c()
    Als[Al < q1(Al)] <- 1
    Als[Al >= q1(Al) & Al < q2(Al)] <- 2
    Als[Al >= q2(Al) & Al < q3(Al)] <- 3
    Als[Al >= q3(Al) & Al < q4(Al)] <- 4
    Als[Al >= q4(Al)] <- 5

  Mes <- c()
    Mes[Me < q1(Me)] <- 5
    Mes[Me >= q1(Me) & Me < q2(Me)] <- 4
    Mes[Me >= q2(Me) & Me < q3(Me)] <- 3
    Mes[Me >= q3(Me) & Me < q4(Me)] <- 2
    Mes[Me >= q4(Me)] <- 1

  RCs <- c()
    RCs[RC < q1(RC)] <- 5
    RCs[RC >= q1(RC) & RC < q2(RC)] <- 4
    RCs[RC >= q2(RC) & RC < q3(RC)] <- 3
    RCs[RC >= q3(RC) & RC < q4(RC)] <- 2
    RCs[RC >= q4(RC)] <- 1


  score <- data.frame(OOil = OOs, Fiber = Fis, Fruit = Frs, Vegtbls = Ves, Fish = Fss, Alcohol = Als, Meat = Mes, RefCer = RCs)
  score$absolute <- apply(score, 1, function(x) sum(x, na.rm = rm.na))
  score$percent <- round(100 * score$absolute/40, 1)


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
