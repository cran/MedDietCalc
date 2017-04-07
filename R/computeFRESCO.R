computeFRESCO <- function(data, outcome = c("Coronary", "Stroke", "All"), simplified = FALSE,
                          Sex, Age, Smoker, BMI,
                          Diabetes, SBP, TotChol, HDL, HBPpill,
                          men="male", women="female") {


  arguments <- as.list( match.call() )
  Sex <- eval(arguments$Sex, data)
  Age <- as.numeric(eval(arguments$Age, data))
  Smoker <- as.numeric(eval(arguments$Smoker, data))
  BMI <- as.numeric(eval(arguments$BMI, data))
  DM <- as.numeric(eval(arguments$Diabetes, data))
  SBP <- as.numeric(eval(arguments$SBP, data))
  TotChol <- as.numeric(eval(arguments$TotChol, data))
  HDL <- as.numeric(eval(arguments$HDL, data))
  HBPpill <- as.numeric(eval(arguments$HBPpill, data))



  BMI[BMI < 30] <- 0
  BMI[BMI >= 30] <- 1


  if(simplified == TRUE){
    # As the model selected is the simplified, these are its coefficients:
    if(outcome == "Coronary"){
      basal <- ifelse(Sex == men, 0.962, 0.987)
      Ac <- ifelse(Sex == men, 0.053, 0.080)
      Sc <- ifelse(Sex == men, 0.466, 0.776)
      Bc <- ifelse(Sex == men, 0.331, 0.217)
    } else {
      if(outcome == "Stroke"){
        basal <- ifelse(Sex == men, 0.980, 0.990)
        Ac <- ifelse(Sex == men, 0.075, 0.097)
        Sc <- ifelse(Sex == men, 0.116, 0.864)
        Bc <- ifelse(Sex == men, 0.091, -0.192)
      } else {
        if(outcome == "All"){
          basal <- ifelse(Sex == men, 0.942, 0.978)
          Ac <- ifelse(Sex == men, 0.061, 0.089)
          Sc <- ifelse(Sex == men, 0.345, 0.856)
          Bc <- ifelse(Sex == men, 0.251, 0.011)
        } else {stop("Please, select the outcome for which you want to compute risk with the 'outcome' argument")}
      }
    }

    exponent <- numeric()
    exponent <- Age*Ac/10 + Smoker*Sc + BMI*Bc



  } else {
    # These are the full model coefficients

    if(outcome == "Coronary"){
      basal <- ifelse(Sex == men, 0.969, 0.990)
      Ac <- ifelse(Sex == men, 0.241, 0.066)
      Sc <- ifelse(Sex == men, 2.453, 0.784)
      Bc <- ifelse(Sex == men, 0, 0) # full model doesn`t use Body Mass Index
      Dc <- ifelse(Sex == men, 0.528, 0.778)
      BPc <- ifelse(Sex == men, 0.888, 0.038)
      Cc <- ifelse(Sex == men, 0.061, 0.077)
      Hc <- ifelse(Sex == men, -0.211, -0.272)
      I1c <- ifelse(Sex == men, 0.519, 0.133)
      I2c <- ifelse(Sex == men, -0.034, 0)
      I3c <- ifelse(Sex == men, -0.013, 0)
    } else {
      if(outcome == "Stroke"){
        basal <- ifelse(Sex == men, 0.981, 0.991)
        Ac <- ifelse(Sex == men, 0.058, 0.088)
        Sc <- ifelse(Sex == men, 0.095, 0.775)
        Bc <- ifelse(Sex == men, 0, 0)
        Dc <- ifelse(Sex == men, 0.519, 0.513)
        BPc <- ifelse(Sex == men, 0.176, 0.030)
        Cc <- ifelse(Sex == men, 0.009, -0.020)
        Hc <- ifelse(Sex == men, -0.005, -0.038)
        I1c <- ifelse(Sex == men, -0.027, 0.054)
        I2c <- ifelse(Sex == men, 0, 0)
        I3c <- ifelse(Sex == men, 0, 0)
      } else {
        if(outcome == "All"){
          basal <- ifelse(Sex == men, 0.951, 0.981)
          Ac <- ifelse(Sex == men, 0.198, 0.077)
          Sc <- ifelse(Sex == men, 1.913, 0.826)
          Bc <- ifelse(Sex == men, 0, 0)
          Dc <- ifelse(Sex == men, 0.519, 0.684)
          BPc <- ifelse(Sex == men, 0.728, 0.038)
          Cc <- ifelse(Sex == men, 0.045, 0.032)
          Hc <- ifelse(Sex == men, -0.139, -0.168)
          I1c <- ifelse(Sex == men, 0.330, 0.119)
          I2c <- ifelse(Sex == men, -0.026, 0)
          I3c <- ifelse(Sex == men, -0.010, 0)
        } else {stop("Please, select the outcome for which you want to compute risk with the 'outcome' argument")}
      }
    }

    exponent <- Age*Ac/10 + Smoker*Sc + BMI*Bc + DM*Dc + SBP*BPc/10 + TotChol*Cc/10 + HDL*Hc/10 + ifelse(HBPpill == 1 & SBP > 120, I1c, 0) + I2c*(Age/10)*Smoker + I3c*(Age/10)*(SBP/10)
  }

  Risk <- basal^exp(exponent)
  round(100 - (Risk * 100), 2)
}




