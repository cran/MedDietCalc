periodicity <- function(x, OriginalFreq, TargetFreq){
  if(OriginalFreq == "daily") {
    if(TargetFreq == "weekly") {V <- 7
    } else {
      if(TargetFreq == "monthly") {V <- 30
      } else {
        if(TargetFreq == "daily") {V <- 1
        }
      }
    }
  } else {
    if(OriginalFreq == "weekly") {
      if(TargetFreq == "daily") {V <- 1/7
      } else {
        if(TargetFreq == "monthly") {V <- (1/7)*30
        } else {
          if(TargetFreq == "weekly") {V <- 1
          }
        }
      }
    } else {
      if(OriginalFreq == "monthly") {
        if(TargetFreq == "daily") {V <- 1/30
        } else {
          if(TargetFreq == "weekly") {V <- (1/30)*7
          } else {
            if(TargetFreq == "monthly") {V <- 1
            }
          }
        }
      }
    }
  }

  if(is.list(x)){
    lapply(x, periodicity, OriginalFreq, TargetFreq)
  } else {
    x*V
  }
}
