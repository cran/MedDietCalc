\name{computeCardio}
\encoding{UTF-8}
\alias{computeCardio}

\title{
computeCardio
}

\description{
Computes Cardioprotective Mediterranean Diet Index
}
\usage{
computeCardio(data = NULL, Vegetables, Fruit, OliveOil, OOmeasure = "gr",
              Legumes, Fish, Meat, RefinedRice, RefinedBread, WholeBread, Wine,
              frequency = "percent", output = "percent", rm.na = FALSE)
}


\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns.}
  \item{Vegetables}{Numeric variable with vegetables consumption as servings.}
  \item{Fruit}{Numeric variable with fruit consumption as servings.}
  \item{OliveOil}{Numeric variable with olive oil consumption, measure is set with the 'OOmeasure' argument.}
  \item{OOmeasure}{Character string which informs about the unit of the argument 'OliveOil'. Allowed values are 'gr', 'ml' and 'serving', which means respectively grams, mililiters and servings of 1 table spoon (15 ml).}
  \item{Legumes}{Numeric variable with legumes consumption as servings.}
  \item{Fish}{Numeric variable with fish consumption as servings.}
  \item{Meat}{Numeric variable with meat and meat products consumption as servings.}
  \item{RefinedRice}{Numeric variable with consumption of refined rice as servings.}
  \item{RefinedBread}{Numeric variable with consumption of refined bread as servings.}
  \item{WholeBread}{Numeric variable with consumption of whole bread as servings.}
  \item{Wine}{Numeric variable with wine consumption as glasses.}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame', 'score' and 'percent' (default).}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned, but percent of score adherence will be computed with basis of the whole score range (see Details)}

}
\details{
In the score, the item about refined and whole cereals is scored positively if consumption of both white bread and rice is low or when consumption of whole-grain bread is high. Rice and whole-grain bread are considered weekly, and white bread daily: [White bread (< 1 serving/day) AND rice (< 1 serving/week)] OR whole-grain bread (> 5 servings/week). The function takes as arguments the three foods, with whatever periodicity they have been recorded in the data, as long as it is provided with the 'frequency' argument. Internally function sets them in the suitable fashion to test this score item.

There is an aditional item in the score, computed internally, that provides one point if both vegetables and fruit consumption have received 1 point each one.
}

\value{Computed Cardio score. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'score'}{Instead of the full data.frame, just the integer vector corresponding to the absolute points of adherence to Mediterranean Diet for each person, from 0 (min.) to 9 (max.)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person, from 0 (min. adherence) to 100 percent (max. adhrence)}
}


\references{
Martinez-Gonzalez, M. A., E. Fernandez-Jarne, M. Serrano-Martinez, M. Wright, and E. Gomez-Gracia. 2004. 'Development of a Short Dietary Intake Questionnaire for the Quantitative Estimation of Adherence to a Cardioprotective Mediterranean Diet'. European Journal of Clinical Nutrition 58 (11): 1550-52. doi:10.1038/sj.ejcn.1602004.

}
\author{
Miguel Menendez
}

\examples{
data(nutriSample)

MedDiet <- computeCardio(data = nutriSample,
                          OliveOil = Aceitegr,
                          OOmeasure = "gr",
                          Fruit = P50rac + P52rac,
                          Vegetables = P41rac + P42rac,
                          Legumes = P46rac,
                          Fish = P35rac + P36rac + P37rac + P38rac,
                          Wine = P96rac,
                          Meat = P29rac + P30rac + P31rac + P32rac,
                          RefinedBread = P55rac,
                          RefinedRice = P61rac,
                          WholeBread = P56rac,
                          frequency = "daily", output = "percent", rm.na = FALSE)
hist(MedDiet)
}


\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
