\name{computeMDP02}
\encoding{UTF-8}
\alias{computeMDP02}

\title{
computeMDP02
}
\description{
Computes Mediterranean Diet adherence score known as Mediterranean Dietary Pattern, by Martinez-Gonzalez et al. 2002.
}
\usage{
computeMDP02(data, OliveOil, OOmeasure = "gr", Fiber, Fruit, Vegetables, Fish,
             Alcohol, Meat, RefinedCereals,
             output = "percent", rm.na = FALSE, frequency = "daily")
}

\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns}
  \item{OliveOil}{Numeric variable with olive oil consumption, measure is set with the 'OOmeasure' argument}
  \item{OOmeasure}{Character string which informs about the unit of the argument 'OliveOil'. Allowed values are 'gr', 'ml' and 'serving', which means respectively grams, mililiters and servings of 1 table spoon (15 ml).}
  \item{Fiber}{Numeric variable with consumption of Dietary Fiber as grams.}
  \item{Fruit}{Numeric variable with consumption of Fruits as grams.}
  \item{Vegetables}{Numeric variable with Vegetables consumption as grams}
  \item{Fish}{Numeric variable with Fish consumption as grams}
  \item{Alcohol}{Numeric variable with Alcohol consumption as etanol grams from any beberage origin}
  \item{Meat}{Numeric variable with Meat and Meat Products consumption as grams}
  \item{RefinedCereals}{Numeric variable with Refined Cereals consumption as grams}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame', 'score' and 'percent' (default).}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned, but percent of score adherence will be computed with basis of the whole score range (see Details)}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}
}


\value{Computed MDP02 score. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'score'}{Instead of the full data.frame, just the integer vector corresponding to the absolute points of adherence to Mediterranean Diet for each person, from 5 (min.) to 40 (max.)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person, from 0 (min. adherence) to 100 percent (max. adhrence)}
}



\references{
Martinez-Gonzalez, Miguel A., Elena Fernandez-Jarne, Manuel Serrano-Martinez, Amelia Marti, J. Alfredo Martinez, and Jose M. Martin-Moreno. 2002. 'Mediterranean Diet and Reduction in the Risk of a First Acute Myocardial Infarction: An Operational Healthy Dietary Score'. European Journal of Nutrition 41 (4): 153-60. doi:10.1007/s00394-002-0370-6.
}

\author{
Miguel Menendez
}


\examples{
data(nutriSample)

MedDiet <- computeMDP02(data = nutriSample,
                        OliveOil = Aceitegr,
                        OOmeasure = "gr",
                        Fiber = totalFibra,
                        Fruit = P50grCom,
                        Vegetables = P41grCom + P42grCom,
                        Fish = P35grCom + P36grCom + P37grCom + P38grCom,
                        Alcohol = 12 * (P94rac + P96rac + P97rac + P98rac + P99rac),
                        Meat = P29grCom + P30grCom + P31grCom + P32grCom,
                        RefinedCereals = P55grCom + P61grCom,
                        output = "percent", rm.na = FALSE, frequency = "daily")
hist(MedDiet)
}

