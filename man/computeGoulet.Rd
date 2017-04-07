\name{computeGoulet}
\encoding{UTF-8}
\alias{computeGoulet}

\title{
computeGoulet
}
\description{
Computes Mediterranean Diet adherence score according to Goulet et al. in 2003.
}

\usage{
computeGoulet(data, WholeCereals, Vegetables, Fruit, LegumesAndNuts, OliveOil,
              OOmeasure = "gr", Olives, Dairy, Fish, Poultry, Eggs, Sweets, Meat,
              output = "percent", frequency = "daily", rm.na = FALSE)
}

\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns.}
  \item{WholeCereals}{Numeric variable with consumption of whole grain products as servings.}
  \item{Vegetables}{Numeric variable with vegetables consumption as servings.}
  \item{Fruit}{Numeric variable with fruit consumption as servings.}
  \item{LegumesAndNuts}{Numeric variable with legumes, nuts and seed consumption as servings.}
  \item{OliveOil}{Numeric variable with olive oil consumption, measure is set with the 'OOmeasure' argument.}
  \item{OOmeasure}{Character string which informs about the unit of the argument 'OliveOil'. Allowed values are 'gr', 'ml' and 'serving', which means respectively grams, mililiters and servings of 1 table spoon (15 ml).}
  \item{Olives}{Numeric variable with olives consumption as servings.}
  \item{Dairy}{Numeric variable with dairy consumption as servings.}
  \item{Fish}{Numeric variable with fish consumption as servings.}
  \item{Poultry}{Numeric variable with poultry (other than breaded) consumption as servings.}
  \item{Eggs}{Numeric variable with eggs consumption as servings.}
  \item{Sweets}{Numeric variable with sweets consumption as servings.}
  \item{Meat}{Numeric variable with red meat and meat products consumption as servings.}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame', 'score' and 'percent' (default).}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned, but percent of score adherence will be computed with basis of the whole score range (see Details)}


}
\details{
Computes Mediterranean Diet adherence score according to Goulet et al. in 2003. It can be found as Mediterranean Score (MS) [Mila-Villarroel et al., 2011].
}

\value{
Computed Mediterranean Diet Adherence score according to Goulet et al. 2003. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'score'}{Instead of the full data.frame, just the integer vector corresponding to the absolute points of adherence to Mediterranean Diet for each person, from 0 (min.) to 44 (max.)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person, from 0 (min. adherence) to 100 percent (max. adhrence)}
}

\references{
Goulet, Julie, Benoıt Lamarche, Genevieve Nadeau, and Simone Lemieux. 2003. 'Effect of a Nutritional Intervention Promoting the Mediterranean Food Pattern on Plasma Lipids, Lipoproteins and Body Weight in Healthy French-Canadian Women'. Atherosclerosis 170 (1): 115-24. doi:10.1016/S0021-9150(03)00243-0.


Mila-Villarroel, Raimon, Anna Bach-Faig, Josep Puig, Anna Puchal, Andreu Farran, Lluis Serra-Majem, and Josep Lluis Carrasco. 2011. 'Comparison and Evaluation of the Reliability of Indexes of Adherence to the Mediterranean Diet'. Public Health Nutrition 14 (12A): 2338-45. doi:10.1017/S1368980011002606.

}

\author{
Miguel Menendez
}


\examples{
data(nutriSample)
MedDiet <- computeGoulet(data = nutriSample,
  WholeCereals = P56rac + ifelse(nutriSample$P63_2 == 2, nutriSample$P61rac, 0),
  Vegetables = P41rac + P42rac,
  Fruit = P50rac + P52rac,
  LegumesAndNuts = P46rac + P53rac + P75rac,
  OliveOil = Aceitegr,
  OOmeasure = "gr",
  Olives = P54rac,
  Dairy = P19rac + P20rac + P20rac + P22rac + P23rac + P24rac + P25rac + P26rac + P27rac,
  Fish = P35rac + P36rac + P37rac + P38rac,
  Poultry = P33rac,
  Eggs = P28rac,
  Sweets = P69rac + P70rac + P71rac + P72rac + P73rac,
  Meat = P29rac + P30rac + P31rac + P32rac,
  output = "percent", frequency = "daily", rm.na = FALSE)
hist(MedDiet)
}
