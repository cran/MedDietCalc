\name{computePitsavos}
\encoding{UTF-8}
\alias{computePitsavos}
\title{
computePitsavos
}
\description{
Computes the Mediterranean Diet adherence score developed by Pitsavos et al. in 2005, it can alo be found as Dietary Score (see Details).
}
\usage{
computePitsavos(data, WholeCereals, Fruit, Vegetables, Potatoes,
                Legumes, OliveOil, OOmeasure = "gr", Fish, Meat,
                Poultry, WholeDairy, Wine, output = "percent",
                frequency = "daily", rm.na = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns.}
  \item{WholeCereals}{Numeric variable with Whole Cereals consumption as servings.}
  \item{Fruit}{Numeric variable with Fruit consumption as servings.}
  \item{Vegetables}{Numeric variable with Vegetables consumption as servings.}
  \item{Potatoes}{Numeric variable with Potatoes consumption as servings.}
  \item{Legumes}{Numeric variable with Legumes consumption as servings.}
  \item{OliveOil}{Numeric variable with olive oil consumption, measure is set with the 'OOmeasure' argument.}
  \item{OOmeasure}{Character string which informs about the unit of the argument 'OliveOil'. Allowed values are 'gr', 'ml' and 'serving', which means respectively grams, mililiters and servings of 1 table spoon (15 ml).}
  \item{Fish}{Numeric variable with Fish consumption as servings.}
  \item{Meat}{Numeric variable with Meat consumption as servings.}
  \item{Poultry}{Numeric variable with Poultry consumption as servings.}
  \item{WholeDairy}{Numeric variable with fish consumption as servings.}
  \item{Wine}{Numeric variable with Wine consumption as glasses.}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame', 'score' and 'percent' (default).}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned,
 but percent of score adherence will be computed with basis of the whole score range (see Details)}
}
\details{
We have chosen to name this score by its first author name (Pitsavos), althought it can be found in the literature as Dietary Score (DS) [Milà-Villarroel, 2011; D'Alesandro-De Pergola, 2015] or as a derivate from MDS (Waijers et al. [Waijers et al., 2007] refer to it as MDS-a IV)
}
\value{
 Computed score. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'score'}{Instead of the full data.frame, just the integer vector corresponding to the absolute points of adherence to Mediterranean Diet for each person, from 0 (min.) to 9 (max.)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person, from 0 (min. adherence) to 100 percent (max. adhrence)}
}

\references{
D'Alessandro, Annunziata, and Giovanni De Pergola. 2015. "Mediterranean Diet and Cardiovascular Disease: A Critical Evaluation of A Priori Dietary Indexes." Nutrients 7 (9): 7863-88. doi:10.3390/nu7095367.

Mila-Villarroel, Raimon, Anna Bach-Faig, Josep Puig, Anna Puchal, Andreu Farran, Lluis Serra-Majem, and Josep Lluis Carrasco. 2011. "Comparison and Evaluation of the Reliability of Indexes of Adherence to the Mediterranean Diet." Public Health Nutrition 14 (12A): 2338-45. doi:10.1017/S1368980011002606.

Pitsavos, Christos, Demosthenes B. Panagiotakos, Natalia Tzima, Christina Chrysohoou, Manolis Economou, Antonis Zampelas, and Christodoulos Stefanadis. 2005. "Adherence to the Mediterranean Diet Is Associated with Total Antioxidant Capacity in Healthy Adults: The ATTICA Study". The American Journal of Clinical Nutrition 82 (3): 694-99. http://ajcn.nutrition.org/content/82/3/694.

Waijers, Patricia M. C. M., Edith J. M. Feskens, and Marga C. Ocke. 2007. "A Critical Review of Predefined Diet Quality Scores." British Journal of Nutrition 97 (2): 219-231. doi:10.1017/S0007114507250421.


}
\author{
Miguel Menendez
}

\examples{
data(nutriSample)


MedDiet <- computePitsavos(data = nutriSample,
  WholeCereals = P56rac + ifelse(nutriSample$P63_2 == 2, nutriSample$P61rac, 0),
  Fruit = P50rac + P52rac,
  Vegetables = P41rac + P42rac,
  Potatoes = P43rac + P44rac + P45rac,
  Legumes = P46rac,
  OliveOil = Aceitegr,
  OOmeasure = "gr",
  Fish = P35rac + P36rac + P37rac + P38rac,
  Meat = P29rac + P30rac + P31rac + P32rac,
  Poultry = P33rac,
  WholeDairy = P19grCom + P22grCom,
  Wine = P96rac,
  output = "percent", frequency = "daily", rm.na = FALSE)

hist(MedDiet)

}

