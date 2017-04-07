\name{computeSofi}
\encoding{UTF-8}
\alias{computeSofi}

\title{
ComputeSofi
}
\description{
Computes Mediterranean Diet adherence score accoring to the literature review by Sofi et al. in 2014.
}
\usage{
computeSofi(data, Fruit, Vegetables, Legumes, Cereals,
            Fish, Meat, Dairy, Alcohol, OliveOil,
            output = "percent", rm.na = FALSE, frequency = NULL)

}

\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns}
  \item{Fruit}{Numeric variable with fruit consumption as servings (1 serving: 150g)}
  \item{Vegetables}{Numeric variable with vegetables consumption as servings (1 serving: 100g)}
  \item{Legumes}{Numeric variable with legumes consumption as servings (1 serving: 70g)}
  \item{Cereals}{Numeric variable with cereal consumption as servings (1 serving: 130g)}
  \item{Fish}{Numeric variable with fish consumption as servings (1 serving: 100g)}
  \item{Meat}{Numeric variable with meat and meat products consumption as servings (1 serving: 80g)}
  \item{Dairy}{Numeric variable with dairy consumption as servings (1 serving: 180g)}
  \item{Alcohol}{Numeric variable with alcohol intake as Alcohol Units (1 Alcohol Unit: 12g)}
  \item{OliveOil}{Integer variable indicating if olive oil consumption is consumed as 0 = occasional use, 1 = frequent use or 2 = regular use}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame', 'score' and 'percent' (default).}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned, but percent of score adherence will be computed with basis of the whole score range (see Details)}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}
}

\details{
This questionnaire vas developed after a systematic literature review (Sofi et al., 2014). To set its cut-offs it considered the amounts of food in the included studies, which studied adherence to mediterranean diet and health status.

}

\value{Computed score according to Sofi et al. 2014. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'score'}{Instead of the full data.frame, just the integer vector corresponding to the absolute points of adherence to Mediterranean Diet for each person, from 0 (min.) to 18 (max.)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person, from 0 (min. adherence) to 100 percent (max. adhrence)}
}

\references{
Sofi, Francesco, Claudio Macchi, Rosanna Abbate, Gian Franco Gensini, and Alessandro Casini. 2014. 'Mediterranean Diet and Health Status: An Updated Meta-Analysis and a Proposal for a Literature-Based Adherence Score'. Public Health Nutrition 17 (12): 2769-82. doi:10.1017/S1368980013003169.
}

\author{
Miguel Menendez
}


\examples{
data(nutriSample)

# wether olive oil is principal or not is stored in the sample dataset
# in a different way than asked by formula.
# In the data set it is 1=olive oil, 2=seeds oil, 3=both
# so a transformation is performed:
Oil <- ifelse(nutriSample$AceiteTipo == 2, 0,
              ifelse(nutriSample$AceiteTipo == 3, 1,
                     ifelse(nutriSample$AceiteTipo == 1, 2, 0)))

Sofi <- computeSofi(data = nutriSample,
              Fruit = P50rac + P52rac,
              Vegetables = P41rac +  P42rac,
              Legumes = P46rac,
              Cereals = P55rac + P56rac + P57rac + P59rac + P60rac + P61rac + P62rac,
              Fish = P35rac + P36rac + P37rac + P38rac,
              Meat = P29rac + P30rac + P31rac + P32rac,
              Dairy = P19rac + P20rac + P20rac + P22rac + P23rac +
                      P24rac + P25rac + P26rac + P27rac,
              Alcohol = P94rac + P96rac + P97rac + P98rac + P99rac,
              OliveOil = Oil,
              output = "data.frame", rm.na = FALSE, frequency = "daily")

hist(Sofi$percent)
}
