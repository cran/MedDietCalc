\name{computeFRESCO}
\encoding{UTF-8}
\alias{computeFRESCO}
\title{
computeFRESCO
}
\description{
Computes 10-year risk of fatal or non-fatal stroke and Coronary Heart Disease according to FRESCO score ('Función de Riesgo ESpañola de acontecimientos Coronarios y Otros', 'Spanish risk function of coronary and other cardiovascular events').
}
\usage{
computeFRESCO(data, outcome = c("Coronary", "Stroke", "All"), simplified = FALSE,
              Sex, Age, Smoker, BMI,
              Diabetes, SBP, TotChol, HDL, HBPpill,
              men = "male", women = "female")
}
\arguments{
  \item{data}{list or data.frame which contains the variables}
  \item{outcome}{character string indicating for which outcome risk is to be computed. Allowed values are "Coronary", "Stroke" or "All", which means the output is the risk of a coronary event, stroke, or both}
  \item{simplified}{logical. Original FRESCO score was derived in two versions: the full one, which includes all the following variables; and the other is de simplified one, which uses just sex, age, smoking status and body mass index. If TRUE, the simplified version will be computed.}
  \item{Sex}{variable containing gender of the people. It can be character, factor or numeric, as far as the 'men' and 'women' arguments specify how the formula should handle this variable (See below)}
  \item{Age}{numeric with people age in years}
  \item{Smoker}{numeric variable containg smoking status. 0 = non smoker, 1 = currently smoker}
  \item{BMI}{numeric variable with Body Mass Index (weight[kilograms] / height²[meters])}
  \item{Diabetes}{numeric which informs wether the person is diabetic. 0 = no, 1 = yes.}
  \item{SBP}{numeric variable with Systolic Blood Pressure in mmHg}
  \item{TotChol}{numeric with total serum cholesterol in mg/dl}
  \item{HDL}{numeric with serum High Density Lipoprotein cholesterol in mg/dl}
  \item{HBPpill}{numeric which means if the person is currently under treatment because of High Blood Pressure. 0 = no, 1 = yes.}
  \item{men}{character with informs of how males have been recorded in the 'Sex' argument, default is 'male'. If 'Sex' is numeric, a quoted number should be provided (for instance, men = '1'}
  \item{women}{character. Same meaning as 'men' argument, but for females.}
}
\details{
 In Spanish population, Framingham-REGICOR function tends to overestimate cardio and cerebrovascular risk. So, FRESCO score was developed among people from 35 to 79 years, which includes a simplified version with no laboratory results, and another one a bit harder to compute with slightly improved prediction ability.
}
\value{
Numeric vector of same length as rows in 'data' with estimated percentage of 10-year risk of fatal or non-fatal event (Coronary Heart Diesease, or stroke or both depending on 'outcome' argument).
}
\references{
Marrugat, Jaume, Isaac Subierana, Rafael Ramos, Joan Vila, Alejandro Marin-Ibanez, Maria Jesus Guembe, Fernando Rigo, et al. 2014. "Derivation and Validation of a Set of 10-Year Cardiovascular Risk Predictive Functions in Spain: The FRESCO Study." Preventive Medicine 61 (April): 66-74. doi:10.1016/j.ypmed.2013.12.031.
}
\author{
Miguel Menendez
}

\examples{
myself <- list(sex = "male", age = 32, tobacco = 0, bmi = 21.5)
computeFRESCO(data = myself, outcome = "All", simplified = TRUE,
              Sex = sex, Age = age, Smoker = tobacco, BMI = bmi)
}
