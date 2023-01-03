# FFTreesdata_doc.R
# Documenting all datasets.
# -------------------------


#  1. blood: ------

#' Blood donation data
#'
#' @format A data frame containing 748 rows and 5 columns.
#'
#' \describe{
#'   \item{recency}{Months since last donation}
#'   \item{frequency}{Total number of donations}
#'   \item{total}{Total blood donated in c.c.}
#'   \item{time}{Months since first donation}
#'   \item{donation.crit}{Did he/she donated blood in March 2007?}
#'
#'   ...
#' }
#'
#' @family datasets
#'
#' @source https://archive.ics.uci.edu/ml/datasets/Blood+Transfusion+Service+Center

"blood"



#  2. breastcancer: ------

#' Physiological data of patients tested for breast cancer
#'
#' @format A data frame containing 699 patients (rows) and 9 variables (columns).
#'
#' \describe{
#'   \item{thickness}{Clump Thickness}
#'   \item{cellsize.unif}{Uniformity of Cell Size}
#'   \item{cellshape.unif}{Uniformity of Cell Shape}
#'   \item{adhesion}{Marginal Adhesion}
#'   \item{epithelial}{Single Epithelial Cell Size}
#'   \item{nuclei.bare}{Bare Nuclei}
#'   \item{chromatin}{Bland Chromatin}
#'   \item{nucleoli}{Normal Nucleoli}
#'   \item{mitoses}{Mitoses}
#'   \item{diagnosis}{Is cancer present? TRUE or FALSE}
#'
#'   ...
#' }
#'
#' @family datasets
#'
#' @source https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)

"breastcancer"



#  3. car: ------

#' Car acceptability data
#'
#' A dataset on car evaluations based on basic features, derived from a simple hierarchical decision model.
#'
#' The (yet to be binarized) criterion variable is a car's \code{acceptability} rating.
#'
#' @format A data frame containing 1728 cars (rows) and 7 variables (columns).
#'
#' \describe{
#'   \item{buying.price}{Numeric}
#'   \item{maint.price}{Factor}
#'   \item{doors}{Factor}
#'   \item{persons}{Numeric}
#'   \item{luggage}{Numeric}
#'   \item{safety}{Factor}
#'   \item{acceptability}{Factor}
#'
#'   ...
#' }
#'
#' @family datasets
#'
#' @references
#' Bohanec, M., Rajkovic, V. (1990): Expert system for decision making. Sistemica 1 (1), pp. 145-157.
#'
#' @source \url{http://archive.ics.uci.edu/ml/datasets/Car+Evaluation}
#'

"car"



#  4. contraceptive: ------

#' Contraceptive use data
#'
#' A subset of the 1987 National Indonesia Contraceptive Prevalence Survey.
#'
#' The samples describe married women who were either not pregnant or do not know if they were at the time of interview.
#'
#' The problem consists in predicting a woman's current contraceptive method choice (here: binarized \code{cont.crit})
#' based on her demographic and socio-economic characteristics.
#'
#' @format A data frame containing 1473 rows and 10 columns.
#'
#' \describe{
#'   \item{wife.age}{Numeric}
#'   \item{wife.edu}{Factor}
#'   \item{hus.ed}{Factor}
#'   \item{children}{Numeric}
#'   \item{wife.rel}{Numeric}
#'   \item{wife.work}{Factor}
#'   \item{hus.occ}{Factor}
#'   \item{sol}{Factor}
#'   \item{media}{Numeric}
#'   \item{cont.crit}{numeric}
#'
#'   ...
#' }
#'
#' @family datasets
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Contraceptive+Method+Choice}
#'

"contraceptive"



#  5. creditapproval: ------

#' Credit approval data
#'
#' @format A data frame containing 690 rows and 15 columns
#'
#' @family datasets
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Credit+Approval}

"creditapproval"



#  6. fertility: ------

#' Fertility data
#'
#' This dataset describes a sample of 100 volunteers providing a semen sample that was analyzed according to the WHO 2010 criteria.
#'
#' Sperm concentration are related to socio-demographic data, environmental factors, health status, and life habits.
#'
#' The binary criterion variable is \code{diagnosis}: Normal (N) vs. altered (O).
#'
#' @format A data frame containing 100 rows and 10 columns.
#'
#' @family datasets
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Fertility}
#'

"fertility"



#  7. forestfires: ------

#' Forest fires data
#'
#' A dataset of forest fire statistics.
#'
#' @format A data frame containing 517 rows and 13 columns.
#'
#' \describe{
#'   \item{X}{Integer -x-axis spatial coordinate within the Montesinho park map: 1 to 9}
#'   \item{Y}{Integer - y-axis spatial coordinate within the Montesinho park map: 2 to 9}
#'   \item{month}{Factor - month of the year: "jan" to "dec" }
#'   \item{day}{Factor -day of the week: "mon" to "sun"}
#'   \item{FFMC}{Numeric -FFMC index from the FWI system: 18.7 to 96.20}
#'   \item{DMC}{Numeric - DMC index from the FWI system: 1.1 to 291.3 }
#'   \item{DC}{Numeric - DC index from the FWI system: 7.9 to 860.6 }
#'   \item{ISI}{Numeric - ISI index from the FWI system: 0.0 to 56.10}
#'   \item{temp}{Numeric - temperature in Celsius degrees: 2.2 to 33.30}
#'   \item{RH}{Numeric - relative humidity in percent: 15.0 to 100 }
#'   \item{wind}{Numeric - wind speed in km/h: 0.40 to 9.40 }
#'   \item{rain}{Numeric - outside rain in mm/m2 : 0.0 to 6.4 }
#'   \item{area}{Numeric - the burned area of the forest (in ha): 0.00 to 1090.84 }
#'
#'   ...
#' }
#'
#' @family datasets
#'
#' @source \url{http://archive.ics.uci.edu/ml/datasets/Forest+Fires}

"forestfires"



#  8. heart.cost: ------

#' Cue costs for the heartdisease data
#'
#' This data further characterizes the variables (cues) in the \code{\link{heartdisease}} dataset.
#'
#' @format A data frame containing 153 rows and 14 columns.
#'
#' \describe{
#'   \item{cue}{The name of the cue}
#'   \item{cost}{The cost of the cue}
#'
#'   ...
#' }
#'
#' @family datasets
#'
#' @seealso \code{\link{heartdisease}} dataset.
#'
#' @source \url{https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/costs/}
#'

"heart.cost"



#  9. heartdisease: ------

#' Heart disease data
#'
#' A dataset predicting the \code{diagnosis} of 303 patients tested for heart disease.
#'
#' @format A data frame containing 303 rows and 14 columns, with the following variables:
#'
#' \describe{
#'   \item{diagnosis}{True value of binary criterion: TRUE = Heart disease, FALSE = No Heart disease}
#'   \item{age}{Age (in years)}
#'   \item{sex}{Sex, 1 = male, 0 = female}
#'   \item{cp}{Chest pain type: ta = typical angina, aa = atypical angina, np = non-anginal pain, a = asymptomatic}
#'   \item{trestbps}{Resting blood pressure (in mm Hg on admission to the hospital)}
#'   \item{chol}{Serum cholestoral in mg/dl}
#'   \item{fbs}{Fasting blood sugar > 120 mg/dl: 1 = true, 0 = false }
#'   \item{restecg}{Resting electrocardiographic results. "normal" = normal, "abnormal" = having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), "hypertrophy" = showing probable or definite left ventricular hypertrophy by Estes' criteria.}
#'   \item{thalach}{Maximum heart rate achieved}
#'   \item{exang}{Exercise induced angina: 1 = yes, 0 = no}
#'   \item{oldpeak}{ST depression induced by exercise relative to rest }
#'   \item{slope}{The slope of the peak exercise ST segment. }
#'   \item{ca}{Number of major vessels (0-3) colored by flourosopy }
#'   \item{thal}{"normal" = normal, "fd" = fixed defect, "rd" = reversible defect}
#'
#'   ...
#' }
#'
#' @family datasets
#'
#' @seealso \code{\link{heart.cost}} dataset for cost information.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Heart+Disease}

"heartdisease"



# 10. heart.test: ------

#' Heart disease testing data
#'
#' Testing data for a \code{\link{heartdisease}} data.
#' This subset is used to test the prediction performance of a model trained on the \code{\link{heart.train}} data.
#' The dataset \code{\link{heartdisease}} contains both datasets.
#'
#' @format A data frame containing 153 rows and 14 columns (see \code{\link{heartdisease}} for details).
#'
#' @family datasets
#'
#' @seealso \code{\link{heartdisease}} dataset.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Heart+Disease}

"heart.test"



# 11. heart.train: ------

#' Heart disease training data
#'
#' Training data for a binary prediction model (here: FFT) on (a subset of) the \code{\link{heartdisease}} data.
#' The complementary subset for model testing is \code{\link{heart.test}}.
#' The data in \code{\link{heartdisease}} contains both subsets.
#'
#' @format A data frame containing 150 rows and 14 columns (see \code{\link{heartdisease}} for details).
#'
#' @family datasets
#'
#' @seealso \code{\link{heartdisease}} dataset.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Heart+Disease}
#'

"heart.train"



# 12. iris.v: ------

#' Iris data
#'
#' A famous dataset from R.A. Fisher (1936) simplified to predict only the virginica class (i.e., as a binary classification problem).
#'
#' @format A data frame containing 150 rows and 4 columns.
#'
#' @family datasets
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Iris}
#'
#' @references
#' Fisher, R.A. (1936): The use of multiple measurements in taxonomic problems.
#' Annual Eugenics, 7, Part II, pp. 179--188.

"iris.v"



# 13. mushrooms: ------

#' Mushrooms data
#'
#' Data describing poisonous vs. non-poisonous mushrooms.
#'
#' This dataset includes descriptions of hypothetical samples corresponding to 23 species of gilled mushrooms
#' in the Agaricus and Lepiota Family. Each species is classified as \code{poisonous} (True or False).
#' The Guide clearly states that there is no simple rule for determining the edibility of a mushroom;
#' no rule like ``leaflets three, let it be'' for Poisonous Oak and Ivy.
#'
#' @format A data frame containing 8,124 rows and 23 columns.
#'
#' See \url{http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.names} for column descriptions.
#'
#' \describe{
#'  \item{poisonous}{logical criterion variable}
#'  \item{cshape}{character}
#'  \item{csurface}{character}
#'  \item{ccolor}{character}
#'  \item{bruises}{character}
#'  \item{odor}{numeric}
#'  \item{gattach}{character}
#'  \item{gspace}{characte}
#'  \item{gsize}{character}
#'  \item{gcolor}{character}
#'  \item{sshape}{character}
#'  \item{sroot}{character}
#'  \item{ssaring}{character}
#'  \item{ssbring}{character}
#'  \item{scaring}{character}
#'  \item{scbring}{character}
#'  \item{vtype}{character}
#'  \item{vcolor}{character}
#'  \item{ringnum}{character}
#'  \item{ringtype}{character}
#'  \item{sporepc}{character}
#'  \item{population}{character}
#'  \item{habitat}{character}
#'
#'   ...
#' }
#'
#' @family datasets
#'
#' @references
#' Mushroom records drawn from The Audubon Society Field Guide to North American Mushrooms (1981).
#' G.H. Lincoff (Pres.), New York: A.A. Knopf.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Mushroom}
#'

"mushrooms"



# 14. sonar: ------

#' Sonar data
#'
#' @format A data frame containing 208 rows and 60 columns.
#'
#' @family datasets
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+(Sonar,+Mines+vs.+Rocks)}

"sonar"



# 15. titanic: ------

#' Titanic survival data
#'
#' Data indicating who survived on the Titanic.
#'
#' See \code{\link{Titanic}} of the R \strong{datasets} package for details and
#' the same data (in a 4-dimensional \code{table}).
#'
#' @format A data frame containing 2,201 rows and 4 columns.
#'
#' \describe{
#'   \item{class}{Factor - Class (first, second, third, or crew)}
#'   \item{age}{Factor - Age group (child or adult)}
#'   \item{sex}{Factor - Sex (male or female)}
#'   \item{survived}{Factor - Whether the passenger survived (1) or not (0)}
#'
#'   ...
#' }
#'
#' @family datasets
#'
#' @references
#' Dawson, Robert J. MacG. (1995), The ‘Unusual Episode’ Data Revisited. Journal of Statistics Education, 3.
#' doi: 10.1080/10691898.1995.11910499.
#'
#' @source \url{https://www.encyclopedia-titanica.org}

"titanic"



# 16. voting: ------

#' Voting data
#'
#' A dataset of votes for each of the U.S. House of Representatives Congressmen on the 16 key votes identified by the CQA.
#'
#' The CQA lists nine different types of votes: voted for, paired for, and announced for (these three simplified to yea),
#' voted against, paired against, and announced against (these three simplified to nay),
#' voted present, voted present to avoid conflict of interest, and did not vote or otherwise make a position known
#' (these three simplified to an unknown disposition).
#'
#' The binary criterion variable used here is \code{party.crit}.
#'
#' @format A data frame containing 435 rows and 16 columns.
#'
#' @family datasets
#'
#' @references
#' Congressional Quarterly Almanac, 98th Congress, 2nd session 1984, Volume XL: Congressional Quarterly Inc.
#' Washington, D.C., 1985.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Congressional+Voting+Records}
#'

"voting"



# 17. wine: -------

#' Wine tasting data
#'
#' Chemical and tasting data from wines in North Portugal.
#'
#' @format A data frame containing 6497 rows and 13 columns.
#'
#' @family datasets
#'
#' @source \url{http://archive.ics.uci.edu/ml/datasets/Wine+Quality}

"wine"




# ToDo: ------

# etc.

# eof.
