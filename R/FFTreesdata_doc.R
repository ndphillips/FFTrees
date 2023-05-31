# FFTreesdata_doc.R
# Documenting all datasets.
# -------------------------


#  1. blood: ------

#' Blood donation data
#'
#' Data taken from the Blood Transfusion Service Center in Hsin-Chu City in Taiwan
#'
#' @format A data frame containing 748 rows and 5 columns.
#'
#' \describe{
#'   \item{recency}{Months since last donation}
#'   \item{frequency}{Total number of donations}
#'   \item{total}{Total blood donated (in c.c.)}
#'   \item{time}{Months since first donation}
#'   \item{donation.crit}{\emph{Criterion}: Did the person donate blood (in March 2007)?
#'
#'   Values: \code{0}/no vs. \code{1}/yes (76.2\% vs.\ 23.8\%).}
#' }
#'
#' @family datasets
#'
#' @source  \url{https://archive.ics.uci.edu/ml/datasets/Blood+Transfusion+Service+Center}
#'
#'
#'
#' Original owner and donor:
#'
#' Prof. I-Cheng Yeh
#'
#' Department of Information Management
#'
#' Chung-Hua University
#'

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
#'   \item{diagnosis}{\emph{Criterion}: Absence/presence of breast cancer.
#'
#'   Values: \code{FALSE} vs. \code{TRUE} (65.0\% vs.\ 35.0\%).}
#' }
#'
#' @family datasets
#'
#'
#'
#' @details
#'
#' We made the following enhancements to the original data for improved usability:
#'
#' \itemize{
#'  \item{The ID number of the cases was excluded.}
#'  \item{The numeric criterion with value "2" for benign and "4" for malignant was converted to logical TRUE/FALSE.}
#'  \item{16 cases were excluded because they contained NAs.}
#'  }
#'
#'  Other than that, the data remains consistent with the original dataset.
#'
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)}
#'
#' Original creator:
#'
#' Dr. William H. Wolberg (physician)
#'
#' University of Wisconsin Hospitals
#'
#' Madison, Wisconsin, USA
#'
#'
#'

"breastcancer"



#  3. car: ------

#' Car acceptability data
#'
#' A dataset on car evaluations based on basic features, derived from a simple hierarchical decision model.
#'
#' The criterion variable is a car's \code{acceptability} rating.
#'
#' @format A data frame containing 1728 cars (rows) and 7 variables (columns).
#'
#' \describe{
#'   \item{buying.price}{price for buying the car, Factor (high, low, med, vhigh)}
#'   \item{maint.price}{price of the maintenance, Factor (high, low, med, vhigh)}
#'   \item{doors}{number of doors, Factor (2, 3, 4, 5more)}
#'   \item{persons}{capacity in terms of persons to carry, Factor (2, 4, more)}
#'   \item{luggage}{the size of luggage boot, Factor (big, med, small)}
#'   \item{safety}{estimated safety of the car, Factor (high, low, med)}
#'   \item{acceptability}{\emph{Criterion}: Category of acceptability rating.
#'
#'   Values: \code{unacc}/ \code{vgood}/ \code{good}/ \code{acc}}
#' }
#'
#' @family datasets
#'
#'
#'@details
#' The \emph{criterion} for this dataset has not yet been binarized. Before using it with an \emph{FFTree}, this necessary prerequisite step should be completed based on individual preferences.
#'
#' @source \url{http://archive.ics.uci.edu/ml/datasets/Car+Evaluation}
#'
#' Original creator and donor:
#'
#' Marko Bohanec and Blaz Zupan
#'
#' @references
#' Bohanec, M., Rajkovic, V. (1990): Expert system for decision making.
#' \emph{Sistemica}, \emph{1} (1), 145--157.
#'
#'

"car"



#  4. contraceptive: ------

#' Contraceptive use data
#'
#' A subset of the 1987 National Indonesia Contraceptive Prevalence Survey.
#'
#' The samples describe married women who were either not pregnant
#' or do not know if they were pregnant at the time of the interview.
#'
#' The problem consists in predicting a woman's current contraceptive method choice
#' (here: binarized \code{cont.crit})
#' based on her demographic and socio-economic characteristics.
#'
#' @format A data frame containing 1473 cases (rows) and 10 variables (columns).
#'
#' \describe{
#'   \item{wife.age}{Wife's age, Numeric}
#'   \item{wife.edu}{Wife's education, Nummeric, (1=low, 2, 3, 4=high)}
#'   \item{hus.ed}{Husband's education, Nummeric, (1=low, 2, 3, 4=high)}
#'   \item{children}{Number of children ever born, Numeric}
#'   \item{wife.rel}{Wife's religion, Numeric, (0=Non-Islam, 1=Islam)}
#'   \item{wife.work}{Wife's now working?, Nummeric, (0=Yes, 1=No)}
#'   \item{hus.occ}{Husband's occupation, Nummeric, (1, 2, 3, 4)}
#'   \item{sol}{Standard-of-living index, Nummeric, (1=low, 2, 3, 4=high)}
#'   \item{media}{Media exposure, Numeric, (0=Good, 1=Not good)}
#'   \item{cont.crit}{\emph{Criterion}: Use of a contraceptive (as logical).
#'
#'   Values: \code{FALSE} vs. \code{TRUE} (42.7\% vs. 57.3\%).}
#' }
#'
#' @family datasets
#'
#'@details
#'We made the following enhancements to the original data for improved usability:
#'
#'\itemize{
#'  \item{The criterion was binarized from a class attribute variable with three levels (\code{1=No-use}, \code{2=Long-term}, \code{3=Short-term}) , into a logical variable with two levels (\code{TRUE} vs. \code{FALSE}).}
#'  }
#'
#'  Other than that, the data remains consistent with the original dataset.
#'
#'
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Contraceptive+Method+Choice}
#'
#' Original creator and donor:
#'
#' Tjen-Sien Lim
#'

"contraceptive"


#  5. creditapproval: ------

#' Credit approval data
#'
#' This data reports predictors and the result of credit card applications.
#' Its attribute names and values have been changed to symbols to protect confidentiality.
#'
#' This dataset contains a mix of attributes -- continuous, nominal with small Ns,
#' and nominal with larger Ns. There are also a few missing values.
#'
#' @format A data frame containing 690 cases (rows) and 15 variables (columns).
#'
#' \describe{
#'  \item{c.1}{categorical: b, a}
#'  \item{c.2}{continuous}
#'  \item{c.3}{continuous}
#'  \item{c.4}{categorical: u, y, l, t}
#'  \item{c.5}{categorical: g, p, gg}
#'  \item{c.6}{categorical: c, d, cc, i, j, k, m, r, q, w, x, e, aa, ff}
#'  \item{c.7}{categorical: v, h, bb, j, n, z, dd, ff, o}
#'  \item{c.8}{continuous}
#'  \item{c.9}{categorical: t, f}
#'  \item{c.10}{categorical: t, f}
#'  \item{c.11}{continuous}
#'  \item{c.12}{categorical: t, f}
#'  \item{c.13}{categorical: g, p, s}
#'  \item{c.14}{continuous}
#'  \item{c.15}{continuous}
#'  \item{crit}{\emph{Criterion}: Credit approval.
#'
#'  Values: \code{TRUE} (+)  vs. \code{FALSE} (-) (44.5\% vs. 55.5\%).}
#'  }
#'
#' @family datasets
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Credit+Approval}
#'
#' @details
#'  We made the following enhancements to the original data for improved usability:
#'
#'\itemize{
#'  \item{Any missing values, denoted as "?" in the dataset, were transformed into NAs.}
#'  \item{Binary factor variables with exclusive "t" and "f" values were converted to logical TRUE/FALSE vectors.}
#'  }
#'
#'  Other than that, the data remains consistent with the original dataset.
#'

"creditapproval"


#  6. fertility: ------

#' Fertility data
#'
#' This dataset describes a sample of 100 volunteers providing a semen sample that was analyzed according to the WHO 2010 criteria.
#'
#' Sperm concentration are related to socio-demographic data, environmental factors, health status, and life habits.
#'
#' @format A data frame containing 100 rows and 10 columns.
#'
#'
#' \describe{
#'   \item{season}{Season in which the analysis was performed. (winter, spring, summer, fall)}
#'   \item{age}{Age at the time of analysis}
#'   \item{child.dis}{Childish diseases (ie , chicken pox, measles, mumps, polio) (yes(1), no(0)) }
#'   \item{trauma}{Accident or serious trauma (yes(1), no(0))}
#'   \item{surgery}{Surgical intervention (yes(1), no(0))}
#'   \item{fevers}{High fevers in the last year (less than three months ago(-1), more than three months ago (0), no. (1))}
#'   \item{alcohol}{Frequency of alcohol consumption (several times a day, every day, several times a week, once a week, hardly ever or never)}
#'   \item{smoking}{Smoking habit (never(-1), occasional (0)) daily (1))}
#'   \item{sitting}{Number of hours spent sitting per day}
#'   \item{diagnosis}{\emph{Criterion}: Diagnosis normal (TRUE) vs. altered (FALSE) (88.0\% vs.\ 22.0\%).}
#' }
#'
#' @family datasets
#'
#' @details We made the following enhancements to the original data for improved usability:
#'
#'\itemize{
#'
#'  \item{The criterion was redefined from a factor variable with two levels
#'  (\code{N=Normal}, \code{O=Altered}) into a logical variable (\code{TRUE} vs. \code{FALSE}).}
#'
#'  }
#'
#'  Other than that, the data remains consistent with the original dataset.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Fertility}
#'
#' Original contributors:
#'
#' David Gil
#' Lucentia Research Group
#' Department of Computer Technology
#' University of Alicante
#'
#' Jose Luis Girela
#' Department of Biotechnology
#' University of Alicante
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
#'   \item{fire.crit}{\emph{Criterion}: Was there a fire (greater than 1.00 ha)?
#'
#'   Values: \code{TRUE} (yes) vs. \code{FALSE} (no) (47.0\% vs. 53.0\%).}
#'
#' }
#'
#' @family datasets
#'
#'
#' @details
#'  We made the following enhancements to the original data for improved usability:
#'
#'\itemize{
#'  \item{The criterion was redefined from a numeric variable that indicated the number of hectares that burned in a fire into a logical variable (\code{TRUE} (for values >1) vs. \code{FALSE} (for values <=1)).}
#'  }
#'
#'  Other than that, the data remains consistent with the original dataset.
#'
#' @source \url{http://archive.ics.uci.edu/ml/datasets/Forest+Fires}
#'
#' Original creator:
#' Prof. Paulo Cortez and Aníbal Morais
#' Department of Information Systems
#' University of Minho, Portugal
#'

"forestfires"



#  8. heart.cost: ------

#' Cue costs for the heartdisease data
#'
#' This data further characterizes the variables (cues) in the \code{\link{heartdisease}} dataset.
#'
#' @format A list of length 13 containing the cost of each cue in the \code{\link{heartdisease}} dataset (in dollars).
#' Each list element is a single (positive numeric) value.
#'
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
#'
#' }
#'
#' @family datasets
#'
#' @seealso \code{\link{heart.cost}} dataset for cost information.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Heart+Disease}
#'

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
#'

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
#' \describe{
#'   \item{sep.len}{sepal length in cm}
#'   \item{sep.wid}{sepal width in cm}
#'   \item{pet.len}{petal length in cm}
#'   \item{pet.wid}{petal width in cm}
#'   \item{virginica}{\emph{Criterion}: Does an iris belong to the class "virginica"?
#'
#'   Values: \code{TRUE} vs. \code{FALSE} (33.33\% vs.66.67\%).}
#' }
#'
#' @family datasets
#'
#' @details To improve usability, we made the following changes:
#'
#'\itemize{
#'
#'  \item{The criterion was binarized from a factor variable with three levels
#'  (\code{Iris-setosa}, \code{Iris-versicolor}, \code{Iris-virginica}),
#'  into a logical variable (i.e., \code{TRUE} for all instances of \code{Iris-virginica}
#'  and \code{FALSE} for the two other levels).}
#'
#'  }
#'
#'  Other than that, the data remains consistent with the original dataset.
#'
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Iris}
#'
#' @references
#' Fisher, R.A. (1936): The use of multiple measurements in taxonomic problems.
#' Annual Eugenics, 7, Part II, pp. 179--188.
#'

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
#'  \describe{
#'  \item{poisonous}{\emph{Criterion}: Is the mushroom poisonous?
#'
#'   Values: \code{TRUE} (poisonous) vs. \code{FALSE} (eatable) (48.2\% vs.\ 52.8\%).}
#'  \item{cshape}{cap-shape, character (bell=b, conical=c, convex=x, flat=f, knobbed=k, sunken=s)}
#'  \item{csurface}{cap-surface, character (fibrous=f, grooves=g, scaly=y, smooth=s)}
#'  \item{ccolor}{cap-color, character (brown=n, buff=b, cinnamon=c, gray=g, green=r, pink=p, purple=u, red=e, white=w, yellow=y)}
#'  \item{bruises}{Are there bruises? logical (TRUE/FALSE)}
#'  \item{odor}{character (almond=a, anise=l, creosote=c, fishy=y, foul=f, musty=m, none=n, pungent=p, spicy=s) }
#'  \item{gattach}{gill-attachment, character (attached=a, descending=d, free=f, notched=n)}
#'  \item{gspace}{gill-spacing, character (close=c, crowded=w, distant=d)}
#'  \item{gsize}{gill-size, character (broad=b, narrow=n)}
#'  \item{gcolor}{gill-color, character (black=k, brown=n, buff=b, chocolate=h, gray=g, green=r, orange=o, pink=p, purple=u, red=e, white=w, yellow=y)}
#'  \item{sshape}{stalk-shape, character (enlarging=e, tapering=t)}
#'  \item{sroot}{stalk-root, character (bulbous=b ,club=c, cup=u, equal=e, rhizomorphs=z, rooted=r)}
#'  \item{ssaring}{stalk-surface-above-ring, character (fibrous=f, scaly=y, silky=k, smooth=s)}
#'  \item{ssbring}{stalk-surface-below-ring, character (fibrous=f, scaly=y, silky=k, smooth=s)}
#'  \item{scaring}{stalk-color-above-ring, character (brown=n, buff=b, cinnamon=c, gray=g, orange=o, pink=p, red=e, white=w, yellow=y)}
#'  \item{scbring}{stalk-color-below-ring, character (brown=n, buff=b, cinnamon=c, gray=g, orange=o, pink=p, red=e, white=w, yellow=y)}
#'  \item{vtype}{veil-type, character (partial=p, universal=u)}
#'  \item{vcolor}{veil-color, character (brown=n, orange=o, white=w, yellow=y)}
#'  \item{ringnum}{character (none=n, one=o, two=t)}
#'  \item{ringtype}{character (cobwebby=c, evanescent=e, flaring=f, large=l, none=n, pendant=p, sheathing=s, zone=z)}
#'  \item{sporepc}{spore-print-color, character (black=k, brown=n, buff=b, chocolate=h, green=r, orange=o, purple=u, white=w, yellow=y)}
#'  \item{population}{character(abundant=a, clustered=c, numerous=n, scattered=s, several=v, solitary=y)}
#'  \item{habitat}{character (grasses=g, leaves=l, meadows=m, paths=p, urban=u, waste=w, woods=d)}
#'
#' }
#'
#' @family datasets
#'
#' @details
#' We made the following enhancements to the original data for improved usability:
#'
#'\itemize{
#'
#'  \item{Any missing values, denoted as "?" in the dataset, were transformed into NAs.}
#'
#'  \item{Binary factor variables with exclusive "t" and "f" values were converted to logical \code{TRUE/FALSE} vectors.}
#'
#'  \item{The binary factor \emph{criterion} variable with exclusive "p" and "e" values was converted to a logical \code{TRUE/FALSE} vector.}
#'
#'  }
#'
#'  Other than that, the data remains consistent with the original dataset.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Mushroom}
#'
#' @references
#' Mushroom records drawn from The Audubon Society Field Guide to North American Mushrooms (1981).
#' G.H. Lincoff (Pres.), New York: A.A. Knopf.
#'

"mushrooms"



# 14. sonar: ------

#' Sonar data
#'
#'The file contains patterns of sonar signals bounced off a metal cylinder or bounced off a roughly cylindrical rock at various angles and under various conditions.
#'The transmitted sonar signal is a frequency-modulated chirp, rising in frequency.
#'
#' @format A data frame containing 208 rows and 60 columns.
#'
#'
#' \describe{
#'   \item{V1}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V2}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V3}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V4}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V5}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V6}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V7}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V8}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V9}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V10}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V11}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V12}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V13}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V14}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V15}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V16}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V17}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V18}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V19}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V20}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V21}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V22}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V23}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V24}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V25}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V26}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V27}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V28}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V29}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V30}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V31}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V32}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V33}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V34}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V35}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V36}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V37}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V38}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V39}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V40}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V41}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V42}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V43}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V44}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V45}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V46}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V47}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V48}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V49}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V50}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V51}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V52}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V53}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V54}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V55}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V56}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V57}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V58}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V59}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{V60}{Number in the range 0.0 to 1.0 that represents the energy within a particular frequency band, integrated over a certain period of time.}
#'   \item{mine.crit}{\emph{Criterion}: Did a sonar signal bounce off a metal cylinder (or a rock)?
#'
#'   Values: \code{TRUE} (metal cylinder) vs. \code{FALSE} (rock) (53.37\% vs.\ 46.63\%).}
#' }
#'
#'
#' @family datasets
#'
#' @details We made the following enhancements to the original data for improved usability:
#'
#'\itemize{
#'  \item{The binary factor \emph{criterion} variable with exclusive "m" and "r" values was converted to a logical \code{TRUE/FALSE} vector.}
#'  }
#'
#'  Other than that, the data remains consistent with the original dataset.
#'
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+(Sonar,+Mines+vs.+Rocks)}
#'
#' @references
#'  Gorman, R. P., and Sejnowski, T. J. (1988). "Analysis of Hidden Units in a Layered Network Trained to Classify Sonar Targets" in Neural Networks, Vol. 1, pp. 75-89.
#'

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
#'   \item{survived}{Logical - Whether the passenger survived (TRUE) or not (FALSE)}
#'
#' }
#'
#' @family datasets
#'
#'
#' @references
#' Dawson, Robert J. MacG. (1995), The ‘Unusual Episode’ Data Revisited. Journal of Statistics Education, 3.
#' doi: 10.1080/10691898.1995.11910499.
#'
#' @source \url{https://www.encyclopedia-titanica.org}
#'

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
#' @format A data frame containing 435 rows and 16 columns.
#'
#'  \describe{
#'   \item{handicapped}{handicapped-infants, logical (TRUE, FALSE)}
#'   \item{water}{water-project-cost-sharing, logical (TRUE, FALSE)}
#'   \item{adoption}{adoption-of-the-budget-resolution, logical (TRUE, FALSE)}
#'   \item{physician}{physician-fee-freeze, logical (TRUE, FALSE)}
#'   \item{elsalvador}{el-salvador-aid, logical (TRUE, FALSE)}
#'   \item{religionschool}{religious-groups-in-schools, logical (TRUE, FALSE)}
#'   \item{satellite}{anti-satellite-test-ban, logical (TRUE, FALSE)}
#'   \item{nicaraguan}{aid-to-nicaraguan-contras, logical (TRUE, FALSE)}
#'   \item{mxmissile}{mxmissile, logical (TRUE, FALSE)}
#'   \item{immigration}{immigration, logical (TRUE, FALSE)}
#'   \item{synfuels}{synfuels-corporation-cutback, logical (TRUE, FALSE)}
#'   \item{education}{education-spending, logical (TRUE, FALSE)}
#'   \item{superfund}{superfund-right-to-sue, logical (TRUE, FALSE)}
#'   \item{crime}{crime, logical (TRUE, FALSE)}
#'   \item{dutyfree}{duty-free-exports, logical (TRUE, FALSE)}
#'   \item{southafrica}{export-administration-act-south-africa, logical (TRUE, FALSE)}
#'   \item{party.crit}{\emph{Criterion}: Where the voters democratic (or republican) congressmen?
#'
#'   Values: \code{TRUE} (democrat) / \code{FALSE} (republican) (61.52\% vs. 38.48\%).}
#' }
#'
#' @family datasets
#'
#' @details We made the following enhancements to the original data for improved usability:
#'
#'\itemize{
#'  \item{Any missing values, denoted as "?" in the dataset, were transformed into NAs.}
#'  \item{Binary factor variables with exclusive "y" and "n" values were converted to logical TRUE/FALSE vectors.}
#'  \item{The binary character \emph{criterion} variable with exclusive "democrat" and "republican" values was converted to a logical \code{TRUE/FALSE} vector.}
#'  }
#'
#'  Other than that, the data remains consistent with the original dataset.
#'
#'
#' @references
#' Congressional Quarterly Almanac, 98th Congress, 2nd session 1984,
#' Volume XL: Congressional Quarterly Inc.
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
#'  \describe{
#'   \item{fixed.acidity}{fixed acidity (nummeric)}
#'   \item{volatile.acidity}{volatile acidity (nummeric)}
#'   \item{citric.acid}{citric acid (nummeric)}
#'   \item{residual.sugar}{residual sugar (nummeric)}
#'   \item{chlorides}{chlorides (nummeric)}
#'   \item{free.sulfur.dioxide}{free sulfur dioxide (nummeric)}
#'   \item{total.sulfur.dioxide}{total sulfur dioxide (nummeric)}
#'   \item{density}{density (nummeric)}
#'   \item{pH}{PH Value (nummeric)}
#'   \item{sulphates}{Sulphates (nummeric)}
#'   \item{alcohol}{Alcohol (nummeric)}
#'   \item{quality}{Quality (nummeric, score between 0 and 10)}
#'   \item{type}{\emph{Criterion}: Is the wine \code{red} or \code{white}? (24.61\% vs.75.39\%)}
#' }
#'
#'
#' @family datasets
#'
#' @source \url{http://archive.ics.uci.edu/ml/datasets/Wine+Quality}
#'
#' @references
#'  P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis.
#'  Modeling wine preferences by data mining from physicochemical properties.
#'  In Decision Support Systems, \emph{Elsevier}, 47(4):547-553. ISSN: 0167-9236.
#'

"wine"




# ToDo: ------

# etc.

# eof.
