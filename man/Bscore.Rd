\name{Bscore}
\alias{Bscore}
\concept{B score method}
\concept{normalization}
\title{B score normalization}
\description{
  Correction of plate and spatial effects of the data \code{xraw} of a \code{cellHTS} object using the B score method.
  Using this method, a two-way median polish is fitted in a per-plate basis to account for row and column effects.
  Optionally, the obtained residuals within each plate can be further divided by their median absolute deviations to standardize for plate-to-plate variability.
  Optionally, a transformation to \emph{z}-scores can be performed.
}
\usage{
Bscore(x, what="xraw", adjustPlateMedian = TRUE, model.log = FALSE, scale = TRUE, save.model = FALSE)
}
\arguments{
  \item{x}{a \code{cellHTS} object that has already been configured. See details.}
  \item{what}{a character indicating the slot of \code{x} to consider: \code{"xraw"} (default) or \code{"xnorm"}.}
  \item{adjustPlateMedian}{a logical value indicating whether the estimated average of each plate should also be substracted to the raw intensity values.}
  \item{model.log}{a logical value. If \code{TRUE}, the per-plate models will be applied after \code{\link{log2}} transformation. However, the outputs of this function will be given in the original scale.}
  \item{scale}{a logical value indicating if the per-plate model residuals should be further scaled by their variance. See details.}
  \item{save.model}{a logical value specifying whether the per-plate models should be saved, and given as output. See details.}
}

\details{
The normalization is performed in a per-plate fashion using the B score method. 
This function can be called direclty, or indirectly, using \code{\link[cellHTS:normalizePlates]{normalizePlates}} or 
\code{\link[cellHTS:normalizeChannels]{normalizeChannels}}. 
In the B score method, the residual \eqn{r_{ijp}} of the measurement for row \emph{i} and column \emph{j} on the \emph{p}-th plate is obtained by fitting a \link[stats:medpolish]{two-way median polish}, in order to account for both row and column effects within the plate:

\deqn{r_{ijp} = y_{ijp} - \hat{y}_{ijp} = y_{ijp} - (\hat{\mu}_p + \hat{R}_{ip} + \hat{C}_{jp})}{%
r_{ijp} = y_{ijp} - yest_{ijp} = y_{ijp} - (mu_p + R_{ip} + C_{jp})}

\eqn{y_{ijp}} is the measurement value in row \emph{i} and column \emph{j} of plate \emph{p} (taken from \code{x$xraw}), and 
\eqn{\hat{y}_{ipj}}{yest_{ijp}} is the corresponding fitted value. 
This is defined as the sum between the estimated average
of the plate (\eqn{\hat{\mu}_p}{mu_p}), the estimated systematic offset for row \emph{i} 
(\eqn{\hat{R}_{ip}}{R_{ip}}), and the systematic offset for column \emph{j} (\eqn{\hat{C}_{jp}}{C_{jp}}).

If \code{scale=TRUE}, for each plate \emph{p}, each of the obtained residual values \eqn{r_{ijp}}'s are divided by the median absolute deviation of the residuals in plate \emph{p} (\eqn{MAD_p}{MAD_p}), giving the B score value:

\deqn{{B_{score}}_{ijp} = \frac{r_{ijp}}{MAD_p}}{%
Bscore_{ijp} = r_{ijp}/MAD_p
}

If \code{adjustPlateMedian} is set to \code{FALSE}, the estimated overall plate average 
(\eqn{\hat{\mu}_p}{mu_p}) is not removed from the intensity values \eqn{y_{ijp}}'s.

If \code{model.log=TRUE}, the model is fitted on \code{\link{log2}} scale, but the output results (normalized values, overall effect, residuals and rowcol.effects) are given in the original scale.

If \code{save.model=TRUE}, the models residuals, row and column offsets and overall offsets are stored in the slots \code{residuals}, \code{rowcol.effects} and \code{overall.effects} of the \code{cellHTS} object \code{x}.
}

\value{
An object of class \code{cellHTS}, which is a copy of the argument
\code{x}, plus an additional slot \code{xnorm} containing the normalized
data. This is an array of the same dimensions as \code{xraw}.
Furthermore, if \code{save.model=TRUE}, the slots \code{residuals}, \code{rowcol.effects}, and \code{overall.effects} (only if \code{adjustPlateMedian} was also set to \code{TRUE}) are added to \code{x}.
The latter slots are arrays with the same dimension as \code{x$xraw}, except the 
\code{overall.effects}, 
which have dimensions \code{1 x nr Plates x nr Replicates x nr Channels}.

Moreover, the processing status of the \code{cellHTS} object is updated
in the slot \code{state} to \code{x$state["normalized"]=TRUE}.
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}}

\seealso{
  \code{\link[stats:medpolish]{medpolish}},
  \code{\link[cellHTS:plotSpatialEffects]{plotSpatialEffects}},
  \code{\link[cellHTS:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS:normalizeChannels]{normalizeChannels}}
}

\references{

Brideau, C., Gunter, B., Pikounis, B. and Liaw, A. (2003) Improved statistical methods for hit selection in high-throughput screening, \emph{J. Biomol. Screen} \bold{8}, 634--647.

Malo, N., Hanley, J.A., Cerquozzi, S., Pelletier, J. and Nadon, R. (2006) Statistical practice in high-throughput screening data analysis, \emph{Nature Biotechn} \bold{24}(2), 167--175. 
}

\examples{
    data(KcViabSmall)
    x <- KcViabSmall
    x <- Bscore(x, save.model = TRUE)
    ## identical result, but calling Bscore function from "normalizePlates"
    xopt <- normalizePlates(x, normalizationMethod="Bscore", BscoreArgs = list(save.model = TRUE))
    all(x$xnorm==xopt$xnorm, na.rm=TRUE)
}
\keyword{manip}
