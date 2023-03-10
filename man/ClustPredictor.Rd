% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClustPredictor.R
\name{ClustPredictor}
\alias{ClustPredictor}
\title{Clustering Predictor Object}
\description{
A \code{ClustPredictor} object holds any unsupervised clustering algorithm
and the data to be used for analyzing the model. The interpretation methods
in the \code{FACT} package need the clustering algorithm to be wrapped in a
\code{ClustPredictor} object.
}
\details{
A Cluster Predictor object is a container for the unsupervised prediction
model and the data.
This ensures that the clustering algorithm can be analyzed in a robust way.
The Model inherits from \link[iml:Predictor]{iml::Predictor} Object and adjusts this Object to
contain unsupervised Methods.
}
\examples{
require(factoextra)
require(FuzzyDBScan)
multishapes <- as.data.frame(multishapes[, 1:2])
eps = c(0, 0.2)
pts = c(3, 15)
res <- FuzzyDBScan$new(multishapes, eps, pts)
res$plot("x", "y")
# create hard label predictor
predict_part = function(model, newdata) model$predict(new_data = newdata, cmatrix = FALSE)$cluster
ClustPredictor$new(res, as.data.frame(multishapes), y = res$clusters,
                       predict.function = predict_part, type = "partition")
# create soft label predictor
predict_prob = function(model, newdata) model$predict(new_data = newdata)
ClustPredictor$new(res, as.data.frame(multishapes), y = res$results,
                               predict.function = predict_prob, type = "prob")
}
\section{Super class}{
\code{\link[iml:Predictor]{iml::Predictor}} -> \code{ClustPredictor}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{type}}{\code{character(1)}\cr
Either partition for cluster assignments or prob
for soft labels. Can be decided by chosen by the
user when initializing the object. If \code{NULL},
it checks the the dimensions of \code{y}.}

\item{\code{cnames}}{\code{character}\cr
Is \code{NULL}, if hard labeling is used. If soft
labels are used, column names of \code{y} are being
transferred.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ClustPredictor-new}{\code{ClustPredictor$new()}}
\item \href{#method-ClustPredictor-clone}{\code{ClustPredictor$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="iml" data-topic="Predictor" data-id="predict"><a href='../../iml/html/Predictor.html#method-Predictor-predict'><code>iml::Predictor$predict()</code></a></span></li>
<li><span class="pkg-link" data-pkg="iml" data-topic="Predictor" data-id="print"><a href='../../iml/html/Predictor.html#method-Predictor-print'><code>iml::Predictor$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClustPredictor-new"></a>}}
\if{latex}{\out{\hypertarget{method-ClustPredictor-new}{}}}
\subsection{Method \code{new()}}{
Create a ClustPredictor object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClustPredictor$new(
  model = NULL,
  data = NULL,
  predict.function = NULL,
  y = NULL,
  batch.size = 1000,
  type = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{model}}{any\cr
The trained clustering algorithm. Recommended
are models from \code{mlr3cluster}. For other
clustering algorithms predict functions need to
be specified.}

\item{\code{data}}{\link{data.frame}\cr
The data to be used for analyzing the prediction model. Allowed column
classes are: \link{numeric}, \link{factor}, \link{integer}, \link{ordered} and \link{character}}

\item{\code{predict.function}}{\link{function}\cr
The function to assign newdata. Only needed if
\code{model} is not a model from \code{mlr3cluster}. The
first argument of \code{predict.fun} has to be the
model, the second the \code{newdata}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{function(model, newdata)
}\if{html}{\out{</div>}}}

\item{\code{y}}{any\cr
A \link{integer} vector representing the assigned
clusters or a \link{data.frame} representing the
soft labels per cluster assigned in columns.}

\item{\code{batch.size}}{\code{numeric(1)}\cr
The maximum number of rows to be input the model for prediction at once.
Currently only respected for \link{SMART}.}

\item{\code{type}}{\code{character(1)})\cr
This argument is passed to the prediction
function of the model. For soft label
predictions, use \code{type="prob"}. For hard label
predictions, use \code{type="partition"}. Consult
the documentation or definition of the
clustering algorithm you use to find which type
options you have.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClustPredictor-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ClustPredictor-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClustPredictor$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
