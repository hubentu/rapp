% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BuildApp.R
\name{renderUI}
\alias{renderUI}
\title{Build web UI}
\usage{
renderUI(
  ui,
  uid,
  Rfun = list(),
  dataList = list(),
  methodList = list(),
  outdir,
  vuejs = "lib/app.js",
  opencpu = TRUE,
  outType = list("plot"),
  outID = list("plotOut")
)
}
\arguments{
\item{inputPanel}{input menu tag list.}

\item{outputPanel}{output tag list.}
}
\description{
Build web UI
}
