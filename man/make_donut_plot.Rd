% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/donut.R
\name{make_donut_plot}
\alias{make_donut_plot}
\title{Make donut plot}
\usage{
make_donut_plot(data, group, length, loli_ns, k)
}
\arguments{
\item{data}{Input dataframe}

\item{group}{Subgroup column}

\item{length}{Length of each subgroup (portion in the donut plot)}

\item{loli_ns}{Number of loli sticks in each subgroup}

\item{k}{Label text of each group}
}
\value{
A donut plot with loli sticks
}
\description{
Make donut plots combined with lolipop sticks
}
\examples{
sample_df <- data.frame(group=c(1, 2, 3), length=c(2, 1, 9),
                        loli_ns = c(1, 2, 3), k=c(0.5, 0.3, 0.17))
make_donut_plot(sample_df, group = group, length = length, loli_ns = loli_ns, k = k)
}
