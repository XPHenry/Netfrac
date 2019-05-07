\name{Netfrac}
\alias{Netfrac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sequence similarity network distance analysis
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Uses dist_paths function to calculate distances between different species communities in a sequence similarity network. It takes an igraph object as input and returns distance matrices.
}
\usage{
Netfrac(x, distances = "Unifrac", paths = "single", maxcores = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The graph(tree) that will be used. The graph must be in the igraph format, the edges being accessible with E(x) and vertices with V(x). The communities are under V(x)$tax and the weight under E(x)$weight. Those are functions applicable with igraph objects only
%%     ~~Describe \code{x} here~~
}
  \item{distances}{Distance(s) to calculate: transfer, paths, Unifrac and Motifs. The transfer, paths and Unifrac distances are also applicable for trees.
%%     ~~Describe \code{distances} here~~
}
  \item{paths}{deciding whether all the shortest paths should be calculated ("all"), or only one ("single"), which can be faster depending on the graph size.
%%     ~~Describe \code{paths} here~~
}
  \item{maxcores}{uses the multicore function to decide the number of cores to use
%%     ~~Describe \code{maxcores} here~~
}
}
\details{Transfer distance is based on the number of paths that are not unique to one community. Paths distances are different shortest path distances that differ in the measure used to make the calculation. SSNUnifrac is based on the Unifrac distance presented by Lozupone (2005), and Motifs on the motifs count in a graph.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
net_a = load_network("exemples/network_a.txt","exemples/network_a_tax.txt")
Netfrac(net_a)
Netfrac(net_a,"paths","all")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
