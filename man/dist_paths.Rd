\name{dist_paths}
\alias{dist_paths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shortest path distances
%%  ~~function to do ... ~~
}
\description{Takes in input the graph as well as the communities to compare to give back the chosen distance. It is better to use the Netfrac function if the global distances are needed.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dist_paths(x, col1, col2, distance = "paths", paths = "single", info = NULL, type = "graph", maxcores = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The igraph object that will be analyzed
%%     ~~Describe \code{x} here~~
}
  \item{col1}{
%%     ~~Describe \code{col1} here~~
}
  \item{col2}{
%%     ~~Describe \code{col2} here~~
}
  \item{distance}{
%%     ~~Describe \code{distance} here~~
}
  \item{paths}{
%%     ~~Describe \code{paths} here~~
}
  \item{info}{
%%     ~~Describe \code{info} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{maxcores}{
%%     ~~Describe \code{maxcores} here~~
}
}
\details{
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

\seealso{\code(\link{dist_mult})
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, col1, col2, distance = "paths", paths = "single",
    info = NULL, type = "graph", maxcores = 1)
{
    cl <- makeCluster(multicore(maxcores))
    registerDoParallel(cl = cl)
    if (type == "graph") {
        if (distance == "paths" || distance == "transfer") {
            res_dist = dist_par(x, col1, col2, distance, paths)
            stopCluster(cl)
            return(unlist(res_dist))
        }
        else if (distance == "Netfrac") {
            NetFrac_2 = get_global_nb_motifs_by_colors(x, 2,
                col1, col2)
            stopCluster(cl)
            return(NetFrac_2 = NetFrac_2$ratio)
        }
        else if (distance == "Unifrac") {
            stopCluster(cl)
            return(SSNUnifrac(x, col1, col2))
        }
    }
    else if (type == "tree") {
        if (!is.null(info)) {
            if (distance == "Unifrac") {
                return("ok")
            }
            else if (distance == "paths") {
                info <- info[match(x$tip.label, info[, 1]), ]
                if (class(info[1, 1]) != "character") {
                  info[1, 1] = as.character(info[1, 1])
                }
                rtips_pos <- which((info[, 2] != col1) & (info[,
                  2] != col2))
                rtips <- as.vector(info[, 1][rtips_pos])
                if (length(rtips) != 0) {
                  x <- drop.tip(x, rtips)
                  info <- info[-rtips_pos, ]
                }
                tips_number <- c(1:length(x$tip.label))
                tips_name <- x$tip.label
                color_t <- as.vector(info[, 2])
                gtree <- graph_from_edgelist(x$edge, directed = FALSE)
                colored_tips <- intersect(V(gtree), which(tips_name \%in\%
                  (info[, 1])))
                V(gtree)[colored_tips]$tax = color_t
                tax <- V(gtree)$tax
                vvector <- as_ids(V(gtree))
                vlistna <- vvector[which(is.na(tax))]
                vlist <- vvector[which(!is.na(tax))]
                vtax <- set_node(x$edge, vvector, vlistna, vlist,
                  tax, col1, col2)
                V(gtree)$tax = vtax
                E(gtree)$weight = x$edge.length
                tips = V(gtree)[tips_number]
                res_dist_tree = dist_tree_par(gtree, col1, col2,
                  tips)
                stopCluster(cl)
            }
            return(unlist(res_dist_tree))
        }
        else {
            warning("Requires info argument")
            return(NULL)
        }
    }
    else {
        warning("Type not recognized")
        return(NULL)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
