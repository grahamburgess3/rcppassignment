#include <vector>

#include<Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
std::vector<double> dotproduct(const std::vector<double>& a,
const std::vector<double>& b)

{
    int n = a.size();
    return n
}
