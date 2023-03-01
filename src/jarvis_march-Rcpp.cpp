#include <vector>

#include<Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
std::vector<double> dotproduct(const std::vector<double>& a,
const std::vector<double>& b)

{
    int n = a.size();
    std::vector<double> result(n);
    if(b.size() != n)
        {
            std::vector<double> empty;
            return empty;
        }
    for(int i = 0; i < n; i++)
        {
            result[i] = a[i]*b[i];
        }
    return result;
}
