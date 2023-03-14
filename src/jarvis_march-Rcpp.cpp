#include <iostream>
#include <vector>
#include <limits>
#include <stdlib.h> 
#include <algorithm>
#include <fstream>
#include <sstream>
#include <string>
#include <cmath>

#include<Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
struct point
/*
A structure to represent a single point on a two-dimensional plane

Attributes
----------
x : double
    x-coordinate
y : double
    y-coordinate

Methods
-------
None
*/    
{  
    double x;
    double y;
    
    point(double _x,double _y)
    /*
    Initialise instance of the point structure

    Parameters
    ----------
    _x : double
        x-coordinate
    _y : double
        y-coordinate

    Returns
    -------
    None
    */
        
    {
        x = _x;
        y = _y;   
    }
};

struct triplet_of_points
/*
A structure to represent a group of three points (one, two and three) on a two-dimensional plane

Attributes
----------
right_turn : 
    True if a traversal from point 1 to point 3 via point 2 involves a right turn.
    I.e. if the counterclockwise angle between a and b is 180 degrees or less. 
    If the traversal involves just a straight line, right_turn = True
    If the traversal involves a 360 degree turn, right_turn = False
collinearity : 
    True if the three points are collinear. 
a :
    the dimensions of the line between the first and second point in the triplet
b : 
    the dimensions of the line between the third and second point in the triplet
determinent : 
    the determinent of matrix (a^T, b^T)
    I.e. the cross product between a and b. 
dot_product : 
    the dot product of a and b

Methods
-------
find_orientation:
    determines whether a traversal from point 1 to point 3 via point 2 involves a right turn. 
*/    
{  
    bool right_turn {};
    bool collinearity {};
    double determinent {};
    double dot_product {};
    
    triplet_of_points(point p1, point p2, point p3)
    /*
    Initialise instance of the triplet_of_points class

    Parameters
    ----------
    p1 : point
        First point in triplet
    p2 : point
        Second point in triplet
    p3 : point
        Third point in triplet
    Returns
    -------
    None
    */
        
    {
        point a(p1.x - p2.x, p1.y - p2.y);
        point b(p3.x - p2.x, p3.y - p2.y);
        determinent = a.x * b.y - b.x * a.y;
        dot_product = a.x * b.x + a.y * b.y ; 
    }
    
    void find_orientation()
    /*
    Find the orientation (i.e. right-turning or not) of the triplet. 
    Also identify if points on triplet are collinear 

    Parameters
    ----------
    None
    
    Returns
    -------
    None
    */
      
    {        
        if (determinent > 0) // right turn
        {
            right_turn = true;
            collinearity = false;
        }
        else if (determinent == 0 && dot_product < 0) // straight line, categorised as right turn
        {
            right_turn = true;
            collinearity = true;
        }
        else if (determinent == 0 && dot_product > 0) // back on itselft, cateogorised as left turn
        {
            right_turn = false;
            collinearity = true;
        }
        else if (determinent < 0) // left turn
        {
            right_turn = false;
            collinearity = false;
        }
    }
};

int find_leftmost_point(double leftmost_val, std::vector<point> points)
/*
Finds the leftmost point in a set of points. 
If there are two points on the left with same x value, this function will choose the one it encounters first.
This is arbitrary but does not affect the outcome of the algorithm, only the ordering of points on the hull.

Parameters
----------
leftmost_val : double
    the current estimate of the x-value of the leftmost point. 
points : vector<point>
    vector of points which are being analysed

Returns
-------
leftmost_index_update : int
    index (within the points vector) of the leftmost point. 
*/  

{    
double leftmost_val_update {leftmost_val};
int leftmost_index_update {};

    for(int p = 0; p < points.size(); p++)
    {
        if (points[p].x < leftmost_val_update)
        {
            leftmost_val_update = points[p].x;
            leftmost_index_update = p;            
        }
    }
    return leftmost_index_update;
};

int find_new_point(std::vector<point> points, std::vector<int> exceptions)
/*
Finds the index of a new point at random from a vector of points

Parameters
----------
points : vector<point> 
    vector of points which are being analysed
exceptions : vector<int>
    A vector of indices of points not to be included when choosing a new index at random

Returns
-------
new_point : int
    Index of a new point chosen at random from the set. 
*/

{
    bool looking_for_new_point {true};
    int new_point {};
    while (looking_for_new_point == true)
    {
        new_point = rand() % points.size();
        if (std::find(exceptions.begin(), exceptions.end(), new_point) == exceptions.end()) // new point not exception
        {
            looking_for_new_point = false;
        }
    }
    return new_point;
};

std::vector<point> find_convex_hull(std::vector<point> points)
/*
Finds the convex hull of a vector of points. 
First this function deals with indices: i.e. it finds the indices of the points in the vector which are on the hull. 
It then translates these indicies into a vector of points which define the convex hull. 

Parameters
----------
points : vector<point>
    vector of points being analysed

Returns
-------
convex_hull_points : vector<point>
    vector of points on the convex hull
*/    

{
    // set up attributes
    std::vector<int> convex_hull {}; // list of indices indicating list-position of the points on the hull
    std::vector<point> convex_hull_points {}; // list of points (from the points class) on the convex hull
    bool all_points_collinear {true}; // change this if we find non-collinear points
    int leftmost_index;
    int rightmost_index;
    double leftmost_val {std::numeric_limits<double>::infinity()};
    
    // First deal with special cases of small sets of points
    if (points.size() == 0){
        std::cout << "No data points to analyse" << std::endl;
    }
    else if (points.size() == 1){
        std::cout << "Only one data point to analyse" << std::endl;
        convex_hull_points.push_back(points[0]);
    }
    else if (points.size() == 2){
        std::cout << "only two data points to analyse" << std::endl;
        // add leftmost point
        leftmost_index = find_leftmost_point(leftmost_val, points);
        convex_hull.push_back(leftmost_index);
        convex_hull_points.push_back(points[leftmost_index]);
        // add rightmost point
        if (leftmost_index == 0)
        {
            rightmost_index = 1;
        }
        else {
            rightmost_index = 0;
        }
        convex_hull.push_back(rightmost_index);
        convex_hull_points.push_back(points[rightmost_index]);
    }
    else {
        leftmost_index = find_leftmost_point(leftmost_val, points); // find leftmost point
        convex_hull.push_back(leftmost_index);
        bool not_complete_hull {true}; // this will change to False once the convex hull reaches its starting point
        
        // main while loop
        while (not_complete_hull == true)
        {
            // identify end of the current hull
            int end_of_hull_index {convex_hull.back()};
            
            // select candidate
            std::vector<int> exceptions {end_of_hull_index};
            int candidate {};
            candidate = find_new_point(points, exceptions);
            
            // test the candidate with test points
            for(int test_point = 0; test_point < points.size(); test_point++)
            {
                if (test_point != candidate && test_point != end_of_hull_index)
                {
                    // create new instance of triplet class from the end of the hull -> test point -> candidate
                    triplet_of_points triplet(points[end_of_hull_index], points[test_point], points[candidate]);
                    triplet.find_orientation();
                    
                    // update candidate if angle theta in the triplet from a -> b counterclockwise satisfies 0 < theta <= 180
                    if (triplet.right_turn == true)
                    {
                        candidate = test_point;
                    }
                    
                    /* update candidate if triplet is collinear, candidate on the convex hull but not test_point
                    this stops the algorithm prioritising a point already on the convex hull ...
                    ... when comparing against a collinear test point ...
                    ... It might then have chosen a suboptimal point at the next test point, but this ensures not. */
                    if (triplet.collinearity == true && std::find(convex_hull.begin(), convex_hull.end(), candidate) != convex_hull.end() && find(convex_hull.begin()+1, convex_hull.end(), test_point) == convex_hull.end())
                    {
                        candidate = test_point;
                    }
                    
                    // Update instance attribute if a non-collinear triplet has been found. 
                    if (triplet.collinearity == false)
                    {
                        all_points_collinear = false;
                    }
                }
            }
            
            // update hull
            convex_hull.push_back(candidate);

            // is the hull complete?
            if (std::find(convex_hull.begin(), convex_hull.end()-1, convex_hull.back()) != convex_hull.end()-1)
            {
                not_complete_hull = false;
                if (convex_hull.front() == convex_hull.back())
                {
                    convex_hull.pop_back();
                }
            }
        }
            
        // If not all points are collinear, then simply add all point objects the hull, based on their index. 
        if (all_points_collinear == false)
        {
            for(int point_to_add = 0; point_to_add < convex_hull.size(); point_to_add++)
            {
                convex_hull_points.push_back(points[convex_hull[point_to_add]]);
            }
        }

        /* If all the points collinear, ignore self.convex_hull and create self.convex_hull_points from scratch
        This is because in this case, the algorithm will go from the start to the furthest point to find the hull ...
        ... and then it will go back, continuing to add points onto the hull, but in this case ...
        ... it might skip some points and go directly back to the first point. 
        But we can instead generate it from scratch. */   

        else
        {
            for (int i = 0; i < convex_hull.size(); i++)
            {
                // add first point
                if (i == 0)
                {
                    convex_hull_points.push_back(points[convex_hull[i]]);
                }

                // go out to the end of the hull
                else if (std::find(convex_hull.begin(), convex_hull.begin()+(i-1), convex_hull[i]) == convex_hull.begin()+(i-1))
                {
                    convex_hull_points.push_back(points[convex_hull[i]]);
                }

                else
                {
                    break;
                }          
            }
        }
    }
    return convex_hull_points;
};

std::vector<double> jarvis_march(std::vector<double> x, std::vector<double>& y)
/*
Implement an alternative Jarvis march algorithm (for R package build). 

This function takes the inputted x and y vectors, initialises a vector of points and finds its convex hull. 
x coords of convex hull are returned. 

Parameters
----------
x : std::vector<double>
    x coords
y : std::vector<double>
    y coords

Returns
-------
hull_x : std::vector<double>
    x coords of hull
*/
    
{
    // read points
    // std::list<std::vector<double> > xy_pairs {readcsv(filename)};
    std::vector<point> points {};
    for(int i = 0; i < x.size(); i++)
    {
        point new_point(x[i],y[i]);
        points.push_back(new_point);
    }

    // find hull
    srand(10);
    std::vector<point> hull {};
    hull = find_convex_hull(points);
    
    // output
    std::vector<double> hull_x {};
    std::vector<double> hull_y {};
    for(int hull_index = 0; hull_index < hull.size(); hull_index++)
    {
        hull_x.push_back(hull[hull_index].x);
        hull_y.push_back(hull[hull_index].y);
    }
    
    return hull_x;
};
