#include "DoubMatrix.h"
#include "DoubArray.h"

#include <iostream>
#include <fstream>
#include <random>
#include <vector>
#include <algorithm>

// Class ************************************************************************

// Geometric Brownian Stock Price Simulation Class
class Brownian
{
private:
    double m_mean;
    double m_sd;
    // for merton jump: m_lambda, m_jump_mean, m_jump_sd, m_z
    // in constructor: double jump = 5.0/252.0, double jump_mean = 0, double jump_stdv = 1
protected:

public:

    // Constructor
    // Default: standard normal
    Brownian(double mean=0, double sd=1){
        //µ
        m_mean = mean;
        //ø
        m_sd = sd;
    }

    // Sample mean µ, sample variance ø MLE and setter
    void mu_sd_estimator(DoubArray &log_rate)
    {
        // Estimate µ
        double sum = 0;
        for (int i=1; i<log_rate.getLength(); i++){
            sum += log_rate[i];
        }
        m_mean = sum / (log_rate.getLength());

        // Estimate ø
        double sum_var = 0;
        for (int i=1; i<log_rate.getLength(); i++){
            sum_var += (log_rate[i] - m_mean)*(log_rate[i] - m_mean);
        }

        m_sd = sqrt(sum_var / (log_rate.getLength() - 1));
    }

    // Get m_mean
    double getMean(){return m_mean;}

    // Get m_sd
    double getSD(){return m_sd;}

    // Find the next price given previous price
    double NextPrice(double prev)
    {
        std::random_device generator;
        std::normal_distribution<double> stdnormal (0,1);
        double stdNormal = stdnormal(generator);

        double next_price =0;
        // for ∆t = 1 day = 1
        next_price = prev * exp((m_mean - m_sd*m_sd/2) + m_sd*stdNormal);

        return next_price;
    }

    // A matrix to store all stimualted prices for all paths
    DoubMatrix simulate(int nday,int npath, double S0)
    {
        DoubMatrix mtx(nday,npath);

        // first day of every path is the given S0 price
        for (int p=0; p<npath;p++){
             mtx.setValue(0,p,S0);
        }

        for (int p=0; p<npath; p++){
            for (int d=1; d<nday; d++){
                mtx.setValue(d, p, NextPrice(mtx.getValue(d-1,p)));
            }
        }
        return mtx;
    }

    // Get the summary statistics: MSE, etc.
    // double

};


// Functions ********************************************************************

// getData = Read_data
// Read stock price data from a file
DoubArray getData(std::string const &file)
{
    std::ifstream read_file(file);
    if (!read_file.is_open())
        return 1;

    int number_of_rows = 0;
    while(!read_file.eof()){
        double dummy;
        read_file >> dummy;

        // If ".fail()" are "false" = the next row is not empty, continue
        if (!read_file.fail())
            number_of_rows++;
    }

    read_file.clear();
    read_file.seekg(std::ios::beg);

    DoubArray price_data(number_of_rows);
    for (int i = 0; i < number_of_rows; i++){
        double dummy;
        read_file >> dummy;
        price_data.setValue(i,dummy);
    }
    return price_data;
}

// Daily natrual log return array
DoubArray log_return_rate(DoubArray& price_data)
{
    DoubArray log_rate(price_data.getLength()-1);

    for (int i=0; i<log_rate.getLength(); i++){
        log_rate[i] = log(price_data[i+1]/price_data[i]);
    }

    return log_rate;
}

// Get the average of St(s) that will be used in option pricing
double AvgSt(DoubMatrix& mtx)
{
    double sum = 0.0;
    for(int p = 0; p<mtx.getCol(); p++){
    sum += mtx.getValue(mtx.getRow()-1,p);
    }
    double St = sum/mtx.getCol();
    return St;
}


// Calculate call and put option's price
double Option_Pricing(double risk_free, double strike, int maturity, bool option, double estimate_ST){
    double val_at_maturity = 0.0;

    //If option == 1 then this is a call option
    if (option){
        val_at_maturity = fmax(estimate_ST - strike,0);
    }

    //If option == 0 then this is a put option
    else{
        val_at_maturity = fmax(strike - estimate_ST,0);
    }
    //Calculate the option price using present value
    double option_price = option_price = val_at_maturity*exp(-risk_free*maturity);
    return option_price;
}


// Get MSE
// Array is the estimate array
double Mean_Squared_Error(DoubMatrix &matrix, double real_price){
    double MSE = 0.0;
    double sum = 0.0;

    //Loop through the columns of simulations
    for(int i = 0; i < matrix.getCol(); i++){
        sum += pow(matrix.getValue(matrix.getRow()-1,i) - real_price, 2);
    }

    MSE = sqrt(sum / matrix.getCol());
    return MSE;
}

// Value at Risk
double VaR (DoubMatrix &matrix){
    double plevel = 0.01;
    std::vector <double> ST(matrix.getCol());
    for (int i=0; i<matrix.getCol(); i++){
        ST.at(i) = matrix.getValue(matrix.getRow()-1,i);
    }
    std::sort(ST.begin(), ST.end());
    for (int i=0; i<ST.size(); i++){
      //  std::cout << ST.at(i) << std::endl;
    }
    int nlevel = plevel*ST.size();
    return ST[0] - ST[nlevel];
}

double MAPE (DoubMatrix &matrix, double real_price){
  double MAPE = 0.0;
  double sum = 0.0;

  //Loop through the columns of simulations
  for(int i = 0; i < matrix.getCol(); i++){
      sum += fabs(real_price - matrix.getValue(matrix.getRow()-1,i)) / real_price;
  }

  MAPE = 100 * (sum / matrix.getCol());
  return MAPE;
}

// Main ***********************************************************************

int main()
{
    // Using DoubArray copy constructor to:
    // Declare and assign a DoubArray object of all Tesla price data
    DoubArray price_data(getData("Tesla_5yr_NODATE.prn"));

    std::ofstream write_tesla("Tesla_GBM2.dat");

    // Declare and assign a DoubArray object of all daily log rates
    DoubArray log_rate(log_return_rate(price_data));

    // Declare Brownian object
    Brownian tesla;
    tesla.mu_sd_estimator(log_rate);

    // Declare DoubMatrix object to store all simulations (day = 252, simlations = 500)
    DoubMatrix mtx_tesla_simulate(252,500);

    // Simulate 252 days, 10 path, starting price, S0 = $315.4 (price 1 years ago)
    // Assign the result to the matrix
    mtx_tesla_simulate = tesla.simulate(252,500,315.4);

    // Print out the simulation matrix
    write_tesla << mtx_tesla_simulate;

    // Average of 500 paths' end prices at day 252nd
    double avg = AvgSt(mtx_tesla_simulate);
    std::cout << "The average simulated ST: "<< avg << std::endl;

    // Estimate Call Price: interest rate, strike, maturity, bool:1-call 0-put, avg)
    double estimate_option_price = Option_Pricing(0.0144, 300, 252, 1, avg);
    std::cout << "Call Price: " << estimate_option_price << std::endl;

    // Reset to store put's price
    estimate_option_price = 0.0;

    // Estimate Put Price
    // interest rate = 1.44% for 1-3 year Canada Gov Bond on 2017 Nov 13
    estimate_option_price = Option_Pricing(0.0144, 350, 252, 0, avg);
    std::cout << "Put Price: " << estimate_option_price << std::endl;

    // MSE of GBM
    double MSE = Mean_Squared_Error(mtx_tesla_simulate, price_data[1259]); 
    std::cout << "Mean Squared Error: " << MSE << std::endl;

    // VaR base on Monte Carlo Simualtion of Stock
    std::cout << "VaR at 1% probability: $" << VaR(mtx_tesla_simulate) << std::endl;

    double Mean_Absolute_Percentage_Error = MAPE(mtx_tesla_simulate, price_data[1259]);
    std::cout << "Mean Absolute Percentage Error: " << Mean_Absolute_Percentage_Error << std::endl;

    return 0;
}

/* Alogorithm
 1. fn:read data and store them in a doubarray called price_data
 2. fn:read price_data, calculate all daily log-normal return rates= log(st/st-1) and store in a doubarray called log_rate
 3. inside gbm class:
    a. declare parameters µ=average of all daily log-normal return rates and ø=s/d of all daily log-normal return rates
    b. member fn: 1 function to mle estimate the two parameters needed
    c. member fn: 1 function to estimate the next price of a given previous price
    d. member fn: 1 function that calls the NextPrice function everytime it tries to calcualte the next simualted price in a path.
                  this function returns a new matrix declared inside it that is consisted of n path(col), n days(row) in each path.
                  this function needs a S0 stock price, n path wanted, and n days wanted to start running.

 Flaws in this model: m_mean(µ) and m_sd(ø) is constant through the simulation, but tesla is a volatile stock, thus the constant
 parameters might not give a good estimation in NextPrice. But the goal is to still find the size of the errors in the GBM model.

 // Random number generator
 // Generators: Objects that generate uniformly distributed numbers.
 // Distributions: Objects that transform sequences of numbers generated by a generator into sequences of numbers that
 // follow a specific random variable distribution, such as uniform, Normal or Binomial.
*/
