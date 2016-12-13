### A simple peek over the large financial datasets
-------
Playing with some real-world financial datasets such as US stock prices and commodities. Following components are implemented:

* Data cleaning -> dealing with NAs and outlier, which frequently happens in financial datasets (e.g. close of exchanges).
* Rolling calculation of covariance/correlation of stocks over time series.
* Rolling cleaning the correlation matrix.
* Rolling in sample mean variance portfolio optimisation
* Rolling in-out sample risk comparison

#### Structure
Files are organised in following manner:

|- src

		|- init.r		
		
		|- file_checker.r
		
		|- cor_cleaner.r
		
		|- get_filtered_returns.r
		
		|- property_verify.r
		
		|- stats_of_us_stocks.r
		
		|- commodities_data_processing.r
		
		|- commu_detector.r
		
		|- rolling_corr_cov.r
		
		|- visual.r
		
		|- svmComms.py

|- datasets

		|- commodities	
		
		|- us_stocks.rda
		
		|- mid-results --	|- us_stocks	|- rolling_corrs....
										
		                 	|- commodities	|- rolling_corrs...
		                 
|- statistics

		|- commodity_stats.csv	
		
		|- us_stocks_stats.csv

							
								
#### Requirements
* zoo
* xts
* WGCNA
* data.table
* PortfolioAnalytics
* series
* igraph

#### Acknowledgement
This is a collaborative working results with Lu at [EPFL](http://epfl.ch), under guidance of [Prof. Challet](http://fiquant.mas.ecp.fr/people/faculty/damien-challet/), [EPFL](http://epfl.ch) and [CentraleSupélec](https://fr.wikipedia.org/wiki/CentraleSupélec).
