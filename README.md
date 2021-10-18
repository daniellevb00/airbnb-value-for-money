# Assessing price determinants of AirBnB listings and its reflection on consumers' price-quality ratings 
_How do the different attribute features influence the listing price? And is the price rationalization well-reflected in consumers' ratings of the price-quality ratio (i.e., the perceived 'value of money')?_

<p align = 'center' >
   <img src = 'https://upload.wikimedia.org/wikipedia/commons/thumb/6/69/Airbnb_Logo_Bélo.svg/2560px-Airbnb_Logo_Bélo.svg.png' width = 640 height = 200 />
</p>


## Motivation

With Airbnb being the largest accommodation firm in the marketplace, enjoying a remarkable and flexible supply of listings and a record-breaking growth in the number of guests, the platform is recognized as a disruptor for the lodging industry. Aside from its many economic and financial benefits, the value of Airbnb listings is too perceived differently from that of traditional hotels. Namely, consumers engaging in the sharing economy seem to attach more value to the whole experience of their stay (the socialable aspect). The physical and non-physical attributes which are reflected on the price of Airbnb accommodations may play a crucial role on Airbnb guests' decision-making. Thus, the price of Airbnb properties is determined based on the value consumers place on the attributes of Airbnb accommodations. Therefore, examining the price determinants of the nightly listing price is crucial in understanding the factors that drive the growth of the sharing economy. 

Moreover, it would be interesting the research the mean price-quality ratios for each listings relative to the amentiies its offers to observe whether consumers expectations of a listing of X price are close to being realistic (or whether they argue that the higher listing price is not justified). For instance, from previous research by Liang et al. (2017) we know that hosts awarded a superhost badge post their posts at higher prices, especially when they receive more reviews that are higher ratings: But do consumers too think that superhost actually provide better services? Or is the higher price for the superhost status not justified in terms of price/quality ratings?

<p align="center">
  <img src = 'https://media.giphy.com/media/TjkBNCMtRzPHGw6sGt/giphy.gif' width = '280' height = '300' /> 
  <img src = 'https://media.giphy.com/media/gjUMaVJZh6o8gsD4Wb/giphy.gif', width = '300' height = '330' />
</p>

## Method and results

### Method 

We made use of OLS regression techniques to examine the price determinants of Airbnb accommodations, more specifically the _multiple linear regression model_. Namely, we focused on the relationship between the nightly published rate (metric DV) and some (non-)metric IV's which represent the attribute features of 'experiencing a stay at an Airbnb listing' : Y = b0 + b1X1 + b2X2 + ... + bkXk + ei, where b0 represents the baseline price per a night's stay and X1,X2...,Xk represent the (non-)metric features. Therefore, we identified some attribute feature categories (the price determinants) based on some columns of the original dataset as well as newly generated columns (with values from the 'amenities' variable of the dataset). These different attribute feature categories include: 

Index | Category                   | Columns included
------|----------------------------|--------------------
1     | Space attributes           | room type, waterfront, patio/balcony 
2     | Listing quality attributes | number of reviews, mean review rating, bedrooms, beds
3     | Common listing attributes  | kitchen, washer, wifi, TV, coffee maker, dishwasher, oven
4     | Extra listing attributes   | fireplace, air conditioning, hot tub, gym, free parking
5     | Host quality attributes    | superhost, license, host listings count, instant bookable, host greets you
6     | Convenience attributes     | crib, luggage_dropoff, single-level home, changing table
7     | Safety attributes          | carbon monoxide alarm, fire extinguisher, security cameras, private entrance
<p align="center">
  <img src = 'https://media.giphy.com/media/gHoJgcYgjalRxouMir/giphy.gif' width = '400' height = '220' /> 
  <img src = 'https://media.giphy.com/media/PjlpT8M92at7Jy2v5G/giphy.gif', width = '400' height = '220' />
</p>

For the second part of our research, we examined which amenities (and of which of the categories we identified) influence the consumers' rev_value (the rating for the price-quality ratio for the listing) the most. Namely, this would indicate what the concept 'quality' means to a consumer for a certain listing at a certain price. Are hosts focussing on the right amenities to offer to increase the satisfaction of their guests? or can they improve? Are the elements that for the host constitute or justify a higher price, the same as for the guests?

### Results 
_Still need to be added_

## Repository overview 
An overview of the directory structure and files: 
* \data = stores the raw data files (unmodified). 
* \gen\data-preparation = stores any files generated in data-preparation.
* \gen\analysis = stores any files generated during analysis. 
* \src\data-preparation = stores any source code used for the data anlaysis. 
* \src\analysis = stores any source code used for the analysis. 
* \paper = stores the final paper.

## Running instructions
The datasets used for this project are available at [Inside AirBnB](http://insideairbnb.com/get-the-data.html).
Data cleaning and analysis was done in [RStudio](https://www.rstudio.com/), which is an integrated development environment (IDE) for [R](https://www.r-project.org/). 

<p align="center">
  <img src = 'https://media.giphy.com/media/J4JIj5vHSjhHAPy9w5/giphy.gif' width = '340' height = '340' /> 
</p>

## More resources
Some related literature if you are interested in reading more about the topic: 
* Perez-Sanchez, V. R., Serrano-Estrada, L., Marti, P., & Mora-Garcia, R. T. (2018). The what, where, and why of Airbnb price determinants. Sustainability, 10(12), 4596.
* Magno, F., Cassia, F., & Ugolini, M. M. (2018). Accommodation prices on Airbnb: effects of host experience and market demand. The TQM Journal.
* Falk, M., Larpin, B., & Scaglione, M. (2019). The role of specific attributes in determining prices of Airbnb listings in rural and urban locations. International Journal of Hospitality Management, 83, 132-140.
* Voltes-Dorta, A., & Sánchez-Medina, A. (2020). Drivers of Airbnb prices according to property/room type, season and location: A regression approach. Journal of Hospitality and Tourism Management, 45, 266-275.
* Tong, B., & Gunter, U. (2020). Hedonic pricing and the sharing economy: How profile characteristics affect Airbnb accommodation prices in Barcelona, Madrid, and Seville. Current Issues in Tourism, 1-20.
* Barron, K., Kung, E., & Proserpio, D. (2021). The effect of home-sharing on house prices and rents: Evidence from Airbnb. Marketing Science, 40(1), 23-47.

## Contributors
This is a repository for the course [Data Preparation and Workflow Management](https://dprep.hannesdatta.com) at Tilburg University as part of the Master's program 'Marketing Analytics', used for the team project of group 3. 

Members of our team: 

* [Lesley Haerkens](https://github.com/lesleyhaerkens), l.w.g.haerkens@tilburguniversity.edu

* [Mila Gargiulo](https://github.com/MilaGargiulo), m.r.e.m.gargiulo@tilburguniversity.edu

* [Anouk Bor](https://github.com/AnoukBor), a.m.bor@tilburguniversity.edu

* [Mandana Khabbazi](https://github.com/Mandanakhabbazi), m.i.khabbazi@tilburguniversity.edu

* [Daniëlle van Bruggen](https://github.com/daniellevb00), d.m.vanbruggen@tilburguniversity.edu

