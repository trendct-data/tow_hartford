
# Analyzing towed vehicles in Hartford

This repository contains source data and analysis code for the Trend CT story:

* [Where are the most vehicles towed in Hartford?](http://trendct.org/2016/09/07/where-to-avoid-tow-trucks-in-hartford/). 

# Walkthroughs

* Analyzing Hartford towing data- [[RMarkdown](https://github.com/trendct-data/blob/master/tow_hartford/index.Rmd)] [[HTML](http://trendct-data.github.io/tow_hartford/)]

### The Data

The data comes from the Hartford Police Department’s 9–1–1 database and includes details such as:

* Tow firm responsible for towing
* Phone number of tow firm
* Details on vehicle towed such as state, year, make, model, color, and plate
* Address and date of tow

The reasons for being towed and tracked in the database include (but is not indicated as such in the dataset):

* A parking scofflaw (Excessive parking tickets)
* Stolen vehicle or vehicle involved in a crime
* An emergency situation like a water main break or a snow ban
* A tow from a private parking lot

The data is exported into a text file every hour — and from there the data is exported into the portal every hour, which Matt Zagaja scrapes and turned into [an app](http://trendct.org/2015/04/23/towed-in-hartford-heres-how-open-data-can-send-you-a-text-message/
).

### What's in this repo

* `data` [folder]((https://github.com/trendct-data/tow_hartford/data)) with raw data and modified data.
* `tow_analysis.R` is an R script analyzing the datasets and exploratory visualizations.
* `histograms.R` is an R script that will generates about 500 pngs of histograms illustrating the number of tows per hour per tow location.
* `slippy_heatmap_json_generator.R` is a script that outputs a json array that was used in a leaflet heatmap but was ultimately not published. 
* `.png` files of various chart outputs

----

### Philosophy

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. 

Check out the reproducible scripts and data behind many of our other stories in our [central repo](https://github.com/trendct-data)

Follow us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.
0Looking
