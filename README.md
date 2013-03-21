## What is this?

Most data on names and gender is ill-suited for any serious analytical purpose. Websites which collect data on birth names mainly offer searches, top ten lists and suggestions for parents. Most available data on the web comes either from commercial sources or from summary data.

We have collected birth record data from the United States and the United Kingdom across a number of years for all births in the two countries and are releasing the collected and cleaned up data here. We have also generated a simple gender classified based on incidence of gender by name. You can use this data for any purpose compatible with the [license](https://github.com/OpenGenderTracking/globalnamedata/blob/master/LICENSE.md).

And, unlike any other open record for name data, we've provided the scripts necessary to check our work! You don't need to trust us in order to trust the data.

## Setup

The easiest way to set up Global Name Data is to install it as an R package with [devtools](https://github.com/hadley/devtools). With `devtools` installed you can install the package directly from github with `install_github("globalnamedata", "OpenGenderTracking")`. Dependencies will be automatically installed.

## Not an R user?

If you're mainly interest in the data, pre and post classified name data is available in the [assets directory](https://github.com/OpenGenderTracking/globalnamedata/tree/master/assets).

## Contributing

We love pull requests. While not required, please try to adhere to [Google's R Style guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html). 

## Classification

Currently the Global Name Data project is used to produce gender estimates for byline and content classification in [Open Gender Tracker](https://github.com/OpenGenderTracking/GenderTracker). Each name is associated with a gender through the `addClassifier()` function using a binomial estimate. The specific method can be passed in as an argument, as can the thresholds for acceptance.

The classifier is specifically left decoupled from the import and processing function to allow for rapid testing and extension. 

# Data

## Data sources

Currently Global Name Data utlizes four sources:

* The United States 
    * [Social Security Administration](http://www.ssa.gov/)
* United Kingdom
    * UK [Office of National Statistics](http://www.statistics.gov.uk/hub/index.html)
    * [Northern Ireland Statistics and Research Administration](http://www.nisra.gov.uk/)
    * [Scotland General Register Office](http://www.gro-scotland.gov.uk/)

Processed data are provided under the Open Government License or the public domain where appropriate. See the [LICENSE](https://github.com/OpenGenderTracking/globalnamedata/blob/master/LICENSE.md) for details.

### United States

The Social Security Administration provides records for name and gender by year for births between 1880 and 2011. In each year, names with a minimum incidence of 5 births are counted. Prior to 1937, data should be considered suspect and retrospective as names were only recorded for individuals who received a social security card and birth year was not comprehensibly verified. More information can be found [here](http://www.ssa.gov/oact/babynames/limits.html).

### United Kingdom

Records for the United Kingdom are broken out across England and Wales, Northern Ireland and Scotland. The Office of National Statistics records births for England and Wales while Northern Ireland and Scotland are recorded seperately. In all jurisdictions the minimum number of births per year for each name is 3. Each jurisdiction provides summary data (e.g. top 10 names per year) but we do not download this data or use it in any way.

#### England and Wales

Full name data is provided between 1996 and 2011. The ONS offers historical summary data for 1904-1994 but these are restricted to the most popular names per year and so not of much analytical value. Information on the data itself can be found [here](http://www.ons.gov.uk/ons/guide-method/user-guidance/health-and-life-events/births-metadata.pdf) **[PDF]**.

#### Northern Ireland

Northern Ireland provides full name data between 1997 and 2011. Like the ONS, summary data is offered but does not add much value. Information on NISRA data can be found [here](http://www.nisra.gov.uk/demography/default.asp28.htm).

#### Scotland

Scotland only provides full name data for 2009 and 2010. Summary data is offered over the past 20 years. General information about birth record data in Scotland is available [here](http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/bckgr-info.html).

# License 

See the [LICENSE](https://github.com/OpenGenderTracking/globalnamedata/blob/master/LICENSE.md) file for details.