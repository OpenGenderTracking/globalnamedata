Tools to download and process name data from various sources.

## Setup

The easiest way to set up Global Name Data is to clone the repo and open an R console in the base directory. 

Functions for downloading and processing data from the various sources are exposed in the workspace as each script in the `src/gather` directory are read in to R. Shared functions for handling data are made available by loading `src/shared.R`.

### Dependencies

Any version of R 2.14 or greater should work for this project. Prior to 2.14 there were some differences in how R handles regular expressions but this may or may not impact the project materially. The project was built using R 2.15.2 in OS X.

Because different governments store and expose their birth information in many different ways, Global Name Data depends on a variety of R packages for data import and handling.

__Import__

* `XML` [details](http://cran.r-project.org/web/packages/XML/index.html)
* `RCurl` [details](http://cran.r-project.org/web/packages/RCurl/index.html)
* `gdata` [details](http://cran.r-project.org/web/packages/gdata/index.html)

__Data handling__

* `plyr` [details](http://cran.r-project.org/web/packages/plyr/index.html)
* `reshape2` [details](http://cran.r-project.org/web/packages/reshape2/index.html)

`RCurl` and `gdata` will introduce external dependencies, namely Curl and perl (nearly any version in reasonable use will work). On *NIX systems this shouldn't present a problem as both should already be installed. 

Each package need only be installed once in an R environment and will persist across sessions. [Installing R packages](http://cran.r-project.org/doc/manuals/R-admin.html#Installing-packages).

Packages may be by typing `install.packages(c("XML", "RCurl", "gdata", "plyr", "reshape2"))` into the R console. The package installer will determine and install package dependencies as needed.

### Classification

Currently the Global Name Data project is used to produce gender estimates for byline and content classification in [Open Gender Tracker](https://github.com/OpenGenderTracking/GenderTracker). Each name is associated with a likely gender based on incidence in our name data. Future improvements will include estimates of confidence and mechanisms to test and cross-validate gender classifications.

## Contributing

We love pull requests. While not required, please try to adhere to [Google's R Style guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html). 

## Preprocessed data

Because name data is often provided under a Creative Commons compatible license, preprocessed data will be released in the `data` directory. 

These will include:

* By country name/gender counts
* By region (specifically the UK) name/gender counts
* Classification output by region

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

Records for the United Kingdom are broken out across England and Wales, Northern Ireland and Scotland. The Office of National Statistics records births for England and Wales while Northern Ireland and Scotland are recorded seperately. In all jurisdictions the minimum number of births per year for each name is 3. 

#### England and Wales

Full name data is provided between 1996 and 2011. The ONS offers historical summary data for 1904-1994 but these are restricted to the most popular names per year and so not of much analytical value. Information on the data itself can be found [here](http://www.ons.gov.uk/ons/guide-method/user-guidance/health-and-life-events/births-metadata.pdf) **[PDF]**.

#### Northern Ireland

Northern Ireland provides full name data between 1997 and 2011. Like the ONS, summary data is offered but does not add much value. Information on NISRA data can be found [here](http://www.nisra.gov.uk/demography/default.asp28.htm).

#### Scotland

Scotland only provides full name data for 2009 and 2010. Summary data is offered over the past 20 years. General information about birth record data in Scotland is available [here](http://www.gro-scotland.gov.uk/statistics/theme/vital-events/births/bckgr-info.html).

# License 

See the [LICENSE](https://github.com/OpenGenderTracking/globalnamedata/blob/master/LICENSE.md) file for details.