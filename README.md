# POCDART (Point-Of-Care Dashboards, Automation, Reporting)

POCDART is an open source toolkit designed to help streamline point of care surveys and reporting.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

```
Local or Cloud-based Server running:
  * REDCap
  * Andy Martin's REDCap Hook Framework
  * Shiny-Server
  * Drupal 8
  * R
```

### Installing
Create a directory in /srv/shiny-server/ and copy the POCDART repository into it. If you do not have a shiny-server directory, please install shiny-server.
```
/srv/shiny-server/pocdart/
```
# REDCap
Create a new project in REDCap manually or using the template included in this repository. Make sure to record the names for each event, as these will be used during the POCDART setup.
```
* patient_admission_arm_1
* patient_repeating_arm_1
* patient_discharge_arm_1
```
Make sure that surveys are enabled, instruments are selected for your events, and the survey settings match the following:
```
* Use Enhanced Radio/Checkboxes
* Custom question numbering
* All questions on one page
* Auto-continue to next survey
* If the final survey in the chain, set Redirect to a URL
```

Move contents of DET and HOOK folders to the appropriate locations in your REDCap server directory. If using included DETs, please update det_handler.php. Update global_hooks.php or project specific hooks to include code below.

```
Directories:
/var/www/html/redcap/det
/var/www/html/redcap/hooks/framework/resources/

Hook Updates:
if ($hook_event == 'redcap_survey_page_top') {
  include_once HOOK_PATH_FRAMEWORK . "resources/common_resource.php";
  include_once HOOK_PATH_FRAMEWORK . "resources/autoscroll.php";
  include_once HOOK_PATH_FRAMEWORK . "resources/autoadvance.php";
}

```
Enable DET Handler in Project Settings, and generate an API key for the project with Export and Import settings. Record this key for setup of the POCDART toolkit.

Finally, create a new record in REDCap and test the instruments in each event to verify settings and functionality. If things do not work as intended, verify that hooks and settings are correct.

# POCDART
## Setup
After the REDCap project has been created, the POCDART toolkit needs to be customized to your project. This can be performed in a few steps:
* Create keys.R file and place it in shiny_code_repo/redcap_api.R. Contents of the file should be the following:
```
api_url <- "<url to your redcap api>"
offnetwork_url <- "<url to your redcap api>"

getRedcapToken <- function(key_name) {
  keys <- c("<project specific name>" = "<project specific api key>")
  return(keys[key_name])
}
```
* Update global_variables.R to match your project. This includes populating:
  * assay_list - a list of the assays on the admission/discharge events and the repeating event
  * public_survey_url if using the hyperlink registration method
  * FREQUENCY - length of time between assessments
  * EXPIRATION - length of time after due date to expire assessment
  * INVITE - days before frequency reached
  * FUTUREWINDOW - the day at which the assessment shows up early
  * admission_form - registration instrument
  * starting_instrument - starting instrument that the toolkit generates links to for admission and repeating
  * discharge_instrument - instrument that the toolkit generates link to for discharging
  * expiration_instrument - instrument used for expiring an assessment
  * survey_key - update the getRedcapToken to call the <project specific name> used in the keys.R file
  * events - update the events to match your project event labels
  
* Update setwd in dart_cron.R to match your project directory

* In server.R and global_variables.R, replace all instances of "POCDART" with a project specific string. 

* Set permissions for project directory

## Initialize
To initialize the system, go ahead and source the dart_cron.R script after POCDART has been set up. Assuming everything is set up correctly, this should populate the data directory with the project's data and the creation of a number of survey storage CSVs. If this fails, verify that you've set up the project as described earlier and that you have the correct REDCap API key.

## Automate
The system currently assumes that you are going to have two automation steps -
  * A nightly cronjob that runs dart_cron.R
  * A 5/10/etc minute cronjob that runs update_cron.R
  
dart_cron.R regenerates surveys, allowing users to always have an up to date survey list on daily use.

update_cron.R continuously updates local redcap data for up to date reports, survey completions, etc. If using a database to populate new patients, this can also be edited to check and add new patients.

### Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [REDCap](https://www.project-redcap.org/) - Survey creation and database
* [Drupal 8](https://www.drupal.org/8) - Serving POCDART to end user
* [R](https://www.r-project.org/) - Code backbone of POCDART
* [RStudio Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/) - 

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Andrew Ready** - *Initial work* - Sheppard-Pratt Lieber Research Institute
* **Alexander Maclay** - *Initial work* - Sheppard-Pratt Lieber Research Institute

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* REDCap Consortium
