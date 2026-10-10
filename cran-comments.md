## Resubmission of a New Package
This is a Resubmission of a new package. A CRAN team member responded to us with comments that we have addressed. 

* We have removed "Tools for" and elaborated on the description per CRAN feedback. We were also recommended to add a citation references describing the methods in our package, but the current paper describing these is in review. We will add the corresponding citation when the manuscript is published.
* We have removed all print statements. These were being used to return plots to the user, but we now just return the object.
* win-builder (devel, release, and oldrelease) checks suggest some URLs may be invalid. We have checked the URLs and they are correct and can be reached via web browser and are valid.
* win-builder (devel, release, and oldrelease) checks suggests some Possibly misspelled words in DESCRIPTION. We have checked these and they are a spelled correctly. 

## Test environments
* local Windows 11 install, R 4.6.1
* Ubuntu 24.04.3 LTS on GitHub Actions(devel, release, oldrelease)
* win-builder (devel, release, and oldrelease)
* macOS 15 on GitHub Actions (release)

## R CMD check results
* There were no ERRORS or WARNINGS