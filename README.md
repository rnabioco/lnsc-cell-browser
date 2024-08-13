## lnsc-cell-browser

### Adding a new publication

* Add a new entry to pubs.yml, the only information needed for the entry is
  the pubmed URL which is added as the "pubmed" field
* Optional links can also be added under the "links" field,
  this includes "github", "atlas", and "geo"
* Run pre-build.R to scrape the remaining information for the publication and
  to update authors.yml
* Rebuild site

### Adding a new cell atlas

* Add the atlas link to the associated publication listed in pubs.yml
* Rebuild site

### Adding a new project

* Add project information to projects.yml
* Required fields include, "title", "short", "hypothesis", and "abstract"
* An optional image can also be included
* Edit authors.yml to add authors to the new project
* Run pre-build.R to automatically associate publications with the new project
* Rebuild site

### Building site

* Remove directories from previous build (_site, .quarto)
* Run pre-build.R and commit changes
* Render site

