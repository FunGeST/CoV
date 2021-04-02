# CoV
This dashboard makes it easy to manipulate patient info with sequencing data of the COVID-19 pandemic and display it as chronology, summary tables and phylogenic trees


## Input files

`1]. annotation `: __Table with your annotation__
(the header is required, but the order of the columns can change)
* `name` : name of the sequence. Any alphanumeric string. Should refer to name used in alignment tree newick file
* `virus` : name of the virus. Any alphanumeric string. Default="sars_cov_2
* `clade` : name of the clade. According to nexstrain nomenclature (https://clades.nextstrain.org/)
* `pangolin_lineage` : name of the clade. According to Pangolin nomenclature (https://pangolin.cog-uk.io/)
* `qc.overallStatus` : QC evaluation of sequence. Any alphanumeric string.
* `date_prel` : date of sampling (DD/MM/YYY)
* `DDN` : date of birth (YYYY)
* `UH` : Origin of the patient. Any alphanumeric string.
* `nb_labo` : Patient codification. Any alphanumeric string.
* `sexe` : Gender. Any alphanumeric string.
* `aaSubstitutions` : aminoacid changes (example: N:M234I,ORF1a:D827G,S:D614G). Use one letter AA
* `aaDeletions` : aminoacid deletions (example: ORF1a:M85-,ORF1a:V86-). Use one letter AA
__then add in column the annotations you are interested in testing__


`2]. tree `: __Alignment tree.newick__


## Packages needed

The application was built on R version 4.0.2.

The following dependencies are required:
`shiny` (v1.6.0)
`shinyWidgets` (v0.6.0)
`shinyjs` (v2.0.0)
`tidyverse` (v1.3.0)
`knitr` (v1.3.1)
`ggtree` (v2.0.0 not +)
`tidytree` (v0.3.3)
`treeio` (v1.10.0 or +)
`ggplot2` (v3.3.3)
`g3viz` (v1.1.3 or +)
`janitor`(v2.1.0)
`lubridate` (v1.7.10)
`rcartocolor` (v2.0.0)
`ggtext` (v0.1.1)
`shadowtext` (v0.0.7)
`forcats` (v0.5.1)
`dplyr` (v1.0.3 or +)
`plotly` (v4.9.3)

The tested versions are indicated in brackets

Note: You only need to install those packages once (or not at all, if all these packages are already on your system).


## Starting the app

Click "Clone or download" -> "Download ZIP". Find the zip file (typically in your Downloads folder) and extract it to a desired location. Open the app.R file in RStudio and click "Run app".


## Contributors
* [Sandrine Imbeaud](https://github.com/FunGeST), Dr. PhD, INSERM UMRS1138, Centre de Recherches des Cordeliers - Sorbonne Université-Inserm-Université de Paris, Paris, France

* [Wack Maxime](https://github.com/maximewack), MD, Département d'Informatique Médicale, Biostatistiques et Santé Publique @, HEGP, Paris, France

* __Veyer David__, PharmaD, PhD,  INSERM UMRS1138, Centre de Recherches des Cordeliers - Sorbonne Université-Inserm-Université de Paris, Paris, France
                                  Unité de Virologie, Service de Microbiologie, HEGP, Paris, France

* __Péré Hélène__, PharmaD, PhD,  INSERM UMRS1138, Centre de Recherches des Cordeliers - Sorbonne Université-Inserm-Université de Paris, Paris, France
                                  Unité de Virologie, Service de Microbiologie, HEGP, Paris, France


## License

Copyright (C) 2021 Sandrine Imbeaud

CoV is a free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.

![](https://github.com/FunGeST/CoV/blob/main/IDDN%20Certificate_21135-3.png)
