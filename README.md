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
* `DDN` : date of birth (YYY)
* `UH` : Origin of the patient. Any alphanumeric string.
* `nb_labo` : Patient codification. Any alphanumeric string.
* `sexe` : Gender. Any alphanumeric string.
* `aaSubstitutions` : aminoacid changes (example: N:M234I,ORF1a:D827G,S:D614G). Use one letter AA
* `aaDeletions` : aminoacid deletions (example: ORF1a:M85-,ORF1a:V86-). Use one letter AA
__then add in column the annotations you are interested in testing__


`2]. tree `: __Alignment tree.newick__


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
