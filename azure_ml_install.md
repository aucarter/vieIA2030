# Azure ML Install instructions
1. Install non-R dependencies :
```
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libudunits2-dev
sudo apt-get install libgdal-dev
```
2. Install `devtools`:
```
install.packages("devtools")
```
3. Set the working directory to the location of the code directory:
```
setwd("vieIA2030")
```
4. Install R dependencies:
```
devtools::install_deps()
```
5. Load the package:
```
devtools::load_all()
```
