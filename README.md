
# The Intrinsic Dimension of Bioclimatic Variables

This repository contains code necessary to reproduce the analysis found in the manuscript entitled:

*How many variables does Wordclim have, really? Generative A.I. unravels the intrinsic dimension of bioclimatic variables.*

A preprint of this manuscript is available here: 

Russell Dinnage. [How many variables does Bioclim have, really?](https://www.authorea.com/users/5518/articles/643051-how-many-variables-does-bioclim-have-really). Authorea. June 06, 2023.
DOI: 10.22541/au.168607335.58947023/v1

**Running the Analysis**

The Variational Autoencoder is implemented and run on WordClim variables in [`R/bioclim_vae.R`](https://github.com/rdinnager/bioclim_intrinsic_dimension/blob/master/R/bioclim_vae.R)

[`R/post_process.R`](https://github.com/rdinnager/bioclim_intrinsic_dimension/blob/master/R/post_process.R) has code to make the figures found mostly in the Supplementary material, showing how to interpret the VAE encoded manifold dimensions. The final 5 manifold dimension maps (before projection) were made using [`R/make_maps.R`](https://github.com/rdinnager/bioclim_intrinsic_dimension/blob/master/R/make_maps.R)

Lastly, the downstream Species Distribution Models (SDMs), implemented using the [`ENMTools`](https://github.com/danlwarren/ENMTools) R package, were conducted using the R package [`targets`](https://github.com/ropensci/targets). The logic for the `targets` workflow is in [`_targets.R`](https://github.com/rdinnager/bioclim_intrinsic_dimension/blob/master/_targets.R). See the `targets` [documentation](https://books.ropensci.org/targets/) for details on how to use the workflow.

This code uses my in-develpment package [`dagnn`](https://github.com/rdinnager/dagnn), which must also be installed in order to run the code. 

**Data**

WorldClim data necessary to run the code in [`R/bioclim_vae.R`](https://github.com/rdinnager/bioclim_intrinsic_dimension/blob/master/R/bioclim_vae.R) can be downloaded [here](https://www.worldclim.org/data/worldclim21.html). 

Data generated as part of this study, that is, the 5 manifold variables that encode the information in the 19 WorldClim bioclimatic variables have been provided as part of the R package [`biocman`](https://github.com/rdinnager/biocman).






