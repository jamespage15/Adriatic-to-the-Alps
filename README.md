# Adriatic-to-the-Alps
This repository includes the data and notebooks to replicate and reuse the analysis of the MADINI dataset published in:

Page, J. (Forthcoming). *From the Adriatic to the Alps. Trade in Transport Networks in Roman and Late Antique Northern Italy*. Oxford: Archaeopress. 

During the Roman period, inland regions are often assumed to have been difficult to access, with imports and trade dropping off as distance from the coast increased. Long-distance maritime trade has been the subject of intensive study, but the complex dynamics that governed inland trade have not seen the same level of interest. Within this book, Northern Italy serves as a case study to explore the role transport cost and consumer choice played in the distribution of local and imported goods throughout inland regions. Using three contrasting, quantified datasets of amphorae, finewares, and marble (together forming the Material Data in Northern Italy (MADINI) dataset), chronological and spatial patterns in inland trade are analysed using aoristic analysis and hierarchical clustering. The results demonstrate that inland trade was far more complex than a simple regression of imports as distance from the coast increased. Clear zones of consumption across Northern Italy are seen in the distribution of the material data, often closely linked to transport costs. While the river network is shown to have been a crucial in facilitating inland transport, the significance of trans-mountain trade across the Alps and the Apennines has been underestimated. Areas furthest inland are often shown to have had the greatest diversity in the provenance and types of material, as opposed to coastal areas which demonstrate a more limited selection. The results highlight the diverse array of factors governing inland trade and the interplay between cost and choice in the decisions made by consumers.

# Data
The data used here are restructured versions of the component databases of the MADINI dataset collected by James Page and openly available on Zenodo:

Page, J. The MADINI Dataset. Zenodo; 2024 [cited 2024 Sept 12]. Available [here](https://doi.org/10.5281/zenodo.13745898).

Please cite this resource when citing the original MADINI dataset.

The original datasets are in a matrix format and includes multiple tables. This has been restructured into a single table format to enable the data analysis in the study.

The datasets are available as .csv files:

AMINI database: [amphora.csv](https://github.com/jamespage15/Adriatic-to-the-Alps/blob/main/Data/amphora.csv)

REFINI database: [fineware.csv](https://github.com/jamespage15/Adriatic-to-the-Alps/blob/main/Data/fineware.csv)

DESTINI database: [stone.csv](https://github.com/jamespage15/Adriatic-to-the-Alps/blob/main/Data/stone.csv)

# Notebooks
The R notebooks used to generate the data analysis figures are stored in this repository.

Each notebook reproduces one figures, numbered in the same way as in the publication.

Each notebook is structured in the same way:

- Import packages
- Read in data
- Data analysis 
- Plot the graphs
