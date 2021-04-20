# Risk-Benefit and Genomic Epidemiology Intenship at Technical University of Denmark (DTU) 👩💻

This repository includes the data and the analysis I performed during my master dissertation project in the risk-benefit and genomic epidemiology units at DTU. This work was developed during 4 months, between January and May 2021.

### Classic QMRA
#### Data  📔
<ul>
  <li><a href = 'https://github.com/Raquel-Costa/intern_dtu/blob/main/classic_QMRA/serra_da_estrela_cheese.xlsx'>Serra da Estrela Cheese</a> - data regarding the product at retail level and consumer habits</li>
  <li><a href='https://github.com/Raquel-Costa/intern_dtu/blob/main/classic_QMRA/population_pt.csv'>Population in Portugal between 2012 and 2020</a> - data of the number of portuguese population by year and by age</li>
</ul>


#### Analysis Performed 🕵
<ul>
  <li><a href = 'https://github.com/Raquel-Costa/intern_dtu/blob/main/classic_QMRA/gQMRA.R'>Classic QMRA</a> - QMRA performed on Serra da Estrela cheese data, based on <a href = 'https://github.com/Raquel-Costa/intern_dtu/tree/main/classic_QMRA/original_EFSA'>EFSA QMRA for L.monocytogenes in RTE food products (gQMRA)</a></li>
  <li><a href='https://github.com/Raquel-Costa/intern_dtu/blob/main/classic_QMRA/population_pt.R'>Population in Portugal between 2012 and 2020</a> - analysis to get the average yearly portuguese population by age group</li>
</ul>

<br>

### QMRA based on genomic data
#### Data  📔
<ul>
  <li><a href = 'https://github.com/Raquel-Costa/intern_dtu/blob/main/genomic_data_QMRA/genomic_data.xlsx'>Cheese WGS samples</a> - whole genome sequencing data and additional information from cheese samples obtained from NCBI database and ENA browser. The excel file contains multiple sheets:</li>
  <ul>
    <li>wgs_cheese_related – cheese, environmental and multi-ingredient samples available (all related to cheese). All samples from environment and multi-ingredient are marked in yellow</li>
    <li>wgs_cheese – cheese only samples available (the samples marked in yellow were eliminated). Samples with the sample number (biosample_ss) in red are a part of more than one project (bioproject_s). Samples marked in blue have more than one run number associated to them and all of the run numbers are present in the database</li>
    <li>wgs_data_download – samples downloaded to computerome as for the samples in red, one project data out of the multiple projects the sample was in was used and for the samples marked in blue only one of the run numbers was used (when posible the choice was made based on quality data present in the data base). In pink are the samples were assembly could not be done</li>
    <li>assembly_qc - quality control metrics after assembly. Marked in yellow are the samples with more than 500 contigs</li>
    <li>assembly_high_qc- assembled samples with less than 500 contigs</li>
    <li>assembly_low_qc – assembled samples with more than 500 contigs. Marked in pink are the samples with characteristics that may not be present in the high quality samples</li>
    <li>metadata_assembly_samples – auxiliar data regarding all the assembled samples</li>
    <li>metadata_samples_with_mlst - auxiliar data regarding the samples with MLST complete</li>
    <li>metadata_plus_clinical - auxiliar data regarding the samples with MLST complete plus the auxiliar data regarding the samples with clinical outcome associated and with MLST complete. This will be used on PHYLOViZ Online to build the phylogenetic tree</li>
    <li>prokka_roary - samples where prokka and roary where performed without any issues</li>
  </ul> 
  
  <li><a href = 'https://github.com/Raquel-Costa/intern_dtu/blob/main/genomic_data_QMRA/metadata.txt'>Metadata</a> - auxiliar data used to build the phylogenetic trees</li>
  

</ul>


  
#### Analysis Performed 🕵

<ul>
  <li>Bioinformatics:</li>
  
  <ul>
    <li><a href = 'https://github.com/Raquel-Costa/intern_dtu/blob/main/genomic_data_QMRA/mlst_complete.txt'>MLST results</a> - results from the 7 gene MLST both in the NCBI samples and the samples with the clinical outcome associated</li>

  <li><a href = 'https://github.com/Raquel-Costa/intern_dtu/tree/main/genomic_data_QMRA'>*.Rtab</a> - multiple files obtained in the Roary output</li>
  
  <li><a href = 'https://github.com/Raquel-Costa/intern_dtu/blob/main/genomic_data_QMRA/roary_plots.R'>Roary plots</a> - an R script where the *.Rtab files were used to create plots</li>
    
  </ul>
  
  <li>Machine Learning</li>
  
  <ul>
  
  </ul>
  
</ul>
  
  
