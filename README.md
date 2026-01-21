# Evolving Outcomes in Geriatric Patients with Orthopedic Polyfractures: the NTDB 2008-2023

by Anthony Peraza, B. Vittorio Resnick, Tim Wu, Jack Janett, Patrick Smith, Reginald T. A. Conley, Dana Hazem, Daniel Rhode, Meir T. Marmor, MD

## Question 

How do clinical outcomes in NTDB geriatric patients with orthopedic polyfractures evolve after the ACS Geriatric Trauma Management Guidelines?

## How to run the analysis 

This repo already has the .R scripts that you need to run our analytical pipeline. First, run 0_utilities.r to establish utilities; then 1_dataset_stitch_decode_purge.r to standardize the datasets, stitch, decode, and clean; then optionally, 1.5_ICD_decode.r to get a readout of each ICD-9 and ICD-10 fracture Dx code with their respective frequencies; then 2_tables.r to build Table 1 and Table 2; then 3_MCA to build Figure 2A; and then, 4_regression.r to run the main logistic regression analyses, generating Figures 2B, 2C, and 3. Be sure to check the pathnames at the beginning of each script under "Load data". The empty directories for output are included.

## Data availability

This repository contains analytic code only. NTDB data are not included due to data use restrictions.

## Contact

Direct questions to Anthony Peraza (anthony.peraza@ucsf.edu) or Vitto Resnick (kinzer@berkeley.edu)
