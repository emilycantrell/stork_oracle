#!/bin/bash

#SBATCH --job-name stork_sub
#SBATCH -p comp_env
#SBATCH -t 48:00:00
#SBATCH --ntasks 1
#SBATCH --cpus-per-task 72
#SBATCH -e %x-%j.err 
#SBATCH -o %x-%j.out

module purge
module load 2023
module load R-bundle-CRAN/2023.12-foss-2023a
module load R/4.3.2-gfbf-2023a

export SOURCE_DIR=/gpfs/ostor/ossc9469/homedir
export DATA_DIR=${SOURCE_DIR}/cbs_raw/Other
export REPO_DIR=${SOURCE_DIR}/PreFer/pmt/repositories/stork_oracle_with_live_in_partner_for_OSSC_2024-10-28
export SAMPLING_DIR=${SOURCE_DIR}/PreFer/pmt/data/train_and_eval_samples/

cd ${REPO_DIR}

Rscript --vanilla run_all.R ${DATA_DIR}/train.csv ${SOURCE_DIR}/cbs_raw/Bevolking/GBAPERSOONTAB/2020/GBAPERSOON2020TABV3.csv ${SOURCE_DIR}/cbs_raw/Bevolking/GBAHUISHOUDENSBUS/GBAHUISHOUDENS2020BUSV1.csv ${SOURCE_DIR}/cbs_raw/Bevolking/FAMILIENETWERKTAB/FAMILIENETWERK2020TABV1.csv ${DATA_DIR}/Codebook_UPD.xlsx ${SAMPLING_DIR}