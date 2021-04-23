#!/bin/bash
#SBATCH -J Bipedal-Tuner
#SBATCH -o Bipedal-Tuner.out
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=128
#SBATCH -p emc
#SBATCH -t 48:00:00
#SBATCH --mail-user=eforbes@vassar.edu
#SBATCH --mail-type=begin
#SBATCH --mail-type=end

cd /home/eforbes/WANN/brain-tokyo-workshop/WANNRelease/WANNTool
python3 train.py biped -e 4 -n 3 -t 120 -o cma --sigma_init 0.5
