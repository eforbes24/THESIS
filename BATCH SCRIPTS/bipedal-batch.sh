#!/bin/bash
#SBATCH -J Bipedal-Tester
#SBATCH -o Bipedal-Tester.out
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=48
#SBATCH -p general
#SBATCH -t 96:00:00
#SBATCH --mail-user=eforbes@vassar.edu
#SBATCH --mail-type=begin
#SBATCH --mail-type=end

cd /home/eforbes/WANN/brain-tokyo-workshop/WANNRelease/prettyNeatWann
python3 wann_train.py -p p/biped.json -n 95
