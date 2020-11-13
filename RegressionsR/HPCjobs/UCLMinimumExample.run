#!/bin/bash -l

# Example jobscript to run a single core R job

# Request ten minutes of wallclock time (format hours:minutes:seconds).
# Change this to suit your requirements.
#$ -l h_rt=20:10:0

# Request 1 gigabyte of RAM. Change this to suit your requirements.
#$ -l mem=1G

# Set the name of the job. You can change this if you wish.
#$ -N R_job_1

# Set the working directory to somewhere in your scratch space.  This is
# necessary because the compute nodes cannot write to your $HOME
# NOTE: this directory must exist.
# Replace "<your_UCL_id>" with your UCL user ID
#$ -wd /home/ucacjxu/Scratch/R_output

# Your work must be done in $TMPDIR (serial jobs particularly) 
cd $TMPDIR

# Load the R module and run your R program
module unload compilers
module unload mpi
module load r/recommended

R --no-save < /home/jxu/TradingBehavior/RegressionsR/HPCjobs/UCLMinimumExample.R > UCLMinimumExample.out
