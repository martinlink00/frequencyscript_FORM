# Kapitza Script
The scripts in this directory use the method of modulated Fourier expansion to calculate the slow frequency of an inverted or Kapitza pendulum in an iterative calculation using Mathematica 12.1.
# How to use the scripts
  - Input the highest non-trivial correction you wish to calculate into the params.txt file
  - Run main script by typing "math -script freq.wls" or "math12 -script freq.wls" in the project folder
  - Results as well as logfiles will be saved into a subdirectory exports, which will be automatically created when the program is first run
# Results
  - View results using the Results.nb notebook in the project folder (instructions on usage given there)
  - Each new run of either one of the scripts will overwrite previous results!
    - If you wish to keep results, copy the exports directory and Results.nb somewhere else to avoid risk of loosing everything 
# Estimated time
  - If you wish to use the available logfiles in exports/logs/ to predict upcoming calculations on your setup, you can run paramest.wls
  - Note that this script will only analyse the available logfiles, and will only be of use if the program has run at least once for 2 - 3 different orders
  - In order to run the main script for orders 2, 3 and 4, run the bash script run234.bash
