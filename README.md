# THESIS GITHUB
## Eden Forbes

In this repository are all the files necessary to run the analyses I conducted in my thesis project. Below are some quick notes on how to navigate these files:
1) The brain-tokyo-workshop folder is an adaptation of the original WANN code tailored to this project. I have tried to remove the files that are not relevant to this project, leaving behind just those relevant to the bipedal walkers. The original Git repo can be found here: https://github.com/google/brain-tokyo-workshop 
2) The R project, file, and data structure are all that is necessary to recreate the analyses from the model in Chapter 5. 
3) Scripts contains the batch scripts used to run data collection on Vassar's computer cluster Hopper. Additionally, it contains two jupyter notebooks used to collect information about the structures of the resulting WANNs (WANNNotebook.ipynb) and simulate realistic signals through the WANNs (WANNSignal.ipynb). In practice, these need to be included in the folder of a specific walker in WALKER_LOG, described further below. 
4) WALKER_LOG catalogues all the actual WANNs used in the dataset. The five are demarcated by date and additional videos of failed training attempts are included for reference. Each of the walkers' folders contains terminal logs of the training and tuning processes (Bipedal_Tester.out & Bipedal_Tuner.out), a folder with the resulting best network (log), a folder with the resulting best tuned network and tuning history (log_tool), network adjacency data (adjacency_....csv), network activity from artificially created signals (activity_....csv), network activity from a simulated walk as well as the simulated inputs and outputs used to create it (walk_....csv; sim_input.txt, sim_output.txt), other network information (info_....csv), and a video of the walker in action (...-SR).


