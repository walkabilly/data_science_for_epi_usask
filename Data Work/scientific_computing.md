---
title: "Scientific Computing"
author: "Daniel Fuller"
output:
      html_document:
        keep_md: true
---

## Why?

Frequently, research problems that use computing can outgrow the desktop or laptop computer where they started:

* A statistics student wants to cross-validate their model. This involves running the model 1000 times – but each run takes an hour. Running on their laptop will take over a month!
* A genomics researcher has been using small datasets of sequence data, but soon will be receiving a new type of sequencing data that is 10 times as large. It’s already challenging to open the datasets on their computer – analyzing these larger datasets will be extremely difficult if not impossible!
* An engineer is using a fluid dynamics package that has an option to run in parallel. So far, they haven’t used this option on their desktop, but in going from 2D to 3D simulations, simulation time has more than tripled and it might be useful to take advantage of that feature.

In all these cases, what is needed is access to more processing power. Luckily, large scale computing systems – shared computing resources with lots of computers – are available at many universities, labs, or through national networks. These resources usually have more central processing units(CPUs), CPUs that operate at higher speeds, more memory, more storage, and faster connections with other computer systems. They are frequently called “clusters”, “supercomputers” or resources for “high performance computing” or HPC. In this lesson, we will usually use the terminology of HPC and HPC cluster.

Using a cluster often has the following advantages for researchers:

* __Speed__. With many more CPU cores, often with higher performance specs, than a typical laptop or desktop, HPC systems can offer significant speed up.
* __Volume__. Many HPC systems have both the processing memory (RAM) and disk storage to handle very large amounts of data. Terabytes of RAM and petabytes of storage are available for research projects.
* __Efficiency__. Many HPC systems operate a pool of resources that are drawn on by many users. In most cases when the pool is large and diverse enough the resources on the system are used almost constantly.
* __Cost__. Bulk purchasing and government funding mean that the cost to the research community for using these systems in significantly less that it would be otherwise.
* __Convenience__. Maybe your calculations just take a long time to run or are otherwise inconvenient to run on your personal computer. There’s no need to tie up your own computer for hours when you can use someone else’s instead.

## Command Line

Using HPC systems often involves the use of a shell through a command line interface (CLI) and either specialized software or programming techniques. The shell is a program with the special role of having the job of running other programs rather than doing calculations or similar tasks itself. What the user types goes into the shell, which then figures out what commands to run and orders the computer to execute them. (Note that the shell is called “the shell” because it encloses the operating system in order to hide some of its complexity and make it simpler to interact with.) The most popular Unix shell is Bash, the Bourne Again SHell (so-called because it’s derived from a shell written by Stephen Bourne). Bash is the default shell on most modern implementations of Unix and in most packages that provide Unix-like tools for Windows.

Connecting to an HPC system is most often done through a tool known as “SSH” (Secure SHell) and usually SSH is run through a terminal. So, to begin using an HPC system we need to begin by opening a terminal. Different operating systems have different terminals, none of which are exactly the same in terms of their features and abilities while working on the operating system. When connected to the remote system the experience between terminals will be identical as each terminal is running on the same remote system.

## Graphical User Interface

There are lots of graphical user interface (GUI) options available for working with HPC or other servers (including things like Amazon Web Service, Google Cloud Storage, and Microsoft Azure). Personally, I use [FileZila](https://filezilla-project.org/) as it's a free and open source tool (with a pro upgrade available). These tools allow you to visulize what is happening in the file system but are often limited in terms of running the full range of commands you will need to work with the HPC. Generally, I tend to work with a mix of command line and GUI depending on the task. Other good options for GUIs are

* [Cyberduck](https://cyberduck.io/) 
* [CoreFTP](https://www.coreftp.com/)
* [WinSCP](https://winscp.net/eng/index.php)

## Connecting

SSH allows us to connect to UNIX computers remotely, and use them as if they were our own. The general syntax of the connection command follows the format `ssh yourUsername@some.computer.address` Let’s attempt to connect to the HPC system now

```{}
ssh nsid@plato.usask.ca
```

If you’ve connected successfully, you should see a prompt like the one below. This prompt is informative, and lets you grasp certain information at a glance. (If you don’t understand what these things are, don’t worry! We will cover things in depth as we explore the system further.)

```{}
[nsid@platolgn01 ~]$ 
```

You may have noticed that the prompt changed when you logged into the remote system using the terminal (if you logged in using PuTTY this will not apply because it does not offer a local terminal). This change is important because it makes it clear on which system the commands you type will be run when you pass them into the terminal. This change is also a small complication that we will need to navigate throughout the workshop. Exactly what is reported before the $ in the terminal when it is connected to the local system and the remote system will typically be different for every user. We still need to indicate which system we are entering commands on though so we will adopt the following convention:

* [local]$ when the command is to be entered on a terminal connected to your local computer
* [nsid@platolgn01 ~]$ when the command is to be entered on a terminal connected to the remote system
* $ when it really doesn’t matter which system the terminal is connected to.

### Working in multiple places

It is strongly recommended that you have two terminals open, one connected to the local system and one connected to the remote system, that you can switch back and forth between. If you only use one terminal window then you will need to reconnect to the remote system using one of the methods above when you see a change from `[local]$` to `[nsid@platolgn01 ~]$` and disconnect when you see the reverse.

# Basic commands

* `ls`: prints the names of the files and directories in the current directory in alphabetical order, arranged neatly into columns.
* `cd`: change directory. Example: `$ cd /globalhome/yourUserName/HPC`
* `mkdir`: specifies the name of the directory you wish to create.
* `~`:  is a shortcut that represents your home directory
* `.`: represents your current directory
* `..`: represents the “parent” directory of your current location

Many commands also have multiple behaviours that you can invoke with optional command line ‘flags.’ What is a flag? It’s generally just your command followed by a `-` and the name of the flag (sometimes it’s `--` followed by the name of the flag). You follow the flag(s) with any additional arguments you might need.

* `-a`: show hidden files.
* `-l`: show files, their size in bytes, date last modified, permissions.
* `-R`: lists the contents of directories recursively, i.e., lists their sub-directories, sub-sub-directories, and so on.
* `-t`: lists things by time of last change, with most recently changed files or directories first.

A quick note on the structure of a UNIX (Linux/Mac/Android/Solaris/etc) filesystem. Directories and absolute paths (i.e. exact position in the system) are always prefixed with a `/`. `/` by itself is the “root” or base directory. While typing nearly anything, you can have bash try to autocomplete what you are typing by pressing the `tab` key.

# Looking at and modifying files

There are resources for opening and modifying text files [here](https://cbe453.github.io/arc-plato-shell/03-files/index.html). In reality it's unlikely you will be doing this much. More likely that you will be opening files from a text editor or something on your local computer. The one thing that you might need to to do is go grab and unzip a large file on the HPC using the command line. This can be done directly in your `r` code (more on this later) or using existing commands. 

If I need to move stuff around or delete files I tend to do that in Filezilla rather than the command line. Just easier.

#### wget

wget link downloads a file from a link.

```{}
$ wget https://cbe453.github.io/arc-plato-shell/files/bash-lesson.tar.gz
```

#### curl

cURL is the command-line interface to `libcurl`, a powerful library for programming interactions with remote resources over a wide variety of network protocols. You can use `cURL` as an alternative 

```{}
$ curl -O https://cbe453.github.io/arc-plato-shell/files/bash-lesson.tar.gz
```

#### unzipping

* __gunzip__: extracts the contents of `.gz` files
* __unzip__: extracts the contents of `.zip` files
* __tar -xvf__: extracts the contents of `.tar.gz` and `.tar.bz2` files

# Permissions

It's often the case that file permissions will need to be changed. This depends on the cluster and setup. In Digital Alliance Canada the person who creates the file is the owner and the file is not necessarily shareable with other people. GUI interfaces tend to not have full support for changing file permissions so it's important to be able to do this online. 

```{}
$ ls -l
```

That’s a huge amount of output: a full listing of everything in the directory. Let’s see if we can understand what each field of a given row represents, working left to right.

* __Permissions__: On the very left side, there is a string of the characters `d`, `r`, `w`, `x`, and `-`. The d indicates if something is a directory (there is a `-` in that spot if it is not a directory). The other `r`, `w`, `x` bits indicate permission to Read, Write, and eXecute a file. There are three fields of `rwx` permissions following the spot for `d`. If a user is missing a permission to do something, it’s indicated by a   -.
    * The first set of `rwx` are the permissions that the owner has (in this case the owner is yourUsername).
    * The second set of rwxs are permissions that other members of the owner’s group share (in this case, the group is named yourUsername).
    * The third set of rwxs are permissions that anyone else with access to this computer can do with a file. Though files are typically created with read permissions for everyone, typically the permissions on your home directory prevent others from being able to access the file in the first place.
* __References__: This counts the number of references (hard links) to the item (file, folder, symbolic link or “shortcut”).
* __Owner__: This is the username of the user who owns the file. Their permissions are indicated in the first permissions field.
* __Group__: This is the user group of the user who owns the file. Members of this user group have permissions indicated in the second permissions field.
* __Size of item__: This is the number of bytes in a file, or the number of filesystem blocks occupied by the contents of a folder. (We can use the -h option here to get a human-readable file size in megabytes, gigabytes, etc.)
* __Time last modified__: This is the last time the file was modified.
* __Filename__: This is the filename.


| Letters |	Definition |
| ------- | ---------- |
| ‘r’ |	“read” the file’s contents. |
| ‘w’ |	“write”, or modify, the file’s contents. |
|‘x’  |	“execute” the file. This permission is given only if the file is a program.|


So how do we change permissions? As I mentioned earlier, we need permission to execute our script. Changing permissions is done with `chmod`. 

| Operators |	Definition |
| --------- | ---------- |
| `+` |	Add permissions |
| `-` |	Remove permissions |
| `=` |	Set the permissions to the specified values |


If you want to give “execute” permission to the world (“group”) for file “demo.txt”, you will start by typing. 


```{}
chmod g
```

Now you would type a ‘+’ to say that you are “adding” permission. 

```{}
chmod g+
```

Then you would type an ‘x’ to say that you are adding “execute” permission. 

```{}
chmod g+x
```

Finally, specify which file you are changing.  

```{}
chmod g+x demo.txt
```

Change permissions so all users can execute a file: 

```{}
$ chmod +x demo.txt
$ ls -l
```

# Environment and R 

You need to setup your environment in the HPC before you do analysis. The environment is basically what the HPC will install for you to work with. You need to pick the right version of R and other software to make this work properly. Same as on your local computer. 

List available versions of R on the cluster.

```{}
module spider r
```

Then, look at the version you want specifically to see if there are any other modules you need to load in order to run it. For example, if I want to use R 4.4.0, I would run:
    
```{}
module spider r/4.4.0
```
    
This will show any other modules & versions you need to load before you can run R.

```{}
module load StdEnv/2023 r/4.4.0
```

This loads package required to run R version 4.4.0, then loads R. It is important that they be in the correct order (StdEnv/2023 before r).

```{}
R
```

This opens an interactive R session.

```{}
install.packages("tidyverse")
library(tidyverse)
```

From here we can run our interactive session like we would normally do... just with a much much faster machine. It's best to develop your code locally on a smaller dataset then run here as doing simple code development on the cluster is not really that helpful. 


```{}
data <- read_csv("/accel_data_no_id.csv")
mean(data$y_axis)
```

Close the interactive R session.

```{}
q()
```

If you try installing packages and receive error messages about specific modules needing to be loaded, check which versions you need with module spider and add them to the job script. For example, to use the package `raster`, you need to load modules for `gdal` and `proj.`

# SLURM 

Slurm is an open source, fault-tolerant, and highly scalable cluster management and job scheduling system for large and small Linux clusters. Slurm requires no kernel modifications for its operation and is relatively self-contained. As a cluster workload manager, Slurm has three key functions. First, it allocates exclusive and/or non-exclusive access to resources (compute nodes) to users for some duration of time so they can perform work. Second, it provides a framework for starting, executing, and monitoring work (normally a parallel job) on the set of allocated nodes. Finally, it arbitrates contention for resources by managing a queue of pending work. Official documentation for [SLURM here](https://slurm.schedmd.com/documentation.html). 

__TLDR__ SLURM lets you schedule jobs on the HPC. You do this with a job script file that contains information about the job you want to submit and a path to the code you want to run. 

### Serial Job

```{}
#!/bin/bash

#SBATCH --time=01:00:00             # How much time your program needs
#SBATCH --job-name="Test SLURM"     # A name for the job. Helps with error handling
#SBATCH --mem=10GB                  # Amount of memory to use
#SBATCH --output=Test.out           # A name for the output file

cd /project/chep_fuller             # Edit to where the code and data live

module load StdEnv/2023 r/4.4.0 

srun Rscript testRun.R
```

This example illustrates the typical structure of a job script:

1. Choose scheduler options: `#SBATCH` instructions
2. Set the software environment: `module load` commands
3. Perform the computation: commands prefixed with `srun`

## Other types of jobs 

Resources here to help with what your job script file needs depending the type of job [https://wiki.usask.ca/display/ARC/Plato+SLURM+job+scripts](https://wiki.usask.ca/display/ARC/Plato+SLURM+job+scripts).

### How do I know how much time I need?

Different HPCs have different time limits, ranging from 24 hours to 28 days. If you can, try running your script on a subset of data and seeing what it might scale up to (while being aware that not all processes scale linearly!)

### How do I know how much memory I need?

* Start large (at least 4GB) on a test script. Then
    * While the script is running, check how much memory is used in real time by typing `sstat yourjobID.batch --format="JobID,MaxRSS"`
    * When the script is done running, check how much was used by typing `sacct -o MaxRSS -j yourjobID`
    * If you check the `slurm.out` file and you’re getting `“oom-kill”` errors, you need to request more memory
    * If you’re using less than you asked for, it’s beneficial to reduce the memory in `--mem` or `--mem-per-cpu` (this way your job will get scheduled sooner)
    * Resubmit your job with your new estimate.

### How do I know how many CPUs I need?

* If you didn’t explicitly do anything in your code to make your script run in parallel, it is set up to run on 1 core.
* If you have, then you can request many cores. “How many?” It depends, how many do you need to run your script?

## Submit the job

We have all the information we need in our shell script. Now we just need to submit it to the scheduler. In Terminal, while in your user folder in the login node, write the following command:


```{}
sbatch regularTestRun.slurm ### File extension are arbitrary (every is text)
```

Now it’s out of your hands! The scheduler will see how many resources you are asking for, and try to slot in your job whenever there are some compute nodes available. You can check up on your job by typing `squeue -u yourusename` in Terminal.

You can check any outputs from the R console in the slurm-yourjobID.out file. Anything that you put in a `print()` command in your R script should appear in the `Test.out` file, which can help with debugging!

# Resources

1. How to run your r scripts on Digital Alliance Canada. Julie Fortin. __Note. a bit old now so don't use the same versions of things__. [https://medium.com/the-nature-of-food/how-to-run-your-r-script-with-compute-canada-c325c0ab2973](https://medium.com/the-nature-of-food/how-to-run-your-r-script-with-compute-canada-c325c0ab2973)

### Plato
1. Introduction to Using the Shell in a High-Performance Computing Context. Connor Burbridge. [https://cbe453.github.io/arc-plato-shell/00-hpc-intro/index.html](https://cbe453.github.io/arc-plato-shell/00-hpc-intro/index.html) and 
2. Plato SLURM job scripts. Olivier Fisset. [https://wiki.usask.ca/display/ARC/Plato+SLURM+job+scripts](https://wiki.usask.ca/display/ARC/Plato+SLURM+job+scripts)

### Digital Alliance Canada
1. Software [https://docs.alliancecan.ca/wiki/Available_software](https://docs.alliancecan.ca/wiki/Available_software)
2. Linux intro [https://docs.alliancecan.ca/wiki/Linux_introduction](https://docs.alliancecan.ca/wiki/Linux_introduction)
3. SSH [https://docs.alliancecan.ca/wiki/SSH](https://docs.alliancecan.ca/wiki/SSH)
4. Standard Software environments: [https://docs.alliancecan.ca/wiki/Standard_software_environments](https://docs.alliancecan.ca/wiki/Standard_software_environments)
5. Running Jobs: [https://docs.alliancecan.ca/wiki/Running_jobs](https://docs.alliancecan.ca/wiki/Running_jobs)
6. Machine Learning: [https://docs.alliancecan.ca/wiki/AI_and_Machine_Learning](https://docs.alliancecan.ca/wiki/AI_and_Machine_Learning)
7. R script: [https://docs.alliancecan.ca/wiki/R](https://docs.alliancecan.ca/wiki/R)
