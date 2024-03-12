# Visual-spatial dynamics drive adaptive social learning in immersive environments

This repository contains all data and code for running the experiment, analyzing the data, and the Unity simulations for computing the visual field transcription. 

Wu, C. M., Deffner, D., Kahl, B., Meder, B., Ho, M. H., & Kurvers, R. H. J. M. (2023). Visual-spatial dynamics drive adaptive social learning in immersive environments. BioRxiv. https://doi.org/10.1101/2023.06.28.546887


## Directory structure


* `analysis/`: contains the behavioral analyses, including network analyses, temporal dynamics, and pull detection
* `modeling/`: contains code for running and analyzing the computational models
* `simulations/`: code used for agent-based simulations
* `images/`: contains a minecraft sprite used in some of the visualizations. Microsoft please don't come after us 
* `environments/`: contains code used to generate the reward environments
* `experiment/`: contains the code for running the experiment, which is implemented as a Minecraft mod
* `visibility/`: contains the Unity project used for visual field transcriptions

## Analyses

This folder contains participant data and the code used to perform behavioral analyses

The data folders are:
* `data/2021batch` contains all the participant data, which are summarized in `all_blocks_pd_cache.feather` describing all destruction events and `all_players_pd_cache.feather` describing player states at a 20hz sampling rate. The latter file is stored using `git lfs`, since it is larger than the 50mb limit on github. The other csv files are the raw logs
* `simData` contains the outputs of the Unity simulations used to automate the transcription of visual field data. `pvisDF.Rds` contains player visibility events, while `evisDF.Rds` contains all other transcribed events (e.g., splash indicating social reward observation)

The main analyses reported in the paper are based on:
* `behavior.R` contains all analyses of reward, foraging rate, spatial distance, and other basic analyses of behavior. Regression models are saved in `brmsModels/`, which have not been included to avoid this repository getting too big. 
* `networkAnalysis.R` contains the spatial and visibility network analyses. Intermediate outputs are saved in `trajectories` (not included due to size) and `networks` 
* `individualDynamics.R` and `temporalDynamics.R` are used together to conduct the dynamic analyses, with intermediate outputs saved in `dynamicData`
* `pullAnalysis.R` relies on `extract_pulls.Strandburg-Peshkin.r` with intermediate outputs saved in `sequences` and `events`
* `illustrativeExamples.R` visualize some illustrative behavior from the task for Figure 1
* `rewardTimes.R` are used to compute individual and socially observed reward times, which are used in the computational models. 
* `utilities.R` contains some useful functions for saving brms models
* `statisticalTests.R` contains code used to perform stats

## Modeling

This folder contains the code used to perform computational modeling

* `compileModelData.py` is used to compute the model features for all participant data. This is supported by `featureFunctions.py` where model features are defined and `dataprocessing.py` for attaching behavioral data. Model features are saved in `data/modelFeatures` for each participant.
* `STAN/` contains the various computational models implemented in STAN. These models are initialized via `Stan_prep_run.R` and summaries of the model weights are in `Stanfits/`
* `modelPlots.R` plots the model comparison and analyses model weights. The PXP analysis is performed in `PXP.ipynb` and is supported by `bms.py`
* `adaptivityPerf.R` relates model weights to various behavioral outcomes
* `visualizeModelFeatures.R` provides a visualization of model features 


## Simulations
Agent-based simulations are computed using a simplified version of the task using similar feature functions as in the the computational models.

* `task.py` describes the basic task environment
* `spatial_gp.py` contains the code for the reward prediction feature
* `simulation.py` provides the framework for agent-based simulations, which is then run in `runSimulations.py`
* `plotSimulations.R` finally visualizes the results

## Environments
This folder contains the environments used in the experiment and the code used to generate them.
* `smoothEnvGenerator.R` generated the smooth environments
* `randomEnvgeneration.R` generated the random environments
* `PlotEnvs.r` visualizes the environments, which are individually plotted in `plots/` and summarized in `Environments.pdf`
* `Trialstructure.R` is used to generate the counter-balanced ordering across sessions

## Visual field transcription

The visual-field simulator is a standard Unity project using Unity version 2019.3.0f5 with no additional dependencies.

### Usage

List the blocks and player files you want to simulate in the `visibility/blocks_paths.txt` and `visibility/players_paths.txt` files respectively. Make sure to list them as paths relative to the root directory of the git repository (not the Unity project). Likewise, make sure the listed blocks and players files match each other correctly.

Afterwards, open the Unity project, run the scene `VFSScene` and click on "Run". The application will now go through the list and generate a `evis` and `pvis` file for each round. These contain data for the visibility of events and players respectively.

If you want to view the simulated visual fields as they are being generated, you can enable them in the Unity editor under `Canvas > Perspectives` (in the VFSScene). If you want the colors of the entities to be more distinct, you can increase the `CER_id_multiplier` on the `Root` script. If you want the simulation to run in real-time (as opposed to as fast as possible), you can toggle the `realTime` field of the `Director_Generate` script.

### Source Code

The project is comprised of two distinct scenes. 

The `Reference_test` scene merely has the purpose of ensuring that the Unity-simulated particle effect from reward-splashes matches the native minecraft one as closely as possible. You can press spacebar to start a video recording of a minecraft splash effect overlayed by a simulated one.

The `VFSScene` is the scene that actually simulates the visual fields for a round and generates the respective data. Each simulated object (player, splash, block etc.) is represented by a `ColorEncodedRenderer` (CER) script, which is assigned a unique ID number that it then derives its color from.

The entire data-generation process is overseen by a `Director_Process` script, which creates a new `Director_Generate` instance for each round that is simulated. The latter goes through the corresponding players file and, for each timestep, places the players in their recorded positions, removes any blocks that were broken since the previous step and updates each splash effect.
Each of the player-objects (P1-4) contain a snapshot-camera than renders that player's perspective into a distinct render texture. After the game-state has been updated, each player's perspective is rendered and the resulting texture is retrieved into CPU memory, where the number of occurences (ie. pixels) of each color (any by extension CER) is counted and logged.

## Experiment

This folder contains the code used to implement the experiment, which we called the producerScrounger Minecraft Mod.The mod runs on Minecraft Java Edition ver. 1.12.2 using Forge ver. 14.23.5.2847. NOTE: This software utilizes a deprecated version of Java and may be subject to unpatched security vulnerabilities. Do not run it over a public network.

- `/logging` Folder: The logging folder contained the raw data-logs, which we have moved to the `analysis/` folder to avoid duplication. Some of these logs had falsely recorded timestamps, which we corrected using the `fix_time.py` python script, using the static Minecraft tickrate of 20Hz. There is also a python script to generate a top-down video of any round given to it. The resulting video will look like [this](https://www.youtube.com/watch?v=vUHaAhjfFVo&ab_channel=BenjaminKahl).
- ´/build/libs´ Folder: Contains the jar-files for every major revision of the mod. See below for instructions on how to install or modify it.
- ´/src´ Folder: Contains the source code for the newest mod version. See below for further documentation.

### Running the experiment

#### Client-Side

To set up and run the experiment client-side, follow these instructions:

- Make sure to have an installation of Minecraft Java Edition 1.12.2 and to have launched it at least once.
- Make sure to have installed [Java Runtime Environment](https://www.oracle.com/java/technologies/downloads/)
- Head to [the forge website](https://files.minecraftforge.net/net/minecraftforge/forge/index_1.12.2.html) and download the installer for Forge version 14.23.5.2847. Newer versions are likely to work just as well, so long as their number starts with 14.3.5.*.
- Running the installer should produce a prompt, allowing you to select "Install client".
- Locate your Minecraft installation folder. (Typically `C:\Users\(...)\AppData\Roaming\.minecraft`).
- On this repository, go to `build/libs/1.2.4/client` (Choose another sub-folder if you want to run an older version of the mod) and copy its contents into the installation folder located in the previous step.

In you Minecraft Launcher you should now have an installation entry labeled `1.12.2-forge1.12.2-14.23.5.2847`. When you launch it you should get into a Minecraft instance with a "Forage Mod" entry under "Mods".
You are now ready to connect this instance to a Forage-Mod server.

#### Server-Side

To create a server, follow the steps of the previous section, but select "Install server" on the forge prompt, installing it into an empty directory.

Then, launch the generated server .jar-file once, which will generate a file `eula.txt`. Edit the file's contents to set `eula=false` to `eula=true`. With this action you confirm reading and accepting the Mojang End User License Agreement.

Launch the server jar anew and let generate the necessary server files. Once it is done, close the server.

Head to `build/libs/1.2.4/server` and copy its contents into the server directory. Overwrite any duplicate files. Lastly, delete the `world/` directory.

You can now launch the server and connect to it. Depending on your machine, the launch may require special commandline parameters, like `java -Xms1G -Xmx3G
-jar forge-1.12.2-14.23.5.2847-universal.jar`.

#### Web-Interface

Once your server is running, you can also launch a web-interface to manage it. Simply head over to the `web_api` folder of your server directory and use Python to launch the `run.py` script.

This python script will run a simple HTML server on port 8000, through which you can manage and supervise the server instance.

#### Developer Setup

If you want to re-compile the mods built jar-files with your own changes, you will need to setup a developer environment.

It might be helpful to read up on the forge documentation: https://docs.minecraftforge.net/en/fg-6.x/gettingstarted/

To set up a dev environment, follow these steps:

- Download and install the Java 1.8 jdk *and* jre from [here](https://www.oracle.com/java/technologies/javase/javase8-archive-downloads.html)
- Download & extract Eclipse Neon 3, (other versions don't always work: https://www.eclipse.org/downloads/packages/release/neon/3
- Go [to the forge website](https://files.minecraftforge.net/net/minecraftforge/forge/index_1.12.2.html), from version 14.23.5.2847 download the "Mdk"
- Extract the downloaded file into the working directory you intend to work in. (Moving it later will break things)
- Open a terminal, cd to the directory and run ´./gradlew setupDecompWorkspace´ and ´./gradlew eclipse´.
- Open Eclipse and set the working directory to ´/eclipse´ inside the directory you extracted forge into.
- Hit play and see if Minecraft starts. If you get a $project_loc variable error, make sure the project on the left (explorer) *is selected*. If everything works, close eclipse again.
- Copy over and overwrite the files in your working directory with the ones from this repo.

### Source Code

The mod consists of several components working alongside each other.

### Gamemaster

Settings and parameters are parsed from the `forage_settings.txt` file and utilized to construct the experiment. The core Java classes are located in the `forage/gamemaster` folder. The experiment takes the form of an event-queue, which is processed first-to-last. The different event-types are located in `gamemaster`/events`.

The `L.java` class serves as a source for correctly localized instructions.

The `WebAPI.java` class serves to execute commands passed by the web-interface manager.

`PlayerScoreTracker.java` keepts track of everyones score, whilst `PlayerVisTracker.java` keepts track of each players mutual visibility. Note that the visibility data provided by this class is not particularly accurate, and becomes redundant if the Unity Simulations are used instead.

### Structures

To construct the experiment from the data in `forage_settings.txt`, certain structures (tutorial, lobby, field etc.) need to be defined.

For complex structures like the turorial or the lobbies, we utilize Minecraft's own NBT file format, which let us easily modify and adjust structures through the use of *structure blocks*.
You can view these structure blocks in-game by enabling the `dev=true` field in the forage settings file.

The only meaningful structure that does not use the NBT format is the melon/pumpkin field. These are automatically constructed each round based on the parameters in forage_settings as well as the corresponding environment.csv file.

The field consists of a layer of brown wool, framed by a fence and custom melon (or pumpkin) blocks placed at regular intervals.
These blocks are defined in `forage/blocks`. Their only specialty is that they spawn a splash-effect and contain a reward when the block underneath them is powered by redstone.

### Commands

There are several developer commands that interact with the mod's code. A full list can be found in `forage/commands`.


The two most important ones are:

- `setup [session-name]`: Will set up a new session based on the json-file with the given name. For example, `setup session7_ge`, will set up and launch the german version of session 7.
- `egame`: Will end the current session.


