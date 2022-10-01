# Should I Stay or Should I Go?

Here are all the files included in this GitHub Repo:

Data Folder - contains the 2015 AA run expectancy data I used as well as the indicator variables I created if the runner was safe or not. SMT Data was in this folder as well but was not allowed to be included in this repository

Graphics Folder - Contains all images as well as the GIFs that were included in the final document

rsconnect Folder - Contains Files for deploying Shiny App

Stay-or-Go-Shiny Folder - Contains Files for Creating Shiny App

Animate-Play.R - The original file I used to animate plays. This is the exact gif generator I used to watch all the plays

Ball-2-Base-Dist.R - Calculates the distance from the ball to the target base

Ball-2-OF.R - Created a dataset of all the plays where the ball was hit to the outfield and only touched by 1 player

Base-Running-Speed.R - Created dataset of the average speeds of all the runners. This is also where the outlier corrections occur as well

Billy-Fryer SMT-Data-Challenge-Poster.pdf - PDF Version of my Research Poster presented at the UConn Sports Analytics Symposium October 8, 2022 

BIP-Plays.R - Creates a lookup dataframe of all the plays where the ball was actually hit into play

BR-2-Base-Dist.R - Calculates distance from base runner to the target base

Change-In-RE.R - Function that calculates the change in run expectancy

Combine-Datasets.R - The way I set up this project was to have a lot of scripts that each produced a long data frame of game_str, play_id, and the desired metric. This R script combined those into 1 dataframe for me to model with.

Data Challenge Poster.ppt - Powerpoint file used to design final poster for UConn Sports Analytics Symposium

Data Challenge Poster.png - PNG Version of my Research Poster presented at the UConn Sports Analytics Symposium October 8, 2022 

Final-Paper.Rmd - R Markdown document that I used to create my final paper

Find-Baserunners-From-PBP.R -Found what baserunner (by player id) was on what base for a given game_str and play_id

Graphics.R - File that I used to make the majority of the graphics in my final paper

Inning-Finder.R - Gave the inning for a given play_id and game_str

Modeling.R - R script where I did all of my modeling

Other-Runners.R - R script that created the Game State for a given play_id and game_str

Player-Position-Lookup.R - R script that is used as a lookup dataframe to give the player id for any position, play_id and game_str

QR Code.png - QR Code that links to my website where paper, animated plays, and Shiny App are located

Position-At-Throw.R - Pulls player position at the exact time of throw to calculate distances between ball and base and player and base

Run-Expectancy.R - Calculates Run Expectancy from publicly availible data from MiLB API via baseballR package

Save-All-GIFs.R - The loop I used to create all the gifs I needed to watch and manually label

Shareable-GIFs.R - File I used to create the GIFs linked in the paper

SMT-Data-FINAL.Rproj - R Project I used for this project
