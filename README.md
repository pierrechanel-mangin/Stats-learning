# Road Safety project

Repository for our Advanced Statistical Learning project stats learning project - Jordan & Pierre-Chanel. There are multiple folders for this repo:

-   **raw_data**: unprocessed data received from professor and external sources

-   **processed_data**: Cleaned versions of raw_data that is used in modelling tasks

-   **scripts**: Multiple scripts for cleaning, modelling, ranking

    -   cleaning\_\*.R: used for data cleaning of raw_datasets

    -   modelling_setup.R: Used for set up before modelling in others scripts

    -   onestage_modelling.R: Modelling using only a single stage (for comparison)

    -   stage_one.R: modelling of risk-free and risky intersections using double CV and gain matrix

    -   stage_two.R: modelling of number of accidents for risky intersections

    -   ranking.R: Generate ranking based on results from two stages

-   **output**: graphics, modelling objects and predictions

-   references: Miscellaneous extra information regarding datasets and project.
