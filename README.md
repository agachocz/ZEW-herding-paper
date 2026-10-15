This repository contains the data and code used for the paper "Herding behaviour in sentiment formation: analysis of
the ZEW sentiment indicators for Eurozone"

macro data.R - loading and cleaning up macroeconomic variables.

data preparation.R - loading and cleaning ZEW indicators data, descriptive statistics and plots.

zew vs macro data analysis.R - initial VAR models with both ZEW and macro indicators, used for selecting the number of lags and variables used in the herding models.

herding model.R - functions for model estimation.

herding model estimation.R - estimation using the functions from the previous files. Main results for the paper are created here.
