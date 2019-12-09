# MS&E252 Case Study

## Opening the app
1. Open RStudio, type the following commands in the console:

```bash
library(shiny)
runGitHub( "MS-E252_Case_Study", "AlexaYuqinD")
```

2. The other way is to download the files and open app.R under
the decision tool folder in RStudio, then click on the "Run App" button.

## Decision Analysis
First, select the availability of data and spores respectively. There are 4 scenar-
ios:

**1. Data not available, Spores not available**

In this case, the app will directly recommend harvest decisions and calcu-
late the certain equivalent of harvest immediately and not harvest imme-
diately respectively.

The user can also change the probability of warm rain and risk tolerance
by sliding the sliders and see how the decision changes.

**2. Data not available, Spores available**

In this case, the app will recommend the decision on whether to harvest
immediately as well as whether to buy spores. It also calculates the value
of getting spores and the certain equivalent of two harvest decisions.
The user can also change the unfixed parameters to see how the decision
changes.

**3. Data available, Spores not available**

When only data is available, the user can choose a prediction model from
logistic regression, random forest and neural networks.

Then, they can upload input data file with the name "input data.csv". In
the submitted files, we provide an example of input data file. The user
can either directly use that file or change the data if they'd like to.
The selected model will then give predictions on whether there will be
warm rain based on the input data.

The app will recommend decision on whether to buy data based on the
performance of the selected model trained with the data to be bought, as
well as the harvest decision. It will also calculate the value of getting the data and the certain equivalent of two harvest decisions. If no data file is uploaded, the decision analysis will be based on training data.
The user can also change the unfixed parameters to see how the decision
changes.

(Note that with the given data and parameters, the value of data we cal-
culated is 0 for all of our models. Models with nearly perfect performance
will generate different results.)

**4. Data available, Spores available**

When both data and spores are available, the user should first choose a
prediction model from logistic regression, random forest and neural net-
works for the data to be trained on.

Then, they can upload input data file with the name "input data.csv". In
the submitted files, we provide an example of input data file. The user
can either directly use that file or change the data if they'd like to.
The selected model will then give predictions on whether there will be
warm rain based on the input data.

The app will recommend decision on whether to buy data based on the
performance of the selected model trained with the data to be bought,
whether to harvest immediately, as well as whether to buy spores. It will
also calculate the certain equivalent of two harvest decisions. If no data
file is uploaded, the decision analysis will be based on training data.
The user can also change the unfixed parameters to see how the decision
changes.

## Sensitivity Analysis

The user should first choose the parameter that they want to implement sensitivity analysis on and the corresponding plot will be rendered on the right. They can also change the probability of warm rain and risk tolerance by sliding the sliders and see how the figure changes. The available options are 

- Probability of Warm Rain
- Probability of Botrytis
- Probability of 25% Sugar Level
- Risk Tolerance

