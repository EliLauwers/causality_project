# About this document

This repo handles the 2022 project for the course of "causality and missing data". The project requirements can be found in `CMD2022_Project.pdf`. In essence, the project requires student to calculate the marginal effect of _quitting smoking_ on _weight gain in a later stage_ by means of various techniques. The final report is stored in `project.pdf`. The PDF-file is knit based on an R-Markdown file `project.Rmd`. 

# Code structure

The project consists of 7 main questions. I used the same workflow for every question:

0. As general preparation, two objects were created: `results` and `keep_in_env`
1. Create a main question file in the main directory
    + E.g.: Code for question 3 can be found in `q3.R`
    + Advantage: cutting up large projects in multiple code files helps to keep code files small and manageable
    + Disadvantage: It is tempting to create a nested code file structure that is not as manageable anymore
2. If the main question consists of multiple subquestions, create a code file for every subquestion and store it in the `scripts`-directory
    + E.g.: For questions 4, 5, and 7 I needed to fit multiple different models. Each of those models was stored in its own file
    + This method was chosen to be able to quickly _switch off_ an analysis (i.e. by out-commenting the particular `source(SUBQUESTION)` line), as sometimes I wanted to run/knit only a part of the document instead of the whole thing.
    + Subquestion code files were structured with the goal of returning some values or output to the main question file
3. In the main question file, the results for that question were then stored in the `results` object. 
4. After appending to the `results` object, I clean the environment. Here, all objects present in the `keep_in_env` list remain in the environment, while all other objects are removed.
    + I came up with a function `scripts/clean_env.R` to clean the global R-environment from within a sub environment.
    + I chose this workflow because sometimes it can happen that a script does not run properly, but can still carry on running because of variables that have persisted since other runs. By cleaning the work environment, this method guarantees that a script cannot run properly if not all needed objects are created properly.
5. Within the `.Rmd` document, the `results` object is outputted as needed. Here, a boolean for every question is used (e.g: `runQ4=TRUE`). If a boolean evaluates to `FALSE`, the main code file and by extension relevant subquestion code files are not run, which saves runtime.

# Workflow and things learned

I've noticed that I find it difficult to come up with a workflow which is flexible to not having to run full projects when I only want to see a small part of the output. In this project, I used a workflow of nested sourcing, and structuring code in nested code files. Looking back, I noticed that my codebase has become somewhat cluttered, with some room for optimization.

If I were to be doing this project again, I would structure my code a bit different and more like a project I have done in professional context.

- I would create a `.yml` file with some arguments to indicate which part of the report I want to run (e.g.: `runQ3=TRUE` or `runQ4.supercustom=FALSE`). Also, the yaml file can be used to store some constants, like the used cutoff in backwards step regression, or the used superLearner libraries
- When the code for a (sub) question is ran, I would store the results to some file (type).
- When a question is skipped, I would reuse the formerly stored results.
- This workflow makes sure that my main files wouldn't get as cluttered as now is the case. For example, `q5.R` would nearly be cut in half by removing all clutter. 


