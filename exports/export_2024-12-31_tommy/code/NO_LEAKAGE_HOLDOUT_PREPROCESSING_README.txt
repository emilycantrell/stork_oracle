This is the same code that is used to create preprocessed training data, so the data files are called "train" 
even though in this case we are actually using the holdout data.

In "filter_to_prefer_train_and_eval_set," a few lines had to be changed because unlike the training data, the holdout data
does not have the columns "children_post2021" or "evaluation_set". 
These changes are annotated in all caps, in comments that start with "HOLDOUT DATA PREPROCESSING". 