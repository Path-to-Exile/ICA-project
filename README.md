# ICA-project
Code used in an ICA project on EEG data. ICA pipeline v.2 contains data loading, formatting data, pipeline and figure creation

Abstract:

This paper has two aims: a) Examine whether or not changes in select hyper-parameters can influence the perceived performance of different ICA methods in our pipelines. b) Assess the performance of different ICA methods in different pipelines on Electroencephalography (EEG) data from the BCI competition IV \cite{BCIdataset2008}.
  In the paper we show that fastICA leads to the best performance while pipelines with SOBI consistently performs the worst out of our considered ICA methods. Further we will establish that some choices in hyper-parameters influence the performance of the pipeline very little, e.g. band-pass filtering data in the $\beta$-range (13-30 Hz) contra band-pass filtering data in the $\alpha$- (8-13 Hz) and $\beta$-range. Other options such as classification algorithm seem to have a greater impact on performance
