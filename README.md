# FacialExpressionRecognition

Description

Recognising facial expressions is a difficult problem, and there is lots of research in this area.
In particular, Grammatical Facial Expressions are used in sign language, and carry a specific
meaning (‘affirmative’, ‘negative’, and so on). Grammatical Facial Expressions are often used
to qualify specific hand signs or to complete the emotion related to the signs.

Data set
The dataset proposed in this Coursework was already used in the experiments described in
the paper by Freitas et al. (2014).

The dataset is composed by eighteen videos recorded using Microsoft Kinect. In each
video, a user performs (five times), in front of the sensor, five sentences in Libras (Brazilian
Sign Language) that require the use of a grammatical facial expression. The dataset considers
9 such Grammatical Facial Expressions (GFEs): affirmative, conditional, doubt question, emphasis,
negative, relative, topics, wh question and yn question.

Each video sequence in the dataset is thus a series of frames portraying each a GFE. By using
Microsoft Kinect, the scientists obtained: (a) an image of each frame, identified by a timestamp;
(b) a text file containing the coordinates (x, y, z) of 100 landmarks outlining the countours of
the facial expression (see Figure 1) from eyes, nose, eyebrows, face contour and iris.
There is one such text file per expression, for each user (A or B). So, in total, 18 text files. In all
such files, each line collects the coordinates of the 100 landmark points extracted from a single
frame. Other 18 files contain the class (positive or negative) of each frame. The name of the
file refers to each video: the letter corresponding to the user (A and B), name of grammatical
facial expression and a specification (target or datapoints). The images enabled the manual
labeling of each file by a specialist, providing the necessary ground truth for classification.

Goal
The goal of this assignment is, having selected one of the 9 available expressions (e.g., ‘affirmative’),
to classify each test frame, represented by a vector with 300 components, collecting the
x; y; z coordinates of the 100 landmarks making up the facial expression, as either a positive
example (i.e., the expression there is affirmative) or a negative example (the expression there
is not affirmative).
Hence, this is a binary classification problem: either the desired expression is present,
or it is not present.
