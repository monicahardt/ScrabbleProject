# ScrabbleProject

## Preface

This project is developed in spring 2023, as a part of a mandatory assignment in the course Functional Programming, taught by prof. Jesper Bengtson and Patrick Bahr. Authors of this project are Monica Hardt (monha@itu.dk), Selma Bernild (selb@itu.dk) & Rakul Maria H. Tórgarð (rakt@itu.dk).

The outcome of the project is to develop a ScrabbleBot, that can find and play valid scrabble moves. The project is left open, giving the developers the opportunity to aim for their level of amibition.

## Project & points

- Uses trie
- Implemented our own dictionary
- Finish a game against yourself on an infinite board (2 point)
  - Does not take points into account
  - Iterates over all combinations of valid words, and plays the longest word possible
  - Checks *every* letter on the board (doesn't use anchorpoints)

Arguing that the final points should be 2 points.

## Running

To run the program, open the project in Rider and run the selected configuration.

If you prefer running the program through the terminal, you will have to set the correct filepath to the directory.

- Go to the Program.fs file and navigate to line 37-40
- Uncomment the ```let words = ...``` line for the terminal (remember to comment the previous line)  

Now you can run the program through the terminal. Navigate to the root folder, ScrabbleProject, and run the following cmd:

    dotnet run --project ScrabbleTemplate/ScrabbleTemplate
