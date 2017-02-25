To run the Main haskell-program, install ghc (Glasgow Haskell Compiler, sudo apt install ghc) and cabal (Haskell package manager sudo apt install cabal-install). Cabal is needed to install dequeue (a library for double ended queues, cabal install dequeue).

After that, 

ghc -O2 Main.hs

should create a binary which can run with 

./Main

and which will produce the output in output.txt.

The lists [3,2..0] and [5,4..0] in line 18 in Main.hs represent the number of right turns and straight pieces respectively (left turns are 12+right turns). Play with these if interested.

The output of the Haskell program can be copied more or less verbatim into the ps files (insert spaces). Adjust /len, /wid and "x x translate" (the ones right before the track definitions) to finetune the layout.
