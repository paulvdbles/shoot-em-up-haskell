# shoot-em-up-haskell

# IDE and the likes:

IntelliJ IDEA with the following plugins:

* IntelliJ-Haskell


## Installation guide for IntelliJ-Haskell
Install the **beta** version of IntelliJ-Haskell plugin:

Add this to Plugins > Browse repositories > Manage Repositories:  
https://plugins.jetbrains.com/plugins/alpha/8258

Now install/update IntelliJ-Haskell

# Building the project with Stack

*In the upper right of your screen:*  
Edit Configurations > + > Haskell Stack > Haskell Stack Runner

Give the config a name and enable `single instance` if you care.

Click OK to close the screen.

Now you're free to click the green play button to run the project 

# Building the project with Cabal
Execute:

`cabal install --only-dependencies`  
`cabal build`
