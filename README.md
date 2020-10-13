# Haskell-webcam: DAL for accessing the webcam

### Building the project:  

  - ```cabal build```

### Testing the project: 
 
 - ```cabal test```
 - ```cabal test --test-show-details=streaming --test-option=--color``` -- for nicer output

The project is tested using hspec as a tool for running the tests. Tests can be specified using the hspec syntax.

Hspec can easily run hunit tests if we prefer that kind of syntax. An example can be found here: https://hspec.github.io/hunit.html

### Adding new tests: 

As test autodiscovery has been set up these conventions have to be followed in order to add a new test file:
 - The name of a spec file has to end in Spec.hs; the module name has to match the file name.
 - Each spec file has to export a top-level binding spec of type Spec.
 - Spec files have to be placed into the same directory as the test driver (test/), or into a subdirectory.



