# AIC-PRAiSE [![Build Status](https://travis-ci.org/aic-sri-international/aic-praise.svg?branch=master)](https://travis-ci.org/aic-sri-international/aic-praise)
SRI International's AIC PRAiSE (Probabilistic Reasoning As Symbolic Evaluation) Library (for Java 1.8+)

### Overview
A Probabilistic Reasoning As Symbolic Evaluation (PRAiSE) library, developed at 
[SRI International's Artificial Intelligence Center](http://www.ai.sri.com/), which provides capabilities in the following areas:

* Lifted First-Order Probabilistic Inference.
* Support for defining First-Order Probabilistic Models. 
* Support for performing inference on (fragments of) Church probabilistic programs.

### Getting Started
* [Introduction](https://github.com/aic-sri-international/aic-praise/wiki/Introduction)
* [User Guide](https://github.com/aic-sri-international/aic-praise/wiki/docs/user%20guide.pdf)
* Latest Maven Information (for integration as a third party library)
      
      ```
      <dependency>
          <groupId>com.googlecode.aic-praise</groupId>
          <artifactId>aic-praise</artifactId>
          <version>1.1.0</version>
      </dependency>
      ```
* [Latest Release](https://github.com/aic-sri-international/aic-praise/releases)
* [Instructions on how to set up your workspace (for aic-praise developers)](https://github.com/aic-sri-international/aic-praise/wiki/Getting-Started)

### Demos
The [more recent demo](https://github.com/aic-sri-international/aic-praise/releases/download/20150602_latest_demo_apps/aic-praise-sgsolver-demo-app.jar) is faster and produces exact results, but does not yet support relational models.

The [older demo](https://github.com/aic-sri-international/aic-praise/releases/download/20150602_latest_demo_apps/aic-praise-old-demo-app.jar) is approximate (because it is based on belief propagation) and slower, but supports relational models.

To execute them, you must have a [Java Runtime Environment](http://java.com/en/download/) installed. Then simply download and run the jar files above.

The [User Guide](https://github.com/aic-sri-international/aic-praise/wiki/docs/user%20guide.pdf) explains the language and demos operation.

##### Acknowledgements
SRI International gratefully acknowledges the support of the Defense Advanced Research Projects Agency (DARPA) 
Machine Reading Program, and Probabilistic Programming for Advanced Machine Learning Program, under Air Force 
Research Laboratory (AFRL) prime Contract Nos. FA8750-09-C-0181 and FA8750-14-C-0005, respectively. Any opinions, 
findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not 
necessarily reflect the view of DARPA, AFRL, or the US government.
