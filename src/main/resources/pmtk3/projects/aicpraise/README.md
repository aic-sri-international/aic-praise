A set of utility routines have been developed for grounding the lifted probabilistic models used in PRAiSE and also for exporting them to other formats so that comparisons between different implementation approaches may be made. Support is provided for exporting to the [PMTK3](https://github.com/probml/pmtk3) probabilistic modeling toolkit for [Matlab](http://www.mathworks.com/products/matlab/)/[Octave](http://www.gnu.org/software/octave/), which contains a collection of propositional based probabilistic inference algorithms.

### Exporting Lifted Models to PMTK3
__Note:__ these instructions have been tested using pmtk3-1nov12.zip (Uploaded Oct 1). On a Windows 7 platform running Matlab R2012b (32 bit). There are issues using the 64 bit version of Matlab out of the box with some of the sub-libraries used by PMTK3 (similarly when running on Mac).

__Note:__ While PMTK3 is dependent on several Matlab toolboxes, for the probability inference capabilities the default Matlab functionality appears to be sufficient (i.e. no additional toolboxes needed).

#### Initial Install and Setup
* Install the Matlab xunit library for use with the aicpraise PMTK3 extension project (used to write validation tests within this project).
* Download and install the latest version of PMTK3.
* Extract PMTK3 onto your machine (ideally C:\pmtk3).
* Check out a workspace for:
    http:code.google.com/p/aic-praise/source/browse/trunk/src/main/resources/pmtk3/projects/aicpraise
    under:
    C:\pmtk3\projects
                     \aicpraise
    this will ensure the aicpraise PMTK3 extension project is visible in Matlab when the PMTK3 project is initialized (see later for details). 
* Follow the PMTK3 installation instructions. 

#### Running Exported PRAiSE Models in PMTK3
* The ExportToPMTK3FactorGraph utility class is what should be used to export a PRAiSE HOGMv0 model to PMTK3.
* Any newly exported PMTK3 models should be placed under:<br>`C:\pmtk3\projects\aicpraise\models`<br>
* Start up Matlab and run the:<br>`initPmtk3`<br> command when C:\pmtk3 is your current working folder in Matlab. This will ensure that everything is visible within the Matlab environment, including the functions/routines defined in the aicpraise extension project. 
* Update the aicPRAiSE() function in: <br>`C:\pmtk3\projects\aicpraise\aicPRAiSE.m`<br> so that the '@mk....' function for the new model is added and then call the aicPRAiSE function from within Matlab. All the relevant PMTK3 probability inference engines will be run, with their output placed on the screen in a format that can be copied and pasted into a spreadsheet for analysis. 
