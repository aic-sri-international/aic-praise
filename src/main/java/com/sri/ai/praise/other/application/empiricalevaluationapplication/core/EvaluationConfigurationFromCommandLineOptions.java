package com.sri.ai.praise.other.application.empiricalevaluationapplication.core;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.util.LinkedList;
import java.util.List;

import com.sri.ai.praise.core.inference.core.praiseprocess.PRAiSESolver;
import com.sri.ai.praise.other.empiricalevaluation.EvaluationConfiguration;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

// TODO - consider using commons-configuration to evaluation input file reading, i.e:
// https://commons.apache.org/proper/commons-configuration/userguide_v1.10/user_guide.html

/**
 * An {@link EvaluationConfiguration} initializaed from command line options.
 * 
 * @author braz
 *
 */
public class EvaluationConfigurationFromCommandLineOptions extends EvaluationConfiguration implements AutoCloseable {

	public OptionSet optionSet;

	public OptionParser parser;
	public OptionSpec<String> solverImplementationClasses;
	public OptionSpec<File> notificationFile;
	public OptionSpec<File> resultFile;
	public OptionSpec<Integer> totalCPURuntimeLimitSecondsPerSolveAttempt;
	public OptionSpec<Integer> totalMemoryLimitInMegabytesPerSolveAttempt;
	public OptionSpec<Integer> numberRunsToAverageOver;
	public OptionSpec<File> workingDirectory;

	public EvaluationConfigurationFromCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		super();
		setOptionsSetUsingDefaultEvaluationConfigurationAndCommandLineArguments(args);
		overrideEvaluationConfigurationsFromOptionSet();
	}

	private void setOptionsSetUsingDefaultEvaluationConfigurationAndCommandLineArguments(String[] args) {
		setOptionSpecificationsUsingDefaultEvaluationConfiguration();
		optionSet = parser.parse(args);
	}

	protected void setOptionSpecificationsUsingDefaultEvaluationConfiguration() {
		
		parser = new OptionParser();

		solverImplementationClasses = parser.accepts("s", "ExternalProcessSolver implementation class name.").withRequiredArg()
				.ofType(String.class);

		notificationFile = parser.accepts("n", "Notification output file name (default to stdout).")
				.withRequiredArg().ofType(File.class);
		resultFile = parser.accepts("r", "Result output file name (defaults to stdout).").withRequiredArg()
				.ofType(File.class);

		totalCPURuntimeLimitSecondsPerSolveAttempt = parser
				.accepts("c",
						"Total CPU runtime limit seconds per solver attempt (defaults to "
								+ getTotalCPURuntimeLimitSecondsPerSolveAttempt() + ").")
				.withRequiredArg().ofType(Integer.class);
		totalMemoryLimitInMegabytesPerSolveAttempt = parser
				.accepts("m",
						"Total memory limit in MB per solver attempt (defaults to "
								+ getTotalMemoryLimitInMegabytesPerSolveAttempt() + ").")
				.withRequiredArg().ofType(Integer.class);
		numberRunsToAverageOver = parser
				.accepts("a",
						"Number of runs to average each result over (defaults to "
								+ getNumberOfRunsToAverageOver() + ").")
				.withRequiredArg().ofType(Integer.class);
		parser.accepts("t",
				"Translate models always, instead of caching them between runs (default behavior is caching)");

		workingDirectory = parser
				.accepts("w", "ExternalProcessSolver Working Directory (temp directories and files will be created under here)")
				.withRequiredArg().required().ofType(File.class);

		parser.accepts("help", "For help on command line arguments").forHelp();
	}
	
	protected void overrideEvaluationConfigurationsFromOptionSet() throws IOException, FileNotFoundException {
		
		if (optionSet.has("help")) {
			parser.printHelpOn(System.out);
			System.exit(0);
		}

		
		List<String> currentSolverImplementationClasses = new LinkedList<>(getSolverImplementationClassNames());
		if (optionSet.has(solverImplementationClasses)) {
			currentSolverImplementationClasses.addAll(optionSet.valuesOf(solverImplementationClasses));
		} else {
			currentSolverImplementationClasses.add(PRAiSESolver.class.getName());
		}
		setSolverImplementationClassNames(currentSolverImplementationClasses);
		
		
		if (optionSet.has(notificationFile)) {
			setNotificationOut(new PrintStream(optionSet.valueOf(notificationFile)));
		}
		
		if (optionSet.has(resultFile)) {
			setCSVOut(new PrintStream(optionSet.valueOf(resultFile)));
		}
		
		if (optionSet.has(totalCPURuntimeLimitSecondsPerSolveAttempt)) {
			setTotalCPURuntimeLimitSecondsPerSolveAttempt(optionSet.valueOf(totalCPURuntimeLimitSecondsPerSolveAttempt));
		}
		
		if (optionSet.has(totalMemoryLimitInMegabytesPerSolveAttempt)) {
			setTotalMemoryLimitInMegabytesPerSolveAttempt(optionSet.valueOf(totalMemoryLimitInMegabytesPerSolveAttempt));
		}
		
		if (optionSet.has(numberRunsToAverageOver)) {
			setNumberOfRunsToAverageOver(optionSet.valueOf(numberRunsToAverageOver));
		}
		
		if (optionSet.has("t")) {
			setDoesNotCacheTranslations(true);
		}

		File workingDirectoryFile = optionSet.valueOf(workingDirectory);
		if (workingDirectoryFile.isDirectory()) {
			setWorkingDirectory(workingDirectoryFile);
		}
		else {
			throw new IllegalArgumentException("Working directory does not exist: " + workingDirectoryFile);
		}
	}

	public void close() throws IOException {
		closePrintStream(getNotificationOut());
		closePrintStream(getResultOut());
	}

	private void closePrintStream(PrintStream printStream) {
		if (printStream != null) {
			printStream.flush();
			if (printStream != System.out) {
				printStream.close();
			}
		}
	}
}