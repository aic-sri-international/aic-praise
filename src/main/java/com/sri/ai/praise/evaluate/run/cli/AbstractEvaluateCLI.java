package com.sri.ai.praise.evaluate.run.cli;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.sri.ai.praise.evaluate.run.Evaluation;
import com.sri.ai.praise.evaluate.solver.SolverEvaluatorConfiguration;
import com.sri.ai.praise.model.common.io.PagedModelContainer;

/**
 * Provides a static method for outputting evaluation results for given solvers,
 * models and configurations.
 * 
 * @author braz
 *
 */
public abstract class AbstractEvaluateCLI {

	/**
	 * Returns options specifications.
	 * 
	 * @return
	 * @throws IOException 
	 * @throws FileNotFoundException 
	 */
	protected PRAiSEEvaluationArgumentsFromCommandLineOptions makeEvaluationArgumentsFromCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		return new PRAiSEEvaluationArgumentsFromCommandLineOptions(args);
	}

	/**
	 * A method making a {@link PagedModelContainer} from evaluation arguments.
	 * 
	 * @param evaluationArgs
	 * @return
	 * @throws IOException
	 */
	abstract protected PagedModelContainer makeModelsContainer(PRAiSEEvaluationArguments evaluationArgs) throws IOException;

	public void run(String[] args) throws Exception {
		
		try (DefaultPRAiSEEvaluationArguments evaluationArgs = getEvaluationArgs(args)) {

			PagedModelContainer modelsContainer = makeModelsContainer(evaluationArgs);
			Evaluation.Configuration configuration = new Evaluation.Configuration(Evaluation.Type.PR,
					evaluationArgs.workingDirectory, evaluationArgs.numberRunsToAverageOver);
			List<SolverEvaluatorConfiguration> solverConfigurations = new ArrayList<>();
			for (String solverImplementationClassName : evaluationArgs.solverImplementationClassNames) {
				solverConfigurations.add(
						new SolverEvaluatorConfiguration(
								solverImplementationClassName,
								evaluationArgs.totalCPURuntimeLimitSecondsPerSolveAttempt,
								evaluationArgs.totalMemoryLimitInMegabytesPerSolveAttempt,
								!evaluationArgs.translateAlways,
								Collections.emptyMap()));
			}
	
			PrintStream notificationOut = evaluationArgs.notificationOut;
			PrintStream resultOut = evaluationArgs.resultOut;
	
			evaluate(configuration, modelsContainer, solverConfigurations, notificationOut, resultOut);
		}
	}

	/**
	 * Evaluates given solvers on given models according to given evaluation
	 * configurations and streams for notifications and results.
	 * 
	 * @param configuration
	 * @param modelsContainer
	 * @param solverConfigurations
	 * @param notificationOut
	 * @param resultOut
	 */
	public static void evaluate(Evaluation.Configuration configuration, PagedModelContainer modelsContainer,
			List<SolverEvaluatorConfiguration> solverConfigurations, PrintStream notificationOut,
			PrintStream resultOut) {

		Evaluation evaluation = new Evaluation();
		evaluation.evaluate(configuration, modelsContainer, solverConfigurations, new Evaluation.Listener() {
			@Override
			public void notification(String notification) {
				notificationOut.println(notification);
			}

			@Override
			public void notificationException(Exception ex) {
				ex.printStackTrace(notificationOut);
			}

			@Override
			public void csvResultOutput(String csvLine) {
				resultOut.println(csvLine);
			}
		});
	}

	private DefaultPRAiSEEvaluationArguments getEvaluationArgs(String[] args)
			throws UnsupportedEncodingException, FileNotFoundException, IOException {

		PRAiSEEvaluationArgumentsFromCommandLineOptions optionSpecs = makeEvaluationArgumentsFromCommandLineOptions(args);
		return optionSpecs.getEvaluationArgs();
	}
}