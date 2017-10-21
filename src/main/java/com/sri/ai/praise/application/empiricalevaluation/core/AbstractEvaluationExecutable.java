package com.sri.ai.praise.application.empiricalevaluation.core;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.sri.ai.praise.application.empiricalevaluation.options.EvaluationConfigurationFromCommandLineOptions;
import com.sri.ai.praise.empiricalevaluation.api.configuration.EvaluationConfiguration;
import com.sri.ai.praise.empiricalevaluation.core.Evaluation;
import com.sri.ai.praise.empiricalevaluation.core.OutputListener;
import com.sri.ai.praise.empiricalevaluation.core.configuration.DefaultEvaluationConfiguration;
import com.sri.ai.praise.empiricalevaluation.core.configuration.SolverConfiguration;
import com.sri.ai.praise.model.common.io.PagedModelContainer;
import com.sri.ai.praise.probabilisticsolver.SolverEvaluatorConfiguration;

/**
 * Provides a static method for outputting evaluation results for given solvers,
 * models and configurations.
 * 
 * @author braz
 *
 */
public abstract class AbstractEvaluationExecutable {

	/**
	 * Returns options specifications.
	 * 
	 * @return
	 * @throws IOException 
	 * @throws FileNotFoundException 
	 */
	protected EvaluationConfigurationFromCommandLineOptions makeEvaluationArgumentsFromCommandLineOptions(String args[]) throws FileNotFoundException, IOException {
		return new EvaluationConfigurationFromCommandLineOptions(args);
	}

	/**
	 * A method making a {@link PagedModelContainer} from evaluation arguments.
	 * 
	 * @param evaluationArgs
	 * @return
	 * @throws IOException
	 */
	abstract protected PagedModelContainer makeModelsContainer(EvaluationConfiguration evaluationArgs) throws IOException;

	public void run(String[] args) throws Exception {
		
		try (DefaultEvaluationConfiguration evaluationConfiguration = getEvaluationArgs(args)) {

			PagedModelContainer modelsContainer = makeModelsContainer(evaluationConfiguration);
			SolverConfiguration configuration = new SolverConfiguration(Evaluation.ProblemType.PR,
					evaluationConfiguration.getWorkingDirectory(), evaluationConfiguration.getNumberOfRunsToAverageOver());
			List<SolverEvaluatorConfiguration> solverConfigurations = new ArrayList<>();
			for (String solverImplementationClassName : evaluationConfiguration.getSolverImplementationClassNames()) {
				solverConfigurations.add(
						new SolverEvaluatorConfiguration(
								solverImplementationClassName,
								evaluationConfiguration.getTotalCPURuntimeLimitSecondsPerSolveAttempt(),
								evaluationConfiguration.getTotalMemoryLimitInMegabytesPerSolveAttempt(),
								!evaluationConfiguration.doesNotCacheTranslations(),
								Collections.emptyMap()));
			}
	
			PrintStream notificationOut = evaluationConfiguration.getNotificationOut();
			PrintStream resultOut = evaluationConfiguration.getResultOut();
	
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
	public static void evaluate(SolverConfiguration configuration, PagedModelContainer modelsContainer,
			List<SolverEvaluatorConfiguration> solverConfigurations, PrintStream notificationOut,
			PrintStream resultOut) {

		Evaluation evaluation = new Evaluation();
		evaluation.evaluate(configuration, solverConfigurations, modelsContainer, new OutputListener() {
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

	private DefaultEvaluationConfiguration getEvaluationArgs(String[] args)
			throws UnsupportedEncodingException, FileNotFoundException, IOException {

		EvaluationConfigurationFromCommandLineOptions optionSpecs = makeEvaluationArgumentsFromCommandLineOptions(args);
		return optionSpecs.getEvaluationArgs();
	}
}