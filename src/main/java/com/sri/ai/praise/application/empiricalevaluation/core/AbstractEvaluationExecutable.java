package com.sri.ai.praise.application.empiricalevaluation.core;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.application.empiricalevaluation.options.EvaluationConfigurationFromCommandLineOptions;
import com.sri.ai.praise.empiricalevaluation.api.configuration.SetOfSolversEvaluationConfiguration;
import com.sri.ai.praise.empiricalevaluation.api.configuration.SolverEvaluationConfiguration;
import com.sri.ai.praise.empiricalevaluation.core.Evaluation;
import com.sri.ai.praise.empiricalevaluation.core.OutputListener;
import com.sri.ai.praise.empiricalevaluation.core.configuration.DefaultSetOfSolversEvaluationConfiguration;
import com.sri.ai.praise.empiricalevaluation.core.configuration.DefaultSolverEvaluationConfiguration;
import com.sri.ai.praise.model.common.io.PagedModelContainer;
import com.sri.ai.praise.probabilisticsolver.SolverConfiguration;

/**
 * Provides a static method for outputting evaluation results for given solvers,
 * models and configurations.
 * 
 * @author braz
 *
 */
public abstract class AbstractEvaluationExecutable {

	/**
	 * A method making a {@link PagedModelContainer} from evaluation arguments.
	 * 
	 * @param evaluationArgs
	 * @return
	 * @throws IOException
	 */
	abstract protected PagedModelContainer makeModelsContainer(SetOfSolversEvaluationConfiguration evaluationArgs) throws IOException;

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

	public void run(String[] args) throws Exception {
		
		try (DefaultSetOfSolversEvaluationConfiguration evaluationConfiguration = getEvaluationArgs(args)) {

			PagedModelContainer modelsContainer = makeModelsContainer(evaluationConfiguration);
			
			SolverEvaluationConfiguration configuration =
					new DefaultSolverEvaluationConfiguration(
							Evaluation.ProblemType.PR,
							evaluationConfiguration.getWorkingDirectory(),
							evaluationConfiguration.getNumberOfRunsToAverageOver());
			
			List<SolverConfiguration> solverConfigurations = makeSolverEvaluatorConfigurations(evaluationConfiguration);
	
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
	public static void evaluate(SolverEvaluationConfiguration configuration, PagedModelContainer modelsContainer,
			List<SolverConfiguration> solverConfigurations, PrintStream notificationOut,
			PrintStream resultOut) {
	
		Evaluation evaluation = new Evaluation();
		OutputListener outputListener = new OutputListener(notificationOut, resultOut);
		evaluation.evaluate(configuration, solverConfigurations, modelsContainer, outputListener);
	}

	private List<SolverConfiguration> makeSolverEvaluatorConfigurations(DefaultSetOfSolversEvaluationConfiguration evaluationConfiguration) {
		List<SolverConfiguration> solverConfigurations = new ArrayList<>();
		for (String solverImplementationClassName : evaluationConfiguration.getSolverImplementationClassNames()) {
			SolverConfiguration solverConfiguration = 
					makeSolverEvaluatorConfiguration(solverImplementationClassName, evaluationConfiguration);
			solverConfigurations.add(solverConfiguration);
		}
		return solverConfigurations;
	}

	private SolverConfiguration makeSolverEvaluatorConfiguration(String solverImplementationClassName, SetOfSolversEvaluationConfiguration evaluationConfiguration) {
		SolverConfiguration solverConfiguration = 
				new SolverConfiguration(
						solverImplementationClassName, evaluationConfiguration);
		return solverConfiguration;
	}

	private DefaultSetOfSolversEvaluationConfiguration getEvaluationArgs(String[] args)
			throws UnsupportedEncodingException, FileNotFoundException, IOException {

		EvaluationConfigurationFromCommandLineOptions optionSpecs = makeEvaluationArgumentsFromCommandLineOptions(args);
		return optionSpecs.getEvaluationArgs();
	}
}