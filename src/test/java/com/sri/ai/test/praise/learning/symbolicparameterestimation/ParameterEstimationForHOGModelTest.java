package com.sri.ai.test.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.parseHOGModelToExpressionBasedModel;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMProblemError;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.learning.symbolicparameterestimation.ParameterEstimationForHOGModel;
import com.sri.ai.util.base.Pair;

/**
 * @author Sarah Perrin
 */
public class ParameterEstimationForHOGModelTest {
	
	@Test
	public void testHOGMBased() {
			
			String modelString = "random terrorAttacks : 0..20;\n"
					+ "random newJobs : 0..100000;\n"
					+ "random dow: 11000..18000;\n"
					+ "random economyIsPoor : Boolean;\n"
					+ "random economyIsGreat : Boolean;\n"
					+ "random attackPerception: Boolean;\n"
					+ "random likeIncumbent  : 0..100000000;\n"
					+ "random likeChallenger : 0..100000000;\n"
					+ "constant Alpha: Real;\n"
					+ "economyIsPoor <=> dow < 13000 and newJobs < 30000;\n"
					+ "economyIsGreat <=> dow > 16000 and newJobs > 70000;\n"
					+ "attackPerception <=> terrorAttacks > 4;\n"
					+ "if economyIsGreat\n"
					+ "then if likeIncumbent > 70000000 then Alpha/30000000 else 1-Alpha/(70000000 + 1)\n"
					+ "else if economyIsPoor\n"
					+ "then if likeIncumbent < 40000000 then 0.8/40000000 else 0.2/(60000000 + 1)\n"
					+ "else if attackPerception\n"
					+ "then if likeIncumbent < 60000000 then 0.9/60000000 else 0.1/(40000000 + 1);\n";
			
			List<HOGMProblemError> modelErrors = new ArrayList<>();
			
			List<Pair<Expression, Expression>> pairsQueryEvidence = new LinkedList<Pair<Expression, Expression>>();
			
			Pair<Expression, Expression> pair = new Pair<Expression,Expression>(parse("likeIncumbent > likeChallenger"), parse("null"));
			
			pairsQueryEvidence.add(pair);
			
			ParameterEstimationForHOGModel parameterEstimationForHOGModel = new ParameterEstimationForHOGModel(modelString, pairsQueryEvidence, modelErrors);
			
			HashMap<Expression,Double> expected = new HashMap<Expression,Double>();
			expected.put(parse("Alpha"), 1.0); 
			
			HashMap<Expression,Double> mapResult = runTestHOGModelBased(expected, parameterEstimationForHOGModel, new double[] {0});

			HOGModel test = parameterEstimationForHOGModel.buildOptimizedHOGModel(mapResult);
			ExpressionBasedModel newModel = parseHOGModelToExpressionBasedModel(test);
			System.out.println(newModel.toString());
			assertEquals(expected, mapResult);
	}
	
	@Test
	public void testBuildOptimizedHOGModel() {
		String modelString = "random earthquake: Boolean;\n"
				+"random burglary: Boolean;\n"
		+"random alarm: Boolean;\n"
		+"constant Alpha: Real;\n"
		+"constant Beta: Real;\n"
		
		+"earthquake Alpha;\n"
		+"burglary Beta;\n"

		+"if earthquake\n"
		   +"then if burglary\n"
		      +"then alarm 0.95\n"
		      +"else alarm 0.6\n"
		   +"else if burglary\n"
		      +"then alarm 0.9\n"
		      +"else alarm 0.01;\n";
		
		List<HOGMProblemError> modelErrors = new ArrayList<>();
		
		List<Pair<Expression, Expression>> pairsQueryEvidence = new LinkedList<Pair<Expression, Expression>>();
		
		Pair<Expression, Expression> pair = new Pair<Expression,Expression>(parse("earthquake"), parse("null"));
		
		pairsQueryEvidence.add(pair);
		
		ParameterEstimationForHOGModel parameterEstimationForHOGModel = new ParameterEstimationForHOGModel(modelString, pairsQueryEvidence, modelErrors);
		
		HashMap<Expression,Double> expected = new HashMap<Expression,Double>();
		expected.put(parse("Alpha"), 1.0); 
		
		HashMap<Expression,Double> mapResult = runTestHOGModelBased(expected, parameterEstimationForHOGModel, new double[] {0});

		String newStringModel = parameterEstimationForHOGModel.buildOptimizedStringModel(mapResult);
		//ExpressionBasedModel newExpressionBasedModel = parameterEstimationForHOGModel.parseHOGModelToExpressionBasedModel(newStringModel);
		//System.out.println(newExpressionBasedModel.toString());
		
		System.out.println(newStringModel);
		
		pairsQueryEvidence.clear();
		
		pair = new Pair<Expression,Expression>(parse("burglary"), parse("null"));
		pairsQueryEvidence.add(pair);
		
		ParameterEstimationForHOGModel parameterEstimationForHOGModel2 = new ParameterEstimationForHOGModel(newStringModel, pairsQueryEvidence, modelErrors);
		
		expected.remove(parse("Alpha"));
		expected.put(parse("Beta"), 1.0); 
		
		HashMap<Expression,Double> mapResult2 = runTestHOGModelBased(expected, parameterEstimationForHOGModel2, new double[] {0});
		
		System.out.println(mapResult);

		String newStringModel2 = parameterEstimationForHOGModel2.buildOptimizedStringModel(mapResult2);
		//ExpressionBasedModel newModel = parameterEstimationForHOGModel.parseHOGModelToExpressionBasedModel(test);
		System.out.println(newStringModel2.toString());
	
	}
		
	private HashMap<Expression,Double> runTestHOGModelBased(HashMap<Expression,Double> expected, ParameterEstimationForHOGModel parameterEstimationForHOGModel, double[] startPoint) {
		
		HashMap<Expression,Double> result = parameterEstimationForHOGModel.optimizeWhenModelIsHOGModel(
				parameterEstimationForHOGModel.hogmModel,
				GoalType.MAXIMIZE,
				startPoint);
		return result;

	}

}
