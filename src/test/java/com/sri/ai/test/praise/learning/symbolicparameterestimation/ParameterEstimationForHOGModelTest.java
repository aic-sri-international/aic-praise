package com.sri.ai.test.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMProblemError;
import com.sri.ai.praise.learning.symbolicparameterestimation.ParameterEstimationForHOGModel;

public class ParameterEstimationForHOGModelTest {
	
	//@Test
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
			
			List<Expression> queryExpressionList = new LinkedList<Expression>();
			queryExpressionList.add(parse("likeIncumbent > likeChallenger"));
			
			ParameterEstimationForHOGModel parameterEstimationForHOGModel = new ParameterEstimationForHOGModel(modelString, queryExpressionList, modelErrors);
			
			HashMap<Expression,Double> expected = new HashMap<Expression,Double>();
			expected.put(parse("Alpha"), 1.0); 
			
			HashMap<Expression,Double> mapResult = runTestHOGModelBased(expected, parameterEstimationForHOGModel, new double[] {0});

			assertEquals(expected, mapResult);
	}
		
	private HashMap<Expression,Double> runTestHOGModelBased(HashMap<Expression,Double> expected, ParameterEstimationForHOGModel parameterEstimationForHOGModel, double[] startPoint) {
		
		HashMap<Expression,Double> result = parameterEstimationForHOGModel.optimizeWhenModelIsHOGModel(
				parameterEstimationForHOGModel.hogmModel,
				parameterEstimationForHOGModel.evidences,
				GoalType.MAXIMIZE,
				startPoint);
		return result;

	}

}
