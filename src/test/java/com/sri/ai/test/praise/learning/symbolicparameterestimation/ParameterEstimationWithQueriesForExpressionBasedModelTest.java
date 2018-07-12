package com.sri.ai.test.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.buildOptimizedExpressionBasedModel;
import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.learning.symbolicparameterestimation.ParameterEstimationWithQueriesForExpressionBasedModel;
import com.sri.ai.praise.learning.symbolicparameterestimation.util.ExpressionBasedModelExamples;
import com.sri.ai.util.base.Pair;

public class ParameterEstimationWithQueriesForExpressionBasedModelTest {
	
	@Test
	public void testExpressionBased() {
		
		ExpressionBasedModel expressionBasedModel = ExpressionBasedModelExamples.buildModel1();

		List<Pair<Expression, Expression>> pairsQueryEvidence = new LinkedList<Pair<Expression, Expression>>();
		
		Pair<Expression, Expression> pair = new Pair<Expression,Expression>(parse("earthquake"), parse("null"));

		Pair<Expression, Expression> pair2 = new Pair<Expression,Expression>(parse("not earthquake"), parse("null"));
		
		pairsQueryEvidence.add(pair);
		
		for(int i = 0; i < 999; i++) {
			pairsQueryEvidence.add(pair2);
		}
		
		HashMap<Expression,Double> expected = new HashMap<Expression,Double>();
		expected.put(parse("Alpha"), 9.999997011753915E-4);
		
		HashMap<Expression,Double> mapResult = runTestExpressionBased(pairsQueryEvidence,
				expressionBasedModel, new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);
		
		pairsQueryEvidence.clear();
		
		pair = new Pair<Expression,Expression>(parse("earthquake"), parse("alarm"));

		pair2 = new Pair<Expression,Expression>(parse("not earthquake"), parse("not alarm"));
		
		pairsQueryEvidence.add(pair);
		
		for(int i = 0; i < 999; i++) {
			pairsQueryEvidence.add(pair2);
		}
		
		expected = new HashMap<Expression,Double>();
		expected.put(parse("Alpha"), 0.008214845805751847);
		expected.put(parse("Beta"), 8.527665270285399E-9);
		
		mapResult = runTestExpressionBased(pairsQueryEvidence,
				expressionBasedModel, new double[] {0,0});
		
		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);
		
	}
	
	private HashMap<Expression,Double> runTestExpressionBased(List<Pair<Expression, Expression>> pairsQueryEvidence, ExpressionBasedModel expressionBasedModel, double[] startPoint) {

		ParameterEstimationWithQueriesForExpressionBasedModel parameterEstimationForExpressionBasedModel = new ParameterEstimationWithQueriesForExpressionBasedModel(expressionBasedModel, pairsQueryEvidence);
		HashMap<Expression,Double> result = parameterEstimationForExpressionBasedModel.optimize(
				expressionBasedModel,
				GoalType.MAXIMIZE,
				startPoint);
		ExpressionBasedModel newModel = buildOptimizedExpressionBasedModel(result, expressionBasedModel);
		System.out.println(" New Model : " + newModel);
		
		return result;

	}

}
