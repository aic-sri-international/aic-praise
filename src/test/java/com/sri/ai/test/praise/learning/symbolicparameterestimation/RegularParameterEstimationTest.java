package com.sri.ai.test.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.ExpressionBasedModelExamples.buildModel1;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.learning.symbolicparameterestimation.regularparameterestimation.RegularParameterEstimation;
import com.sri.ai.praise.learning.symbolicparameterestimation.util.ExpressionBasedModelExamples;

import org.junit.Test;

/**
 * Tests for Regular Symbolic Parameter Estimation.
 * 
 * @author Sarah Perrin
 *
 */

public class RegularParameterEstimationTest {
	
	@Test
	public void testOptimization() {
		
		ExpressionBasedModel expressionBasedModel = buildModel1();
		
		List<Expression> queryExpressionList = new LinkedList<Expression>();
		queryExpressionList.add(parse("not earthquake and not burglary"));
		queryExpressionList.add(parse("not earthquake and not burglary"));
		queryExpressionList.add(parse("earthquake and burglary"));
		queryExpressionList.add(parse("not earthquake and burglary"));
		queryExpressionList.add(parse("earthquake and not burglary"));
		queryExpressionList.add(parse("earthquake and burglary"));
		queryExpressionList.add(parse("earthquake and burglary"));
		queryExpressionList.add(parse("earthquake and burglary"));
		queryExpressionList.add(parse("earthquake and burglary"));
		queryExpressionList.add(parse("not earthquake and burglary"));

		RegularParameterEstimation regularParameterEstimation = new RegularParameterEstimation(expressionBasedModel, queryExpressionList);
		
		Map<Expression, Double> result = regularParameterEstimation.optimize();
		
		System.out.println(result);
		
		ExpressionBasedModel expressionBasedModel2 = ExpressionBasedModelExamples.buildModel2();
		
		List<Expression> queryExpressionList2 = new LinkedList<Expression>();
		queryExpressionList2.add(parse("likeIncumbent > likeChallenger"));
		
		RegularParameterEstimation regularParameterEstimation2 = new RegularParameterEstimation(expressionBasedModel2, queryExpressionList2);
		
		Map<Expression, Double> result2 = regularParameterEstimation2.optimize();
		
		System.out.println(result2);
		
		
		
	}

}