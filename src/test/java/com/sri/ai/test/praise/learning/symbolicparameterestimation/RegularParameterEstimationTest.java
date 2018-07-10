package com.sri.ai.test.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.ExpressionBasedModelExamples.buildModel1;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.learning.symbolicparameterestimation.regularparameterestimation.RegularParameterEstimation;
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
		
		ExpressionBasedModel expressionBasedModel3 = buildModel1();
		
		List<Expression> queryExpressionList3 = new LinkedList<Expression>();
		queryExpressionList3.add(parse("not earthquake and not burglary"));
		queryExpressionList3.add(parse("not earthquake and not burglary"));
		queryExpressionList3.add(parse("earthquake and burglary"));

		RegularParameterEstimation regularParameterEstimation3 = new RegularParameterEstimation(expressionBasedModel3, queryExpressionList3);
		
		Map<Expression, Double> result3 = regularParameterEstimation3.optimize();
		
		System.out.println(result3);
		
		
		
		
	}

}