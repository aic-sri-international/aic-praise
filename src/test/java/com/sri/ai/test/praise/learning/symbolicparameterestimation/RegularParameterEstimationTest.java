package com.sri.ai.test.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.number.Times.getMultiplicands;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedModel;
import com.sri.ai.praise.learning.symbolicparameterestimation.RegularParameterEstimation;

import org.junit.Assert;
import org.junit.Test;

public class RegularParameterEstimationTest {
	
	@Test
	public void testCountOccurencesGivenParameter() {
		
		// The definitions of types
		Map<String, String> mapFromCategoricalTypeNameToSizeString = map();

		// The definitions of variables
		Map<String, String> mapFromRandomVariableNameToTypeName = map(
				"earthquake", "Boolean",
				"burglary",    "Boolean", 
				"alarm",      "Boolean"
				);

		// The definitions of non-uniquely named constants
		Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = map(
				"Alpha", "Real",
				"Beta", "Real"
				);

		// The definitions of non-uniquely named constants
		Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = map();

		// a variant of the earthquake/burglary model in which some burglars are more active than others.
		boolean isBayesianNetwork = true;
		List<Expression> factors = getMultiplicands(parse("" + 
				"(if earthquake then Alpha else 1-Alpha) * " +
				"(if burglary then Beta else 1-Beta) * " +
				// note the division above of the potential by number of remaining values, as the probabilities must sum up to 1
				"(if burglary or earthquake "
				+    "then if alarm then 0.9 else 0.1 "
				+    "else if alarm then 0.05 else 0.95) " +
				""));

		ExpressionBasedModel expressionBasedModel = new DefaultExpressionBasedModel(
				factors,
				mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName,
				mapFromUniquelyNamedConstantNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				list(),
				isBayesianNetwork);

		List<Expression> queryExpressionList = new LinkedList<Expression>();
		queryExpressionList.add(parse("earthquake and burglary"));
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("burglary"));

		RegularParameterEstimation regParEst = new RegularParameterEstimation(expressionBasedModel, queryExpressionList);
		int nbAlpha = regParEst.countOccurencesGivenParameter(parse("Alpha"));
		int nbBeta = regParEst.countOccurencesGivenParameter(parse("Beta"));
		
		Assert.assertEquals(3, nbAlpha);
		Assert.assertEquals(2, nbBeta);
	}

}
