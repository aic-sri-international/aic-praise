package com.sri.ai.test.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.number.Times.getMultiplicands;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedModel;
import com.sri.ai.praise.learning.symbolicparameterestimation.ParameterEstimationForExpressionBasedModel;

public class ParameterEstimationForExpressionBasedModelTest {

	@Test
	public void testExpressionBased() {

		// The definitions of types
		Map<String, String> mapFromCategoricalTypeNameToSizeString = map(
				"Folks", "10",
				"Boolean", "2");

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
		Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = map("none", "Folks", "tom", "Folks");

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
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("not earthquake"));

		HashMap<Expression,Double> expected = new HashMap<Expression,Double>();
		expected.put(parse("Alpha"), 0.5);
		
		HashMap<Expression,Double> mapResult = runTestExpressionBased(queryExpressionList,
				expressionBasedModel, new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);

		queryExpressionList.clear();
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("earthquake"));

		expected.put(parse("Alpha"), 1.0);

		mapResult = runTestExpressionBased(queryExpressionList,
				expressionBasedModel, new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);

		queryExpressionList.clear();
		queryExpressionList.add(parse("not earthquake"));
		queryExpressionList.add(parse("not earthquake"));

		expected.put(parse("Alpha"), 1.4905019930082135E-22);

		mapResult = runTestExpressionBased(queryExpressionList,
				expressionBasedModel, new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);




		queryExpressionList.clear();
		queryExpressionList.add(parse("not earthquake"));
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("earthquake"));
		queryExpressionList.add(parse("earthquake"));

		expected.put(parse("Alpha"), 0.9000011823080596);

		mapResult = runTestExpressionBased(queryExpressionList,
				 expressionBasedModel, new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);

		queryExpressionList.clear();
		queryExpressionList.add(parse("not earthquake"));
		queryExpressionList.add(parse("burglary"));

		expected.put(parse("Alpha"), 6.289892249011522E-23);
		expected.put(parse("Beta"), 1.0);

		mapResult = runTestExpressionBased(queryExpressionList, 
				expressionBasedModel, new double[] {0,0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);

		// Test with another model
		
		// The definitions of types
		mapFromCategoricalTypeNameToSizeString = map(
				"Folks", "10",
				"Boolean", "2");

		// The definitions of variables
		mapFromRandomVariableNameToTypeName = map(
				"earthquake", "Boolean",
				"burglar",    "Folks", // a multi-value random variable
				"alarm",      "Boolean"
				);

		// The definitions of non-uniquely named constants
		mapFromNonUniquelyNamedConstantNameToTypeName = map(
				"seismicLocation", "Boolean"
				);

		// The definitions of non-uniquely named constants
		mapFromUniquelyNamedConstantNameToTypeName = map("none", "Folks", "tom", "Folks", "Alpha", "Real");

		// a variant of the earthquake/burglary model in which some burglars are more active than others.
		isBayesianNetwork = true;
		factors = getMultiplicands(parse("" + 
				"(if earthquake then Alpha else 1-Alpha) * " +
				"(if burglar = none then 0.7 else if burglar = tom then 0.1 else 0.2 / (|Folks| - 2)) * " +
				// note the division above of the potential by number of remaining values, as the probabilities must sum up to 1
				"(if burglar != none or earthquake "
				+    "then if alarm then 0.9 else 0.1 "
				+    "else if alarm then 0.05 else 0.95) " +
				""));

		queryExpressionList.clear();
		queryExpressionList.add(parse("earthquake"));
		expected.remove(parse("Beta"));
		expected.put(parse("Alpha"), 1.0); 
		
		ExpressionBasedModel expressionBasedModel2 = new DefaultExpressionBasedModel(
				factors,
				mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName,
				mapFromUniquelyNamedConstantNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				list(),
				isBayesianNetwork);
		
		mapResult = runTestExpressionBased(queryExpressionList, 
				expressionBasedModel2, new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);

	}

	private HashMap<Expression,Double> runTestExpressionBased(List<Expression> queryExpressions, ExpressionBasedModel expressionBasedModel, double[] startPoint) {

		ParameterEstimationForExpressionBasedModel parameterEstimationForExpressionBasedModel = new ParameterEstimationForExpressionBasedModel(expressionBasedModel, queryExpressions);
		HashMap<Expression,Double> result = parameterEstimationForExpressionBasedModel.optimize(
				expressionBasedModel,
				queryExpressions,
				GoalType.MAXIMIZE,
				startPoint);
		ExpressionBasedModel newModel = parameterEstimationForExpressionBasedModel.buildOptimizedExpressionBasedModel(startPoint, result);
		System.out.println(" New Model : " + newModel);
		return result;

	}
	
}
