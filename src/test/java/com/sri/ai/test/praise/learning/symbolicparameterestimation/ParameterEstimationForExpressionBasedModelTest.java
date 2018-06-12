package com.sri.ai.test.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.number.Times.getMultiplicands;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static org.junit.Assert.assertEquals;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.praise.learning.symbolicparameterestimation.ParameterEstimationForExpressionBasedModel;

public class ParameterEstimationForExpressionBasedModelTest {

	@Test
	public void test() {

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
				"seismicLocation", "Boolean",
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

		Expression[] queryExpressionList = new Expression[2];
		Expression[] evidenceList = new Expression[2];
		queryExpressionList[0] = parse("earthquake");
		queryExpressionList[1] = parse("not earthquake");
		evidenceList[0] = parse("true");
		evidenceList[1] = parse("true");

		HashMap<Expression,Double> expected = new HashMap<Expression,Double>();
		expected.put(parse("Alpha"), 0.5);

		HashMap<Expression,Double> mapResult = runTest(queryExpressionList, evidenceList,
				expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, 
				mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, 
				mapFromCategoricalTypeNameToSizeString, list(), new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);

		queryExpressionList = new Expression[2];
		evidenceList = new Expression[2];
		queryExpressionList[0] = parse("earthquake");
		queryExpressionList[1] = parse("earthquake");
		evidenceList[0] = parse("true");
		evidenceList[1] = parse("true");

		expected.put(parse("Alpha"), 1.0);

		mapResult = runTest(queryExpressionList, evidenceList,
				expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, 
				mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, 
				mapFromCategoricalTypeNameToSizeString, list(), new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);

		queryExpressionList[0] = parse("not earthquake");
		queryExpressionList[1] = parse("not earthquake");
		evidenceList[0] = parse("true");
		evidenceList[1] = parse("true");

		expected.put(parse("Alpha"), 1.4905019930082135E-22);

		mapResult = runTest(queryExpressionList, evidenceList,
				expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, 
				mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, 
				mapFromCategoricalTypeNameToSizeString, list(), new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);




		queryExpressionList = new Expression[10];
		evidenceList = new Expression[10];
		queryExpressionList[0] = parse("earthquake");
		queryExpressionList[1] = parse("earthquake");
		queryExpressionList[2] = parse("earthquake");
		queryExpressionList[3] = parse("earthquake");
		queryExpressionList[4] = parse("earthquake");
		queryExpressionList[5] = parse("earthquake");
		queryExpressionList[6] = parse("earthquake");
		queryExpressionList[7] = parse("earthquake");
		queryExpressionList[8] = parse("earthquake");
		queryExpressionList[9] = parse("not earthquake");
		evidenceList[0] = parse("true");
		evidenceList[1] = parse("true");
		evidenceList[2] = parse("true");
		evidenceList[3] = parse("true");
		evidenceList[4] = parse("true");
		evidenceList[5] = parse("true");
		evidenceList[6] = parse("true");
		evidenceList[7] = parse("true");
		evidenceList[8] = parse("true");
		evidenceList[9] = parse("true");

		expected.put(parse("Alpha"), 0.9000011823080596);

		mapResult = runTest(queryExpressionList, evidenceList,
				 expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, 
				 mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, 
				 mapFromCategoricalTypeNameToSizeString, list(), new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);


		queryExpressionList = new Expression[2];
		evidenceList = new Expression[2];

		queryExpressionList[0] = parse("not earthquake");
		queryExpressionList[1] = parse("not earthquake");
		evidenceList[0] = parse("true");
		evidenceList[1] = parse("true");

		expected.put(parse("Alpha"), 1.4905019930082135E-22);

		mapResult = runTest(queryExpressionList, evidenceList,
				expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, 
				mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, 
				mapFromCategoricalTypeNameToSizeString, list(), new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);

		queryExpressionList[0] = parse("not earthquake");
		queryExpressionList[1] = parse("not earthquake");
		evidenceList[0] = parse("alarm");
		evidenceList[1] = parse("not alarm");

		expected.put(parse("Alpha"), 7.116459640918507E-24);
		expected.put(parse("Beta"), 0.9999996696568659);

		mapResult = runTest(queryExpressionList, evidenceList,
				expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, 
				mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, 
				mapFromCategoricalTypeNameToSizeString, list(), new double[] {0,0});

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

		queryExpressionList = new Expression[1];
		evidenceList = new Expression[1];
		queryExpressionList[0] = parse("earthquake");
		evidenceList[0] = parse("alarm"); 
		expected.remove(parse("Beta"));
		expected.put(parse("Alpha"), 1.0); 
		mapResult = runTest(queryExpressionList, evidenceList,
				expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, 
				mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, 
				mapFromCategoricalTypeNameToSizeString, list(), new double[] {0});

		System.out.println("expected : " + expected);
		System.out.println("result : " + mapResult);
		assertEquals(expected, mapResult);

	}

	private HashMap<Expression,Double> runTest(Expression[] queryExpressionList, Expression[] evidenceList, HashMap<Expression,Double> expected, boolean isBayesianNetwork, List<Expression> factors, Map<String, String> mapFromRandomVariableNameToTypeName, Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName, Map<String, String> mapFromUniquelyNamedConstantNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes, double[] startPoint) {

		HashMap<Expression,Double> result = ParameterEstimationForExpressionBasedModel.optimize(false,
				queryExpressionList,
				evidenceList,
				isBayesianNetwork,
				factors,
				mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName,
				mapFromUniquelyNamedConstantNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				list(),
				GoalType.MAXIMIZE,
				startPoint);
		return result;


	}

}
