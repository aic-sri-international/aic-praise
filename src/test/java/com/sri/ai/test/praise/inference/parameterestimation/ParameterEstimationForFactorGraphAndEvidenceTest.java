package com.sri.ai.test.praise.inference.parameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.praise.inference.parameterestimation.ParameterEstimationForFactorGraphAndEvidence;
import com.sri.ai.util.Util;

public class ParameterEstimationForFactorGraphAndEvidenceTest {
	
public static void main(String[] args) {
		
		// The definitions of types
	Map<String, String> mapFromCategoricalTypeNameToSizeString = Util.map(
				"Folks", "10",
				"Boolean", "2");

		// The definitions of variables
	Map<String, String> mapFromRandomVariableNameToTypeName = Util.map(
				"earthquake", "Boolean",
				"burglary",    "Boolean", 
				"alarm",      "Boolean"
				);

		// The definitions of non-uniquely named constants
	Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = Util.map(
				"seismicLocation", "Boolean",
				"Alpha", "Real",
				"Beta", "Real"
				);

		// The definitions of non-uniquely named constants
	Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = Util.map("none", "Folks", "tom", "Folks");

		// a variant of the earthquake/burglary model in which some burglars are more active than others.
		boolean isBayesianNetwork = true;
		List<Expression> factors = Times.getMultiplicands(parse("" + 
				"(if earthquake then Alpha else 1-Alpha) * " +
				"(if burglary then Beta else 1-Beta) * " +
				// note the division above of the potential by number of remaining values, as the probabilities must sum up to 1
				"(if burglary or earthquake "
				+    "then if alarm then 0.9 else 0.1 "
				+    "else if alarm then 0.05 else 0.95) " +
				""));

		Expression[] queryExpressionList = new Expression[2];
		Expression[] evidenceList = new Expression[2];
		queryExpressionList[0] = parse("earthquake or burglary");
		queryExpressionList[1] = parse("earthquake or burglary");
		evidenceList[0] = parse("alarm");
		evidenceList[1] = parse("true");
		//expected = parse("if earthquake then 0.01 else 0.99"); // the prior

		HashMap<Expression,Double> mapResult = ParameterEstimationForFactorGraphAndEvidence.optimize(false,
				queryExpressionList,
				evidenceList,
				isBayesianNetwork,
				factors,
				mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName,
				mapFromUniquelyNamedConstantNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				Util.list(),
				GoalType.MAXIMIZE,
				new double[] {0,0});
		
		System.out.println(mapResult);
	}

}
