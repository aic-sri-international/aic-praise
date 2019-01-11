package com.sri.ai.praise.learning.symbolicparameterestimation.util;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.number.Times.getMultiplicands;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.parseHOGModelToExpressionBasedModel;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.parseModelStringToHOGMModel;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;

import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;

/**
 * Define database of examples of ExpressionBasedModels to test methods in Parameter Estimation.
 * 
 * @author Sarah Perrin
 *
 */

public class ExpressionBasedModelExamples {

	public static ExpressionBasedModel buildModel1() {
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
		return expressionBasedModel;
	}
	
	public static ExpressionBasedModel buildModel2() {
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
		
		HOGModel hogmModel = parseModelStringToHOGMModel(modelString);
		
		ExpressionBasedModel expressionBasedModel = parseHOGModelToExpressionBasedModel(hogmModel);
		
		return expressionBasedModel;
	}
	
	public static ExpressionBasedModel buildModel3() {
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
		
		HOGModel hogmModel = parseModelStringToHOGMModel(modelString);
		
		ExpressionBasedModel expressionBasedModel = parseHOGModelToExpressionBasedModel(hogmModel);
		
		return expressionBasedModel;
	}
	
	public static ExpressionBasedModel buildModel4() {
		
		Map<String, String>	mapFromCategoricalTypeNameToSizeString = map(
						"Folks", "10",
						"Boolean", "2");

		Map<String, String>	mapFromRandomVariableNameToTypeName = map(
						"earthquake", "Boolean",
						"burglar",    "Folks", // a multi-value random variable
						"alarm",      "Boolean"
						);

		Map<String, String>	mapFromNonUniquelyNamedConstantNameToTypeName = map(
						"seismicLocation", "Boolean"
						);

		Map<String, String>	mapFromUniquelyNamedConstantNameToTypeName = map("none", "Folks", "tom", "Folks", "Alpha", "Real");

		boolean	isBayesianNetwork = true;
		List<Expression> factors = getMultiplicands(parse("" + 
						"(if earthquake then Alpha else 1-Alpha) * " +
						"(if burglar = none then 0.7 else if burglar = tom then 0.1 else 0.2 / (|Folks| - 2)) * " +
						// note the division above of the potential by number of remaining values, as the probabilities must sum up to 1
						"(if burglar != none or earthquake "
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
		return expressionBasedModel;
	}
	
	public static ExpressionBasedModel buildModel5() {
		String modelString = "random earthquake: Boolean;\n"
				+"random burglary: Boolean;\n"
		+"random alarm: Boolean;\n"
		+"constant Alpha: Real;\n"
		+"constant Beta: Real;\n"
		
		+"earthquake 0.01;\n"
		+"burglary 0.1;\n"

		+"if earthquake\n"
		   +"then if burglary\n"
		      +"then alarm Alpha\n"
		      +"else alarm Beta\n"
		   +"else if burglary\n"
		      +"then alarm 0.9\n"
		      +"else alarm 0.01;\n";
		
		HOGModel hogmModel = parseModelStringToHOGMModel(modelString);
		
		ExpressionBasedModel expressionBasedModel = parseHOGModelToExpressionBasedModel(hogmModel);
		
		return expressionBasedModel;
	}

}
