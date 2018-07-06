package com.sri.ai.test.praise.learning.symbolicparameterestimation;

import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.parseHOGModelToExpressionBasedModel;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.parseModelStringToHOGMModel;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMProblemError;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.featurebased.ExpressionBasedModelToFeatureBasedModelTranslation;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;

public class GeneralTest {
	
	public static void main (String[] args) {
		
		String modelString = "random earthquake: Boolean;\n"
				+"random burglary: Boolean;\n"
		+"random alarm: Boolean;\n"
		
		+"earthquake 0.01;\n"
		+"burglary 0.1;\n"

		+"if earthquake\n"
		   +"then if burglary\n"
		      +"then alarm 0.95\n"
		      +"else alarm 0.6\n"
		   +"else if burglary\n"
		      +"then alarm 0.9\n"
		      +"else alarm 0.01;\n";
		
		List<HOGMProblemError> modelErrors = new ArrayList<>();
		HOGModel hogModel = parseModelStringToHOGMModel(modelString, modelErrors);
		ExpressionBasedModel expressionBasedModel = parseHOGModelToExpressionBasedModel(hogModel);
		
		System.out.println(expressionBasedModel.toString() + "\n");

		ExpressionBasedModelToFeatureBasedModelTranslation translation = new ExpressionBasedModelToFeatureBasedModelTranslation(expressionBasedModel, new LinkedList<>());
		
		System.out.println("translation : " + translation.featureBasedModel.toString());
		
	}

}
