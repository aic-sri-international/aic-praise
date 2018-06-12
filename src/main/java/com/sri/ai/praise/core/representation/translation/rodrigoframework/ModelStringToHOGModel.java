package com.sri.ai.praise.core.representation.translation.rodrigoframework;

import java.util.ArrayList;

import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMModelParsing;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;

/**
 * Class to convert a model string into a HOGMModel.
 * 
 * @author Sarah Perrin
 *
 */

public class ModelStringToHOGModel {
	
	public static HOGModel parseModelStringToHOGMModel(String modelString) {
	
		HOGMModelParsing parsingWithErrorCollecting = new HOGMModelParsing(modelString, new ArrayList<>());
		HOGModel result = parsingWithErrorCollecting.getModel();
		return result;
	}

}
