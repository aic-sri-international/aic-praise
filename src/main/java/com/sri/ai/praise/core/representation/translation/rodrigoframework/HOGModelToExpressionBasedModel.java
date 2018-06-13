package com.sri.ai.praise.core.representation.translation.rodrigoframework;

import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;

/**
 * Class to convert a HOGMModel into an ExpressionBasedModel.
 * 
 * @author Sarah Perrin
 *
 */

public class HOGModelToExpressionBasedModel {
	
	public static ExpressionBasedModel parseModelStringToHOGMModel(HOGModel hogmModel) {
		
		ExpressionBasedModel result = hogmModel == null? null : new HOGMExpressionBasedModel(hogmModel);
		return result;
	}

}
