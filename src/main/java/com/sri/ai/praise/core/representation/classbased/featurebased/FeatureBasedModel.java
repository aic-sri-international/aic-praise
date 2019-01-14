package com.sri.ai.praise.core.representation.classbased.featurebased;

import java.util.Map;

import com.sri.ai.expresso.api.Expression;

/**
 * Class to define a FeatureBasedModel
 * 
 * @author Sarah Perrin
 *
 */

public class FeatureBasedModel {
	
	public Map<Expression, Expression> mapConditionToWeight;
	
	public FeatureBasedModel(Map<Expression, Expression> mapConditionToWeight) {
		this.mapConditionToWeight = mapConditionToWeight;
	}
	
	@Override
	public String toString() {
		return mapConditionToWeight.toString();
	}

}
