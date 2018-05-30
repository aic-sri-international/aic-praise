package com.sri.ai.praise.core.model.classbased.expressionbased;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.praise.core.model.api.Model;

public class ExpressionBasedModel implements Model {

	protected List<Expression> factors = new ArrayList<>();
	protected Map<String, String> mapFromRandomVariableNameToTypeName = new LinkedHashMap<>();
	protected Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = new LinkedHashMap<>();
	protected Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = new LinkedHashMap<>();
	protected Map<String, String> mapFromCategoricalTypeNameToSizeString = new LinkedHashMap<>();
	protected Collection<Type> additionalTypes = new LinkedList<>();
	protected boolean isKnownToBeBayesianNetwork = false;
	
	public ExpressionBasedModel(
			List<Expression> factors,
			Map<String, String> mapFromRandomVariableNameToTypeName,
			Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes,
			boolean isKnownToBeBayesianNetwork
			) {
		
		this.factors.addAll(factors);
		this.mapFromRandomVariableNameToTypeName.putAll(mapFromRandomVariableNameToTypeName);
		this.mapFromNonUniquelyNamedConstantNameToTypeName.putAll(mapFromNonUniquelyNamedConstantNameToTypeName);
		this.mapFromUniquelyNamedConstantNameToTypeName.putAll(mapFromUniquelyNamedConstantNameToTypeName);
		this.mapFromCategoricalTypeNameToSizeString.putAll(mapFromCategoricalTypeNameToSizeString);
		this.additionalTypes = additionalTypes;
		this.isKnownToBeBayesianNetwork = isKnownToBeBayesianNetwork;
	}	

	public ExpressionBasedModel() {
		super();
	}

	public List<Expression> getFactors() {
		return factors;
	}

	public Map<String, String> getMapFromRandomVariableNameToTypeName() {
		return mapFromRandomVariableNameToTypeName;
	}

	public Map<String, String> getMapFromNonUniquelyNamedConstantNameToTypeName() {
		return mapFromNonUniquelyNamedConstantNameToTypeName;
	}

	public Map<String, String> getMapFromUniquelyNamedConstantNameToTypeName() {
		return mapFromUniquelyNamedConstantNameToTypeName;
	}

	public Map<String, String> getMapFromCategoricalTypeNameToSizeString() {
		return mapFromCategoricalTypeNameToSizeString;
	}

	public Collection<Type> getAdditionalTypes() {
		return additionalTypes;
	}
	
	public boolean isKnownToBeBayesianNetwork() {
		return isKnownToBeBayesianNetwork;
	}

	public String toString() {
		StringJoiner stringJoiner = new StringJoiner("\n");
		
		stringJoiner.add("factors                                       = " + factors);
		stringJoiner.add("mapFromRandomVariableNameToTypeName           = " + mapFromRandomVariableNameToTypeName);
		stringJoiner.add("mapFromNonUniquelyNamedConstantNameToTypeName = " + mapFromNonUniquelyNamedConstantNameToTypeName);
		stringJoiner.add("mapFromUniquelyNamedConstantNameToTypeName    = " + mapFromUniquelyNamedConstantNameToTypeName);
		stringJoiner.add("mapFromCategoricalTypeNameToSizeString        = " + mapFromCategoricalTypeNameToSizeString);
		stringJoiner.add("additionalTypes                               = " + additionalTypes);
		stringJoiner.add("isKnownToBeBayesianNetwork                    = " + isKnownToBeBayesianNetwork);
		
		return stringJoiner.toString();
	}

}