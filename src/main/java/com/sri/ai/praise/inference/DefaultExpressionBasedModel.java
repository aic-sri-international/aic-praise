package com.sri.ai.praise.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;

public class DefaultExpressionBasedModel implements ExpressionBasedModel {

	protected Map<String, String> mapFromRandomVariableNameToTypeName = new LinkedHashMap<>();
	protected Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = new LinkedHashMap<>();
	protected Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = new LinkedHashMap<>();
	protected Map<String, String> mapFromCategoricalTypeNameToSizeString = new LinkedHashMap<>();
	protected Collection<Type> additionalTypes = new LinkedList<>();
	protected List<Expression> factors = new ArrayList<>();

	public DefaultExpressionBasedModel(
			List<Expression> factors,
			Map<String, String> mapFromRandomVariableNameToTypeName,
			Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes) {
		
		this.factors.addAll(factors);
		this.mapFromRandomVariableNameToTypeName.putAll(mapFromRandomVariableNameToTypeName);
		this.mapFromNonUniquelyNamedConstantNameToTypeName.putAll(mapFromNonUniquelyNamedConstantNameToTypeName);
		this.mapFromUniquelyNamedConstantNameToTypeName.putAll(mapFromUniquelyNamedConstantNameToTypeName);
		this.mapFromCategoricalTypeNameToSizeString.putAll(mapFromCategoricalTypeNameToSizeString);
		this.additionalTypes = additionalTypes;
	}	

	public static class Parameters {
		public List<Expression>    factors                                       = new ArrayList<>(); 
		public Map<String, String> mapFromRandomVariableNameToTypeName           = new LinkedHashMap<>();
		public Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = Collections.emptyMap(); // Not used for Graphical Networks
		public Map<String, String> mapFromUniquelyNamedConstantNameToTypeName    = new LinkedHashMap<>();
		public Map<String, String> mapFromCategoricalTypeNameToSizeString        = new LinkedHashMap<>();
		public Collection<Type>    additionalTypes                               = new LinkedList<>();
	}

	public DefaultExpressionBasedModel(Parameters parameters) {
		this(
				parameters.factors, 
				parameters.mapFromRandomVariableNameToTypeName,
				parameters.mapFromNonUniquelyNamedConstantNameToTypeName,
				parameters.mapFromUniquelyNamedConstantNameToTypeName,
				parameters.mapFromCategoricalTypeNameToSizeString,
				parameters.additionalTypes
				);
	}

	public DefaultExpressionBasedModel() {
		super();
	}

	@Override
	public List<Expression> getFactors() {
		return factors;
	}

	@Override
	public Map<String, String> getMapFromRandomVariableNameToTypeName() {
		return mapFromRandomVariableNameToTypeName;
	}

	@Override
	public Map<String, String> getMapFromNonUniquelyNamedConstantNameToTypeName() {
		return mapFromNonUniquelyNamedConstantNameToTypeName;
	}

	@Override
	public Map<String, String> getMapFromUniquelyNamedConstantNameToTypeName() {
		return mapFromUniquelyNamedConstantNameToTypeName;
	}

	@Override
	public Map<String, String> getMapFromCategoricalTypeNameToSizeString() {
		return mapFromCategoricalTypeNameToSizeString;
	}

	@Override
	public Collection<Type> getAdditionalTypes() {
		return additionalTypes;
	}

	@Override
	public String toString() {
		StringJoiner sj = new StringJoiner("\n");
		
		sj.add("factors                                      ="+factors);
		sj.add("mapFromRandomVariableNameToTypeName          ="+mapFromRandomVariableNameToTypeName);
		sj.add("mapFromNonUniquelyNamedConstantNameToTypeName="+mapFromNonUniquelyNamedConstantNameToTypeName);
		sj.add("mapFromUniquelyNamedConstantNameToTypeName   ="+mapFromUniquelyNamedConstantNameToTypeName);
		sj.add("mapFromCategoricalTypeNameToSizeString       ="+mapFromCategoricalTypeNameToSizeString);
		sj.add("additionalTypes                              ="+additionalTypes);
		
		return sj.toString();
	}

}