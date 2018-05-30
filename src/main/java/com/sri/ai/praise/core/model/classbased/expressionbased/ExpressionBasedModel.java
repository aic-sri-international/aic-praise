package com.sri.ai.praise.core.model.classbased.expressionbased;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;

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
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.model.api.Model;

public class ExpressionBasedModel implements Model, Cloneable {

	protected List<Expression> factors = new ArrayList<>();
	protected Map<String, String> mapFromRandomVariableNameToTypeName = new LinkedHashMap<>();
	protected Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = new LinkedHashMap<>();
	protected Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = new LinkedHashMap<>();
	protected Map<String, String> mapFromCategoricalTypeNameToSizeString = new LinkedHashMap<>();
	protected Collection<Type> additionalTypes = new LinkedList<>();
	protected boolean isKnownToBeBayesianNetwork = false;
	
	public ExpressionBasedModel() {
		super();
	}

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

	public List<Expression> getFactors() {
		return Collections.unmodifiableList(factors);
	}

	@Override
	public ExpressionBasedModel clone() {
		ExpressionBasedModel result;
		try {
			result = (ExpressionBasedModel) super.clone();
		}
		catch (CloneNotSupportedException exception) {
			throw new RuntimeException(exception);
		}
		return result;
	}
	
	public ExpressionBasedModel getConditionedModel(Expression evidence) {
		ExpressionBasedModel result;
		if (evidence != null && !Expressions.isNumber(evidence)) {
			result = clone();
			result.factors = new LinkedList<Expression>(factors);
			result.factors.add(IfThenElse.make(evidence, ONE, ZERO));
			result.isKnownToBeBayesianNetwork = false;
		}
		else {
			result = this;
		}
		return result;
	}
	
	public Map<String, String> getMapFromRandomVariableNameToTypeName() {
		return Collections.unmodifiableMap(mapFromRandomVariableNameToTypeName);
	}

	public Map<String, String> getMapFromNonUniquelyNamedConstantNameToTypeName() {
		return Collections.unmodifiableMap(mapFromNonUniquelyNamedConstantNameToTypeName);
	}

	public Map<String, String> getMapFromUniquelyNamedConstantNameToTypeName() {
		return Collections.unmodifiableMap(mapFromUniquelyNamedConstantNameToTypeName);
	}

	public Map<String, String> getMapFromCategoricalTypeNameToSizeString() {
		return Collections.unmodifiableMap(mapFromCategoricalTypeNameToSizeString);
	}

	public Collection<Type> getAdditionalTypes() {
		return Collections.unmodifiableCollection(additionalTypes);
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