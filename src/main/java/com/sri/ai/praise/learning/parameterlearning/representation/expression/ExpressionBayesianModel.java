package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedModel;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionFactorNetwork;
import com.sri.ai.praise.learning.parameterlearning.BayesianModel;

public class ExpressionBayesianModel extends ExpressionFactorNetwork implements BayesianModel {

	private List<ExpressionBayesianNode> nodes;
	
	public ExpressionBayesianModel(List<ExpressionBayesianNode> nodes) {
		super(nodes);
		this.nodes = nodes;
	}

	@Override
	public List<ExpressionBayesianNode> getNodes() {
		return nodes;
	}
	
	public ExpressionBasedModel convertToAnExpressionBasedModelAfterLearning() {
		// The nodes of this Bayesian model
		List<? extends Expression> factors = this.getNodes();
		
		// The definitions of variables
		Map<String, String> mapFromRandomVariableNameToTypeName = map();
		for(ExpressionBayesianNode node : this.getNodes()) {
			ExpressionVariable nodeVariable = node.getChildVariable();
			Context context = node.getContext();
			Type type = context.getTypeOfRegisteredSymbol(nodeVariable);
			mapFromRandomVariableNameToTypeName.put(nodeVariable.toString(), type.toString());
		}
		
		// The definitions of non-uniquely named constants (after learning we shall have no more constants) // come back here, save previous constants (before adding the parameters as constants) in this map, for every node
		Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = map();
		
		// The definitions of uniquely named constants // come back here, same reason as above
		Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = map();
		
		// The definitions of types // come back here, save categorical types for every node here
		Map<String, String> mapFromCategoricalTypeNameToSizeString = map();
		
		// Additional types // come back here, is there a way to save any additional types for every node here?
		Collection<Type> additionalTypes = list();
		
		// ExpressionBayesianModels are known to be Bayesian models
		boolean isBayesianNetwork = true;
		
		ExpressionBasedModel convertedModel = new DefaultExpressionBasedModel(
				factors,
				mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName,
				mapFromUniquelyNamedConstantNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes,
				isBayesianNetwork);
		
		return convertedModel;
	}
	
}
