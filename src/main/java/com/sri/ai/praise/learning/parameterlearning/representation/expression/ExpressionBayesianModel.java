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
import com.sri.ai.praise.learning.parameterlearning.Dataset;

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
	
	public ExpressionBasedModel learnModelParametersFromCompleteDataAndConvertToAnExpressionBasedModel(Dataset dataset) {
		this.learnModelParametersFromCompleteData(dataset);
		ExpressionBasedModel convertedModel = this.convertToAnExpressionBasedModelAfterLearning();
		return convertedModel;
	}
	
	/**
	 * Converting the learned ExpressionBayesianModel into a ExpressionBasedModel, ready for inferences.
	 * Some assumptions: the only constants in the initial expressions of the nodes were the parameters that are now learned, and there were no special categorical types or additional types 
	 * 
	 * @return the equivalent ExpressionBasedModel for inference
	 */
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
		
		// The definitions of non-uniquely named constants 
		// (we assume that the only constants for the expressions of the nodes were the parameters, now replaced by their learned values. After learning we shall have no more constants then - the map  below is empty)
		Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = map();
		
		// The definitions of uniquely named constants 
		// (similar comment as above)
		Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = map();
		
		// The definitions of types 
		// (we assume that there were no categorical types)
		Map<String, String> mapFromCategoricalTypeNameToSizeString = map();
		
		// Additional types 
		// (we assume that there were no additional types)
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
