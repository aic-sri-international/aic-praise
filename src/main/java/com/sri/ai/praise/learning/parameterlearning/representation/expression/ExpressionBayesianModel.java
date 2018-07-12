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
	
	@Override
	public ExpressionBayesianModel copy() {
		List<ExpressionBayesianNode> copiedNodes = list();
		for(ExpressionBayesianNode node : this.nodes) {
			copiedNodes.add(node.copy());
		}
		ExpressionBayesianModel copy = new ExpressionBayesianModel(copiedNodes);
		return copy;
	}
	
	/**
	 * Learn the parameters of the model and convert the learned ExpressionBayesianModel into an ExpressionBasedModel, ready for inferences.
	 * 
	 * @param dataset
	 * @param mapFromCategoricalTypeNameToSizeString (the user specifies the categorical types used, usually an empty map)
	 * @param additionalTypes (the user specifies the additional types used, usually an empty list)
	 * 
	 * @return the final learned and already converted model
	 */
	public ExpressionBasedModel learnModelParametersFromCompleteDataAndConvertToAnExpressionBasedModel(Dataset dataset, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes) {
		ExpressionBayesianModel learnedModel = (ExpressionBayesianModel) this.learnModelParametersFromCompleteData(dataset);
		ExpressionBasedModel convertedModel = learnedModel.convertToAnExpressionBasedModelAfterLearning(mapFromCategoricalTypeNameToSizeString, additionalTypes);
		return convertedModel;
	}
	
	
	/**
	 * Converting the learned ExpressionBayesianModel into an ExpressionBasedModel, ready for inferences.
	 * Some assumptions: there are no constants in the initial expressions of the nodes (would result in errors when learning the parameters)
	 * 
	 * @param mapFromCategoricalTypeNameToSizeString (the user specifies the categorical types used, usually an empty map)
	 * @param additionalTypes (the user specifies the additional types used, usually an empty list)
	 * 
	 * @return the equivalent ExpressionBasedModel for inference
	 */
	public ExpressionBasedModel convertToAnExpressionBasedModelAfterLearning(Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes) {
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
		// (we assume that there are no constants in the initial expressions of the nodes - would result in errors when learning)
		Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = map();
		
		// The definitions of uniquely named constants 
		// (similar comment as above)
		Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = map();
		
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
