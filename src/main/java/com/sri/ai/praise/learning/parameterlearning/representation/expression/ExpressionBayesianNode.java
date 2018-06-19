package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import static com.sri.ai.util.Util.mergeElementsIntoOneList;

import java.util.LinkedList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.ExpressionVariable;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;

/**
 * Implementation of Bayesian nodes based on Expressions
 * 
 * @author Roger Leite Lucena
 *
 */

public class ExpressionBayesianNode extends ExpressionFactor implements BayesianNode {

	private ExpressionVariable child;
	private LinkedList<ExpressionVariable> parents;
	private LinkedList<ExpressionVariable> allVariables;
	private LinkedList<Expression> parameters;
	
	private LinkedList<Family> families;
	
	public ExpressionBayesianNode(Expression expression, Context context, ExpressionVariable child, LinkedList<ExpressionVariable> parents, LinkedList<Expression> parameters) {
		super(expression, context);
		this.child = child;
		this.parents = parents;
		this.allVariables = mergeElementsIntoOneList(child, parents);
		this.parameters = parameters;
	}

	@Override
	public Variable getChild() {
		return this.child;
	}

	@Override
	public List<? extends Variable> getParents() {
		return this.parents;
	}

	@Override
	public List<? extends Variable> getAllVariables() {
		return this.allVariables;
	}

	@Override
	public void setInitialCountsForAllPossibleChildAndParentsAssignments() {
		LinkedList<Expression> initialFamilies = generateInitialFamilies();
		computeFinalFamilies();
		
		
	}

	@Override
	public void incrementCountForChildAndParentsAssignment(List<? extends Object> childAndParentsValues) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void normalizeParameters() {
		// TODO Auto-generated method stub
		
	}
	
	private void computeFinalFamilies() {
		// TODO Auto-generated method stub
		
	}

	private LinkedList<Expression> generateInitialFamilies() {
		// TODO Auto-generated method stub
		return null;
	}

}
