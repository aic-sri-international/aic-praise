package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsForIndicesInListAndTypesInRegistry;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mergeElementsIntoOneList;
import static com.sri.ai.util.Util.println;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.ExpressionVariable;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;
import com.sri.ai.util.Util;

/**
 * Implementation of Bayesian nodes based on Expressions
 * 
 * @author Roger Leite Lucena
 *
 */

public class ExpressionBayesianNode extends ExpressionFactor implements BayesianNode {

	private Expression expression;
	private Context context;
	private ExpressionVariable child;
	private LinkedList<ExpressionVariable> parents;
	private LinkedList<ExpressionVariable> allVariables;
	private LinkedList<Expression> parameters;
	
	private LinkedHashSet<Family> families;
	private LinkedHashMap<Family, Integer> familiesCount;
	private LinkedHashMap<Family, Integer> parametersCount;
	
	public ExpressionBayesianNode(Expression expression, Context context, ExpressionVariable child, LinkedList<ExpressionVariable> parents, LinkedList<Expression> parameters) {
		super(expression, context);
		this.expression = expression;
		this.context = context;
		this.child = child;
		this.parents = parents;
		this.allVariables = mergeElementsIntoOneList(child, parents);
		this.parameters = parameters;
		this.families = computeFamilies();
	}

	private LinkedHashSet<Family> computeFamilies() {
		LinkedList<Family> initialFamilies = generateInitialFamilies();
		return computeFinalFamilies(initialFamilies);
	}

	private LinkedHashSet<Family> computeFinalFamilies(LinkedList<Family> initialFamilies) {
		LinkedHashSet<Family> finalFamilies = Util.set();
		
		while(!initialFamilies.isEmpty()) {
			Family family1 = initialFamilies.removeFirst();
			
			for(ListIterator<Family> it = initialFamilies.listIterator(); it.hasNext();) {
				Family family2 = it.next();
				
				// compare with family1 and analyse the three cases, for now throw Error in the case of partial intersection, print and debug
			}
			
			finalFamilies.add(family1);
		}
		
		return finalFamilies;
	}

	private LinkedList<Family> generateInitialFamilies() { 
		LinkedList<Family> initialFamilies = list();
		
		for(Expression parameter : parameters) {
			IndexExpressionsSet parameterIndexExpressionsSet = getIndexExpressionsForIndicesInListAndTypesInRegistry(list(parameter), context);
			Expression familyCondition = new DefaultExistentiallyQuantifiedFormula(parameterIndexExpressionsSet, Equality.make(expression, parameter));
			familyCondition = context.evaluate(familyCondition);
			Family family = new Family(familyCondition, Util.set(parameter));
			initialFamilies.add(family);
		}
		
		return initialFamilies;
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
		// HERE
		
		
	}
	
	@Override
	public void incrementCountForChildAndParentsAssignment(List<? extends Object> childAndParentsValues) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void normalizeParameters() {
		// TODO Auto-generated method stub
		
	}
	
	public LinkedHashSet<Family> getFamilies() {
		return this.families;
	}
	
	public static void main(String[] args) {
		
		Theory theory = new CommonTheory();
		
		Context context = new TrueContext(theory);
		
		// Only one child and one parent, 2 parameters (Param1 and Param2)
		
		ExpressionVariable child = new ExpressionVariable(parse("Child"));
		ExpressionVariable parent = new ExpressionVariable(parse("Parent"));
		Expression param1 = parse("Param1");
		Expression param2 = parse("Param2");
		
		context = context.extendWithSymbolsAndTypes("Child", "1..5", "Parent", "1..5", "Param1", "Real", "Param2", "Real");
		LinkedList<ExpressionVariable> parents = list(parent);
		LinkedList<Expression> parameters = list(param1, param2);
		
		Expression E = parse("if Child < 5 then Param1 else Param2");
		// Expression E = parse("if Parent != 5 then Param1 else Param2");
		
		ExpressionBayesianNode node = new ExpressionBayesianNode(E, context, child, parents, parameters);
		println(node.getFamilies());
		
	}

}
