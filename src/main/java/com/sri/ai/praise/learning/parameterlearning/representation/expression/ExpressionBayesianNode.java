package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsForIndicesInListAndTypesInRegistry;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mergeElementsIntoOneList;
import static com.sri.ai.util.Util.println;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * Implementation of Bayesian nodes based on Expressions
 * 
 * @author Roger Leite Lucena
 *
 */

public class ExpressionBayesianNode extends DefaultExpressionFactor implements BayesianNode {

	private static final long serialVersionUID = 1L;
	
	private Expression expression;
	private Context context;
	private ExpressionVariable child;
	private LinkedList<ExpressionVariable> parents;
	private LinkedList<ExpressionVariable> allVariables; // child is the first one, parents are the others
	private LinkedHashSet<Expression> parameters;
	
	private LinkedHashSet<Family> families;
	private LinkedHashMap<Family, Expression> familyCountFromDataset;
	private LinkedHashMap<Pair<Family, Expression>, Expression> parameterCountFromDataset; // a count for every pair (family, parameter)
	private LinkedHashMap<Pair<Family, Expression>, Expression> finalParameterValues;
	
	// Useful variables used frequently:
	private IndexExpressionsSet childIndexExpressionsSet;
	
	public ExpressionBayesianNode(Expression expression, Context context, ExpressionVariable child, LinkedList<ExpressionVariable> parents, LinkedHashSet<Expression> parameters) {
		super(expression, context);
		this.expression = expression;
		this.context = context;
		this.child = child;
		this.parents = parents;
		this.allVariables = mergeElementsIntoOneList(child, parents);
		this.parameters = parameters;
		makeTheParamentersBecomeConstantsInsideTheExpression();
		this.families = computeFamilies();
		
		this.familyCountFromDataset = new LinkedHashMap<Family, Expression>();
		this.parameterCountFromDataset = new LinkedHashMap<Pair<Family, Expression>, Expression>();
		this.finalParameterValues = new LinkedHashMap<Pair<Family, Expression>, Expression>();
		
		this.childIndexExpressionsSet = getIndexExpressionsForIndicesInListAndTypesInRegistry(list(child), context);
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
		for(Family family : families) {
			familyCountFromDataset.put(family, Expressions.ZERO);
			for(Expression parameter : family.parametersThatCanBeGenerated) {
				Expression numberOfChildValuesThatMakeExpressionEqualsToThisParameter = getNumberOfChildValuesThatMakeExpressionEqualsToThisParameter(parameter);
				parameterCountFromDataset.put(new Pair<Family, Expression>(family, parameter), numberOfChildValuesThatMakeExpressionEqualsToThisParameter);
				
				incrementFamilyCount(family, numberOfChildValuesThatMakeExpressionEqualsToThisParameter);
			}
		}
	}
	
	/**
	 * For every childAndParentsValue (must be an Expression) we increment the counts for the corresponding family and parameter
	 */
	@Override
	public void incrementCountForChildAndParentsAssignment(List<? extends Object> childAndParentsValues) {
		verifyIfInputHasExpectedTypeAndSize(childAndParentsValues);
		LinkedList<Expression> parentsValues = extractParentsValuesFrom((List<Expression>) childAndParentsValues);
		
		Expression expressionEvaluation = replaceVariablesByTheirValuesInAnExpression(expression, allVariables, (List<Expression>) childAndParentsValues);		
		expressionEvaluation = context.evaluate(expressionEvaluation);
		
		Family family = findCorrespondentFamilyFor(parentsValues);
		incrementFamilyCount(family, Expressions.ONE);
		incrementParameterCountByOne(expressionEvaluation, family);
	}

	@Override
	public void normalizeParameters() {
		computeTheFinalValuesOfTheParameters();
		updateInnerExpressionWithTheLearnedParameters();
	}
	
	/**
	 * Replaces all the parameters in the original expression by their learned values, and evaluates it with this.context.evaluate().
	 * The new Expression is a conjunction of successive IfThenElses to create a "switch case" structure,
	 * in which every case is the condition of a family and the result for that case is the original Expression with 
	 * the parameters that can be generated by that family substituted by their learned values 
	 */
	private void updateInnerExpressionWithTheLearnedParameters() {
		Expression newExpression = expression; 
		
		Expression previousIfThenElse = null;
		int iterationCount = 0;
		for(Iterator<Family> it = families.iterator(); it.hasNext(); ) {
			Family family = it.next();
			
			// println("\nCurrent family: " + family.condition);
			
			Expression expressionWithNewParameters = expression;
			for(Expression parameter : family.parametersThatCanBeGenerated) {
				Expression learnedParameterValue = finalParameterValues.get(new Pair<Family, Expression>(family, parameter));
				expressionWithNewParameters = expressionWithNewParameters.replaceAllOccurrences(parameter, learnedParameterValue, context);
				// println(parameter + " = " + learnedParameterValue);
			}
			
			expressionWithNewParameters = context.evaluate(expressionWithNewParameters);
			// println("Expression with new parameters: " + expressionWithNewParameters);
			
			if(iterationCount == 0) {
				previousIfThenElse = newExpression = expressionWithNewParameters;
			}
			else {
				previousIfThenElse = newExpression = IfThenElse.make(family.condition, expressionWithNewParameters, previousIfThenElse);
			}
			
			// println("New expression: " + newExpression);
		
			iterationCount++;
		}
		
		newExpression = context.evaluate(newExpression);
		this.innerExpression = expression = newExpression;
	}

	private void computeTheFinalValuesOfTheParameters() {
		for(Family family : families) {
			Expression familyCount = familyCountFromDataset.get(family);
			
			for(Expression parameter : family.parametersThatCanBeGenerated) {
				Expression parameterCount = parameterCountFromDataset.get(new Pair<Family, Expression>(family, parameter));
				
				Expression parameterCountDividedByFamilyCount = Division.make(parameterCount, familyCount);
				Expression normalizationFactor = getNumberOfChildValuesThatMakeExpressionEqualsToThisParameter(parameter);
				Expression finalParameterValue = Division.make(parameterCountDividedByFamilyCount, normalizationFactor);
				finalParameterValue = context.evaluate(finalParameterValue);
				
				finalParameterValues.put(new Pair<Family, Expression>(family, parameter), finalParameterValue);
			}
		}
	}
	
	public LinkedHashSet<Family> getFamilies() {
		return this.families;
	}
	
	/**
	 * Making the parameters become constants - important to forbid PRAiSE in considering the possibility of one parameter being equal to another (having the same value) in a logic evaluation, 
	 * since here parameters must be treated as symbolic literals, not variables
	 */
	private void makeTheParamentersBecomeConstantsInsideTheExpression() {
		Predicate<Expression> isUniquelyNamedConstantPredicate = context.getIsUniquelyNamedConstantPredicate();
		Predicate<Expression> newIsUniquelyNamedConstantPredicate = s -> parameters.contains(s) || isUniquelyNamedConstantPredicate.apply(s);
		context = context.setIsUniquelyNamedConstantPredicate(newIsUniquelyNamedConstantPredicate);
	}

	private LinkedHashSet<Family> computeFamilies() {
		LinkedList<Family> initialFamilies = generateInitialFamilies();
		return computeFinalFamilies(initialFamilies);
	}
	
	/**
	 * Break the initial families into final families without any intersection (shattering)
	 * 
	 * @param initialFamilies
	 * 
	 * @return the set of the final families
	 */
	private LinkedHashSet<Family> computeFinalFamilies(LinkedList<Family> initialFamilies) {
		LinkedHashSet<Family> finalFamilies = Util.set();
		
		while(!initialFamilies.isEmpty()) {
			Family family1 = initialFamilies.removeFirst();
			
			for(ListIterator<Family> it = initialFamilies.listIterator(); it.hasNext();) {
				Family family2 = it.next();
				Expression intersection = verifyEquivalenceAndGetIntersectionCondition(family1.condition, family2.condition);
				if(intersection.equals(Expressions.FALSE)) { // the families are totally disjunct
					continue; 
				}
				else if(intersection.equals(Expressions.TRUE)) { // we have total intersection between the families
					family1.addParameters(family2.parametersThatCanBeGenerated);
					it.remove();
				}
				else {
					Family familyIntersection = new Family(intersection, family1.parametersThatCanBeGenerated);
					familyIntersection.addParameters(family2.parametersThatCanBeGenerated);
					initialFamilies.add(familyIntersection);
					
					Expression newFamily1Condition = context.evaluate(And.make(family1.condition, Not.make(intersection)));
					family1.condition = newFamily1Condition;
					
					Expression newFamily2Condition = context.evaluate(And.make(family2.condition, Not.make(intersection)));
					family2.condition = newFamily2Condition;
					
					if(family1.condition.equals(Expressions.FALSE)) {
						break;
					}
				}
			}
			
			if(!family1.condition.equals(Expressions.FALSE)) {
				finalFamilies.add(family1);
			}
			
		}
		
		return finalFamilies;
	}
	
	private LinkedHashSet<Family> computeFinalFamiliesWithoutPartialIntersections(LinkedList<Family> initialFamilies) {
		LinkedHashSet<Family> finalFamilies = Util.set();
		
		while(!initialFamilies.isEmpty()) {
			Family family1 = initialFamilies.removeFirst();
			
			for(ListIterator<Family> it = initialFamilies.listIterator(); it.hasNext();) {
				Family family2 = it.next();
				Expression intersection = verifyEquivalenceAndGetIntersectionCondition(family1.condition, family2.condition);
				if(intersection.equals(Expressions.FALSE)) { // the families are totally disjunct
					continue; 
				}
				else if(intersection.equals(Expressions.TRUE)) { // we have total intersection between the families
					family1.addParameters(family2.parametersThatCanBeGenerated);
					it.remove();
				}
				else {
					throw new Error("Partial intersections are not handled yet");
				}
			}
		
			finalFamilies.add(family1);
		}
		
		return finalFamilies;
	}
	
	/**
	 * Given two conditions as Expressions, verify if they are equivalent or return the condition for their intersection
	 * @param e1
	 * @param e2
	 * @return true if total equivalence, false if the conditions do not intersect, and in the case of partial intersection the condition for this intersection is returned
	 */
	private Expression verifyEquivalenceAndGetIntersectionCondition(Expression e1, Expression e2) {
		Expression equivalence = Equivalence.make(e1, e2);
		equivalence = context.evaluate(equivalence);
		if(equivalence.equals(Expressions.TRUE)) {
			return equivalence;
		}
		else {
			Expression and = And.make(e1, e2);
			and = context.evaluate(and);
			return and;
		}
	}
	
	/**
	 * Generating one family for each parameter at the beginning, the associated condition being "there exists Child so that Expression = currentParameter"
	 * 
	 * @return list of the initial families
	 */
	private LinkedList<Family> generateInitialFamilies() { 
		LinkedList<Family> initialFamilies = list();
		
		for(Expression parameter : parameters) {
			IndexExpressionsSet childIndexExpressionsSet = getIndexExpressionsForIndicesInListAndTypesInRegistry(list(child), context);
			Expression familyCondition = new DefaultExistentiallyQuantifiedFormula(childIndexExpressionsSet, Equality.make(expression, parameter));
			familyCondition = context.evaluate(familyCondition);
			Family family = new Family(familyCondition, Util.set(parameter));
			initialFamilies.add(family);
		}
		
		return initialFamilies;
	}
	
	private void incrementFamilyCount(Family family, Expression increment) {
		Expression oldFamilyCount = familyCountFromDataset.get(family);
		Expression newFamilyCount = Plus.make(oldFamilyCount, increment);
		familyCountFromDataset.put(family, newFamilyCount);
	}

	private Expression getNumberOfChildValuesThatMakeExpressionEqualsToThisParameter(Expression parameter) {
		Expression multisetOfChildValuesThatMakeExpressionEqualsToThisParameter = new DefaultIntensionalMultiSet(childIndexExpressionsSet, child, Equality.make(expression, parameter));
		return apply(CARDINALITY, multisetOfChildValuesThatMakeExpressionEqualsToThisParameter);
	}
	
	private void verifyIfInputHasExpectedTypeAndSize(List<? extends Object> childAndParentsValues) throws Error {
		boolean valuesAreExpressions = childAndParentsValues.isEmpty() || childAndParentsValues.get(0) instanceof Expression;
		if(!valuesAreExpressions) {
			throw new Error("Values for BayesianVariables must be Expressions here.");
		}
		if(childAndParentsValues.size() != allVariables.size()) {
			throw new Error("The list of variableValues must have the same size as the list this.allVariables");
		}
	}
	
	private LinkedList<Expression> extractParentsValuesFrom(List<Expression> childAndParentsValues) {
		LinkedList<Expression> parentsValues = list();
		int iterationCount = 0;
		for(ListIterator<Expression> it = (ListIterator<Expression>) childAndParentsValues.listIterator(); it.hasNext(); ) {
			Expression value = it.next();
			if(iterationCount != 0) {
				parentsValues.add(value);
			}
			iterationCount++;
		}
		
		return parentsValues;
	}

	private void incrementParameterCountByOne(Expression parameter, Family family) {
		Pair<Family, Expression> familyAndParameter = new Pair<Family, Expression>(family, parameter);
		Expression oldParameterCount = parameterCountFromDataset.get(familyAndParameter);
		Expression newParameterCount = Plus.make(oldParameterCount, Expressions.ONE);
		parameterCountFromDataset.put(familyAndParameter, newParameterCount);
	}
	
	private Expression replaceVariablesByTheirValuesInAnExpression(Expression originalExpression, List<ExpressionVariable> variables, List<Expression> variableValues) {
		if(variables.size() != variableValues.size()) {
			throw new Error("The list for variables and the one for their values must have the same size");
		}
		
		Expression newExpression = originalExpression;
		ListIterator<ExpressionVariable> itVariable = variables.listIterator();
		ListIterator<Expression> itValue = variableValues.listIterator();
		
		while(itValue.hasNext() && itValue.hasNext()) {
			ExpressionVariable variable = itVariable.next();
			Expression variableValue = (Expression) itValue.next(); 
			newExpression = newExpression.replaceAllOccurrences(variable, variableValue, context);
		}
		
		return newExpression;
	}

	private Family findCorrespondentFamilyFor(LinkedList<Expression> parentsValues) {
		for(Family family : families) {
			Expression familyConditionEvaluated = replaceVariablesByTheirValuesInAnExpression(family.condition, parents, parentsValues);
			familyConditionEvaluated = context.evaluate(familyConditionEvaluated);
			if(familyConditionEvaluated.equals(Expressions.TRUE)) {
				return family;
			}
		}
		
		throw new Error("Family for parentsValues not found");
	}
	
	public static void main(String[] args) {
		Theory theory = new CommonTheory();
		
		Context context = new TrueContext(theory);
				
		// Only one child and one parent, 2 parameters (Param1 and Param2)
		
		ExpressionVariable child = new DefaultExpressionVariable(parse("Child"));
		ExpressionVariable parent = new DefaultExpressionVariable(parse("Parent"));
		Expression param1 = parse("Param1");
		Expression param2 = parse("Param2");
		Expression param3 = parse("Param3");
		
		context = context.extendWithSymbolsAndTypes("Child", "1..5", "Parent", "1..5", "Param1", "Real", "Param2", "Real", "Param3", "Real");
		LinkedList<ExpressionVariable> parents = list(parent);
		LinkedHashSet<Expression> parameters = Util.set(param1, param2);
		// parameters.add(param3);
		
		Expression E = parse("if Child < 5 then Param1 else Param2");
		// Expression E = parse("if Parent != 5 then Param1 else Param2");
		// Expression E = parse("if Parent != 5 then if Child < 5 then Param1 else Param2 else Param3");
		// Expression E = parse("if Parent != 5 then if Child < Parent then Param1 else Param2 else Param3"); // partial intersection
		
		println("E = " + E + "\n");
		
		ExpressionBayesianNode node = new ExpressionBayesianNode(E, context, child, parents, parameters);
		println("Families = " + node.getFamilies());
		
		node.setInitialCountsForAllPossibleChildAndParentsAssignments();
		
		// Incrementing from datapoints
		LinkedList<Expression> childAndParentsValues = list(parse("1"), parse("1"));
		
		int nIncrements = 0;
		for(int i = 1; i <= nIncrements; i++) {
			node.incrementCountForChildAndParentsAssignment(childAndParentsValues);
		}
		
		node.normalizeParameters();
		println("\nnew E = " + node.getInnerExpression());
		
		println("\nEnd of Program");
		
	}

}
