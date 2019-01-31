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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * Implementation of Bayesian nodes based on Expressions
 * (The methods implemented here are to make it possible learning parameters for a Bayesian model given complete data.
 * The high-level algorithm is described in the method learnModelParametersFromCompleteData from the interface BayesianModel.java and 
 * consequently in the method setParametersGivenCompleteData from the interface BayesianNode.java, called by the first method)
 * 
 * @author Roger Leite Lucena
 *
 */

public class ExpressionBayesianNode extends DefaultExpressionFactor implements BayesianNode {

	private static final long serialVersionUID = 1L;
	
	private Expression expression;
	private Context context;
	private ExpressionVariable child;
	private List<ExpressionVariable> parents;
	private List<ExpressionVariable> allVariables; // child is the first one, parents are the others
	private LinkedHashSet<Expression> parameters;
	
	private LinkedHashSet<Family> families;
	private LinkedHashMap<Family, Expression> familyCountFromDataset;
	private LinkedHashMap<Pair<Family, Expression>, Expression> parameterCountFromDataset; // a count for every pair (family, parameter)
	private LinkedHashMap<Pair<Family, Expression>, Expression> finalParameterValues;
	
	// Useful variables used frequently:
	private IndexExpressionsSet childIndexExpressionsSet;
	
	public ExpressionBayesianNode(Expression expression, Context context, ExpressionVariable child, List<ExpressionVariable> parents, LinkedHashSet<Expression> parameters) {
		super(expression, context);
		this.expression = expression;
		this.context = context;
		this.child = child;
		this.parents = parents; 
		this.allVariables = mergeElementsIntoOneList(child, parents);
		this.parameters = parameters;
		makeTheParametersBecomeConstantsInsideTheExpression(); 
		
		this.familyCountFromDataset = new LinkedHashMap<Family, Expression>();
		this.parameterCountFromDataset = new LinkedHashMap<Pair<Family, Expression>, Expression>();
		this.finalParameterValues = new LinkedHashMap<Pair<Family, Expression>, Expression>();
		
		this.childIndexExpressionsSet = getIndexExpressionsForIndicesInListAndTypesInRegistry(list(child), context);
		
		this.families = computeFamilies();
	}
	
	@Override
	public ExpressionVariable getChildVariable() {
		return this.child;
	}

	@Override
	public List<ExpressionVariable> getParentsVariables() {
		return this.parents;
	}

	@Override
	public List<ExpressionVariable> getAllVariables() {
		return this.allVariables;
	}

	@Override
	public void setInitialCountsForAllPossibleChildAndParentsAssignments() {
		for(Family family : families) {
			familyCountFromDataset.put(family, Expressions.ZERO);
			setInitialCountForEveryParameterInThatFamilyAndIncrementCorrespondinglyTheCountForThatFamily(family);
		}
	}
	
	/**
	 * For every childAndParentsValues (must be a list of Expressions, represents a datapoint) we increment the counts for the corresponding family and parameter
	 */
	@Override
	public void incrementCountForChildAndParentsAssignment(List<? extends Object> childAndParentsValues) {
		if(thereAreNoParametersToLearn()) { // in this case there are no parameters to learn, we have a fixed prior probability for the node for example, then there is no point on incrementing (we would have errors since we have no families and so on)
			return;
		}
		
		verifyIfInputHasExpectedTypeAndSize(childAndParentsValues);
		@SuppressWarnings("unchecked")
		LinkedList<Expression> parentsValues = extractParentsValuesFrom((List<Expression>) childAndParentsValues);
		
		@SuppressWarnings("unchecked")
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
	
	public LinkedHashSet<Family> getFamilies() {
		return this.families;
	}
	
	@Override
	public Context getContext() {
		return this.context;
	}

	@Override
	public ExpressionBayesianNode copy() {
		ExpressionBayesianNode copy = new ExpressionBayesianNode(this.expression, this.context, this.child, this.parents, this.parameters);
		return copy;
	}
	
	/**
	 * Making the parameters become constants - important to forbid PRAiSE in considering the possibility of one parameter being equal to another (having the same value) in a logic evaluation, 
	 * since here parameters must be treated as symbolic literals, not variables
	 */
	private void makeTheParametersBecomeConstantsInsideTheExpression() {
		Predicate<Expression> isUniquelyNamedConstantPredicate = context.getIsUniquelyNamedConstantPredicate(); 
		Predicate<Expression> newIsUniquelyNamedConstantPredicate = s -> parameters.contains(s) || isUniquelyNamedConstantPredicate.apply(s);
		context = context.setIsUniquelyNamedConstantPredicate(newIsUniquelyNamedConstantPredicate);
	}

	private LinkedHashSet<Family> computeFamilies() {
		LinkedList<Family> initialFamilies = generateInitialFamilies();
		return computeFinalFamilies(initialFamilies);
	}
	
	/**
	 * Break the initial families into final families without any intersection (Shattering)
	 * 
	 * General idea of the shattering algorithm:
	 * We go through the list of families with 2 iterators, family1 (starts at the beginning of the list) and family2 (always starting at the right of family1), and compare then, leading to three possibilities:
	 * 1) if their conditions do not intersect at all then we continue the iteration (doing family2++)
	 * 2) if we have a total intersection (the family conditions are equivalent) then we add the parameters from family2 to family1 and delete family2 from the list
	 * 3) if we have partial intersection between the conditions - that is the tricky case, here we do:
	 * 		i) we create a new family to store this intersectionCondition, called here familyIntersection, with the parameters from both family1 and family2, and add it to the end of the list of families
	 * 		ii) we update the conditions of family1 and family2 as the disjunction between themselves and the intersectionCondition
	 * We continue the process above until family1 gets to the end of the list of families
	 * 
	 * @param initialFamilies
	 * 
	 * @return the set of the final families
	 */
	private LinkedHashSet<Family> computeFinalFamilies(LinkedList<Family> initialFamilies) {
		LinkedHashSet<Family> finalFamilies = Util.set();
		
		while(!initialFamilies.isEmpty()) {
			Family family1 = initialFamilies.removeFirst();
			
			verifyIntersectionBetweenGivenFamilyAndOthersInListAndDoCorrespondentOperation(family1, initialFamilies);
			
			if(!family1.condition.equals(Expressions.FALSE)) { // we add to finalFamilies only families that are not empty (their condition is not false)
				finalFamilies.add(family1);
			}
		}
		
		return finalFamilies;
	}
	
	/**
	 * For every family2 in otherFamilies we take the intersection with family1 and do the correspondent operation:
	 * 1) if intersection is empty we just continue iterating with family2 over otherFamilies
	 * 2) if we have total intersection then the families are equivalent regarding their condition over parents, so we add the parameters from family2 to family1 and remove family2 from the list
	 * 3) if we have partial intersection we handle it as a special case, see method handlePartialFamilyIntersection below
	 * 
	 * @param family1 - the given family to be compared with the others
	 * @param otherFamilies
	 */
	private void verifyIntersectionBetweenGivenFamilyAndOthersInListAndDoCorrespondentOperation(Family family1, LinkedList<Family> otherFamilies) {
		for(ListIterator<Family> it = otherFamilies.listIterator(); it.hasNext();) {
			Family family2 = it.next();
			Expression intersection = verifyEquivalenceAndGetIntersectionCondition(family1.condition, family2.condition);
			if(intersection.equals(Expressions.FALSE)) { // the families are totally disjunct
				continue; 
			}
			else if(intersection.equals(Expressions.TRUE)) { // we have total intersection between the families
				family1.addParameters(family2.parametersThatCanBeGenerated);
				it.remove();
			}
			else { // case of partial intersection
				handlePartialFamilyIntersection(family1, family2, intersection, otherFamilies);
				if(family1.condition.equals(Expressions.FALSE)) break; // optimization: if family1 is empty, we can break here already, there is no point on comparing it with any other families
			}
		}
	}

	/**
	 * Handling the case of partial intersection over the conditions of two families
	 * Here we do:
	 * 	i) we create a new family to store this intersectionCondition, called here familyIntersection, with the parameters from both family1 and family2, and add it to the end of the list of families
	 * 	ii) we update the conditions of family1 and family2 as the disjunction between themselves and the intersectionCondition
	 * 
	 * @param family1
	 * @param family2
	 * @param intersectionCondition
	 * @param otherFamilies
	 */
	private void handlePartialFamilyIntersection(Family family1, Family family2, Expression intersectionCondition, LinkedList<Family> otherFamilies) {
		Family familyIntersection = new Family(intersectionCondition, family1.parametersThatCanBeGenerated);
		familyIntersection.addParameters(family2.parametersThatCanBeGenerated);
		otherFamilies.add(familyIntersection);
		
		Expression newFamily1Condition = context.evaluate(And.make(family1.condition, Not.make(intersectionCondition)));
		family1.condition = newFamily1Condition;
		
		Expression newFamily2Condition = context.evaluate(And.make(family2.condition, Not.make(intersectionCondition)));
		family2.condition = newFamily2Condition;
	}
	
	/**
	 * Given two conditions as Expressions, verify if they are equivalent or return the condition for their intersection
	 * 
	 * @param condition1
	 * @param condition2
	 * @return true if total equivalence, false if the conditions do not intersect at all, and in the case of partial intersection the condition for this intersection is returned
	 */
	private Expression verifyEquivalenceAndGetIntersectionCondition(Expression condition1, Expression condition2) {
		Expression equivalence = Equivalence.make(condition1, condition2);
		equivalence = context.evaluate(equivalence);
		if(equivalence.equals(Expressions.TRUE)) {
			return equivalence;
		}
		else {
			Expression and = And.make(condition1, condition2);
			and = context.evaluate(and);
			return and;
		}
	}
	
	/**
	 * Generating one family for each parameter at the beginning, the associated condition being "there exists Child so that this.expression = currentParameter"
	 * 
	 * @return list of the initial families
	 */
	private LinkedList<Family> generateInitialFamilies() { 
		LinkedList<Family> initialFamilies = list();
		
		for(Expression parameter : parameters) {
			Expression expressionForFamilyCondition = Equality.make(expression, parameter);
			Expression familyCondition = new DefaultExistentiallyQuantifiedFormula(childIndexExpressionsSet, expressionForFamilyCondition);
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
		Expression multisetOfChildValuesThatMakesExpressionEqualsToThisParameter = new DefaultIntensionalMultiSet(childIndexExpressionsSet, child, Equality.make(expression, parameter));
		Expression numberOfChildValues = apply(CARDINALITY, multisetOfChildValuesThatMakesExpressionEqualsToThisParameter);
		
		return numberOfChildValues; 
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
	
	/**
	 * The first value on the list is the childValue, ignore it, all the others are parentsValues - add them to a list and return this list
	 * 
	 * @param childAndParentsValues
	 * @return parentsValues
	 */
	private LinkedList<Expression> extractParentsValuesFrom(List<Expression> childAndParentsValues) {
		LinkedList<Expression> parentsValues = list();
		int iterationCount = 0;
		for(ListIterator<Expression> it = childAndParentsValues.listIterator(); it.hasNext(); ) {
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
			Expression variableValue = itValue.next(); 
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
		
		println("Node: " + this.child);
		println("parentsValues: " + parentsValues);
		
		throw new Error("Family for parentsValues not found");
	}
	
	/**
	 * Replaces all the parameters in the original expression by their learned values, and evaluates it with this.context.evaluate().
	 * The new Expression is a conjunction of successive IfThenElses to create a "switch-case" structure,
	 * in which every case is the condition of a family and the result for that case is the original Expression with 
	 * the parameters that can be generated by that family substituted by their learned values 
	 */
	private void updateInnerExpressionWithTheLearnedParameters() {
		Expression newExpression = generateSwitchCaseStructureFromFamiliesConditionsForFinalExpression();
		
		newExpression = context.evaluate(newExpression);
		this.expression = newExpression;
		this.setInnerExpression(newExpression);
	}
	
	
	/**
	 * Simulates a switch-case like structure using successive IfThenElse expressions
	 * Every "case" here is a family condition, for which we associate an Expression that is the 
	 * original this.expression with the parameters associated to that family replaced by their learned values
	 * 
	 * @return the resulting switch-case structure for the final this.expression
	 */
	private Expression generateSwitchCaseStructureFromFamiliesConditionsForFinalExpression() {
		Expression newExpression = expression; 
		
		Expression previousIfThenElse = null;
		int iterationCount = 0;
		for(Iterator<Family> it = families.iterator(); it.hasNext(); ) {
			Family family = it.next();
			
			Expression expressionWithNewParametersForCurrentFamily = replaceInExpressionAllTheParametersFromThatFamilyByTheirLearnedValues(family);
			
			if(iterationCount == 0) {
				previousIfThenElse = newExpression = expressionWithNewParametersForCurrentFamily;
			}
			else {
				previousIfThenElse = newExpression = IfThenElse.make(family.condition, expressionWithNewParametersForCurrentFamily, previousIfThenElse);
			}
		
			iterationCount++;
		}
		
		return newExpression;
	}

	private Expression replaceInExpressionAllTheParametersFromThatFamilyByTheirLearnedValues(Family family) {
		Expression expressionWithNewParameters = expression;
		for(Expression parameter : family.parametersThatCanBeGenerated) {
			Expression learnedParameterValue = finalParameterValues.get(new Pair<Family, Expression>(family, parameter));
			expressionWithNewParameters = expressionWithNewParameters.replaceAllOccurrences(parameter, learnedParameterValue, context);
		}
		return expressionWithNewParameters;
	}

	private void computeTheFinalValuesOfTheParameters() {
		for(Family family : families) {
			computeTheFinalValuesOfTheParametersInThatFamily(family);
		}
	}

	private void computeTheFinalValuesOfTheParametersInThatFamily(Family family) {
		Expression familyCount = familyCountFromDataset.get(family);
		
		for(Expression parameter : family.parametersThatCanBeGenerated) {
			computeTheFinalValueForParameterInFamily(parameter, family, familyCount);
		}
	}

	private void computeTheFinalValueForParameterInFamily(Expression parameter, Family family, Expression familyCount) {
		Expression parameterCount = parameterCountFromDataset.get(new Pair<Family, Expression>(family, parameter));
		
		Expression parameterCountDividedByFamilyCount = Division.make(parameterCount, familyCount);
		Expression normalizationFactor = getNumberOfChildValuesThatMakeExpressionEqualsToThisParameter(parameter);
		Expression finalParameterValue = Division.make(parameterCountDividedByFamilyCount, normalizationFactor);
		finalParameterValue = context.evaluate(finalParameterValue);
		
		finalParameterValues.put(new Pair<Family, Expression>(family, parameter), finalParameterValue);
	}
	
	private boolean thereAreNoParametersToLearn() {
		return this.parameters.size() == 0;
	}
	
	private void setInitialCountForEveryParameterInThatFamilyAndIncrementCorrespondinglyTheCountForThatFamily(Family family) {
		for(Expression parameter : family.parametersThatCanBeGenerated) {
			Expression numberOfChildValuesThatMakeExpressionEqualsToThisParameter = getNumberOfChildValuesThatMakeExpressionEqualsToThisParameter(parameter);
			parameterCountFromDataset.put(new Pair<Family, Expression>(family, parameter), numberOfChildValuesThatMakeExpressionEqualsToThisParameter);
		
			incrementFamilyCount(family, numberOfChildValuesThatMakeExpressionEqualsToThisParameter);
		}
	}
	
	@SuppressWarnings("unused")
	public static void main(String[] args) {
		Theory theory = new CommonTheory();
		
		Context context = new TrueContext(theory);
				
		// Only one child and one parent, 2 parameters (Param1 and Param2)
		
		ExpressionVariable child = DefaultExpressionVariable.expressionVariable(parse("Child"));
		ExpressionVariable parent = DefaultExpressionVariable.expressionVariable(parse("Parent"));
		Expression param1 = parse("Param1");
		Expression param2 = parse("Param2");
		Expression param3 = parse("Param3");
		
		context = context.extendWithSymbolsAndTypes("Child", "1..5", "Parent", "1..5", "Param1", "Real", "Param2", "Real", "Param3", "Real");
		LinkedHashSet<Expression> parameters = Util.set(param1, param2);
		parameters.add(param3);
		
		// Expression E = parse("if Child < 5 then Param1 else Param2");
		Expression E = parse("if Parent != 5 then Param1 else Param2");
		// Expression E = parse("if Parent != 5 then if Child < 5 then Param1 else Param2 else Param3");
		// Expression E = parse("if Parent != 5 then if Child < Parent then Param1 else Param2 else Param3"); // partial intersection
		
		// Expression E = parse("if Child > 3 then Param1 else Param2");
		
		println("E = " + E + "\n");
		
		new ExpressionBayesianNode(E, context, parent, list(), parameters);
		ExpressionBayesianNode childNode = new ExpressionBayesianNode(E, context, child, list(parent), parameters);
		println("Families = " + childNode.getFamilies());
		
		childNode.setInitialCountsForAllPossibleChildAndParentsAssignments();
		
		// Incrementing from datapoints
		LinkedList<Expression> childAndParentsValues = list(parse("1"), parse("1"));
		
		int nIncrements = 0;
		for(int i = 1; i <= nIncrements; i++) {
			childNode.incrementCountForChildAndParentsAssignment(childAndParentsValues);
		}
		
		childNode.normalizeParameters();
		println("\nnew E = " + childNode.getInnerExpression());
		
		println("\nEnd of Program");
		
	}

}