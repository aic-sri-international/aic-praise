package com.sri.ai.praise.core.representation.translation.rodrigoframework;

import static com.sri.ai.expresso.core.DefaultSymbol.createSymbol;
import static com.sri.ai.util.Util.mapIntegersIntoArrayList;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableVariable;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.IntegerIterator;

public class FromTableToExpressionFactorConverter {

	public ExpressionFactor convert(TableFactor tableFactor, Theory theory) {
		return convert(tableFactor, theory, true);
	}
	
	public ExpressionFactor convert(TableFactor tableFactor, Theory theory, boolean convertAsTreeBasedExpression) {
		Expression expression = makeExpressionEquivalentToTableFactor(tableFactor, convertAsTreeBasedExpression);
		Context context = makeContextWithVariablesFrom(tableFactor, theory);
		ExpressionFactor expressionFactor = new DefaultExpressionFactor(expression, context);
		return expressionFactor;
	}
	
	private Expression makeExpressionEquivalentToTableFactor(TableFactor tableFactor, boolean convertAsTreeBasedExpression) {
		List<Integer> cardinalities = mapIntoArrayList(tableFactor.getVariables(), TableVariable::getCardinality);
		CartesianProductIterator<Integer> assignmentsIterator = makeAssignmentsIterator(cardinalities);
		Expression expression;
		if(convertAsTreeBasedExpression)
		{
			int startingVariableIndex = 0;
			expression = ifThenElseTreeExpressionFromCurrentPositionOf(assignmentsIterator, tableFactor, startingVariableIndex);
		}
		else
		{
			expression = ifThenElseLinearTableExpressionFromCurrentPositionOf(assignmentsIterator, tableFactor);
		}
		return expression;
	}

	private CartesianProductIterator<Integer> makeAssignmentsIterator(List<Integer> cardinalities) {
		List<NullaryFunction<Iterator<Integer>>> valuesIteratorMakers = mapIntoList(cardinalities, c -> makeIteratorMakerFrom0To(c));
		CartesianProductIterator<Integer> assignmentsIterator = new CartesianProductIterator<>(valuesIteratorMakers);
		return assignmentsIterator;
	}

	private NullaryFunction<Iterator<Integer>> makeIteratorMakerFrom0To(Integer i) {
		return () -> new IntegerIterator(0, i);
	}

	private Expression ifThenElseLinearTableExpressionFromCurrentPositionOf(CartesianProductIterator<Integer> assignmentsIterator, TableFactor tableFactor) {
		myAssert(assignmentsIterator.hasNext(), () -> "ifThenElseExpressionFromCurrentPositionOf: requires assignmentsIterator to be non-empty");
		ArrayList<Integer> assignment = assignmentsIterator.next();
		Double potentionForAssignment = tableFactor.getEntryFor(assignment);
		Expression potentialExpression = createSymbol(potentionForAssignment);
		Expression result;
		boolean assignmentIsLastOneSoWeDontNeedToTestForIt = ! assignmentsIterator.hasNext();
		if (assignmentIsLastOneSoWeDontNeedToTestForIt) {
			result = potentialExpression;
		}
		else {
			Expression assignmentTestExpression = makeAssignmentTestExpression(tableFactor.getVariables(), assignment);
			Expression elseExpression = ifThenElseLinearTableExpressionFromCurrentPositionOf(assignmentsIterator, tableFactor);
			result = IfThenElse.make(assignmentTestExpression, potentialExpression, elseExpression);
		}
		return result;
	}
	
	private Expression ifThenElseTreeExpressionFromCurrentPositionOf(CartesianProductIterator<Integer> assignmentsIterator, TableFactor tableFactor, int variableIndex) {
		myAssert(assignmentsIterator.hasNext(), () -> "ifThenElseExpressionFromCurrentPositionOf: requires assignmentsIterator to be non-empty");
		
		ArrayList<TableVariable> variables = tableFactor.getVariables();
		int varCardinality = variables.get(variableIndex).getCardinality();
		int nextVariableIndex = variableIndex + 1;
		
		ArrayList<Expression> subBranchExpressions = new ArrayList<>(varCardinality);
		if(variableIndex == variables.size()-1)
		{
			for(int i = 0; i < varCardinality; ++i)
			{
				ArrayList<Integer> assignment = assignmentsIterator.next();
				Double potentialForAssignment = tableFactor.getEntryFor(assignment);
				Expression potentialExpression = createSymbol(potentialForAssignment);
				subBranchExpressions.add(potentialExpression);
			}
		}
		else
		{
			for(int i = 0; i < varCardinality; ++i)
			{
				Expression branchExpression = ifThenElseTreeExpressionFromCurrentPositionOf(assignmentsIterator, tableFactor, nextVariableIndex);
				subBranchExpressions.add(branchExpression);
			}
		}
		int firstAssignmentValue = 0;
		TableVariable variable = variables.get(variableIndex);
		Expression result = combineVariableSubBranchExpressionsIntoIfElseIFElse(variable, firstAssignmentValue, subBranchExpressions);
		
		return result;
	}
	
	private Expression combineVariableSubBranchExpressionsIntoIfElseIFElse(TableVariable variable, int assignment, ArrayList<Expression> subBranchExpressions)
	{
		Expression result;
		if(assignment == subBranchExpressions.size() - 1)
		{
			result = subBranchExpressions.get(assignment);
		}
		else
		{
			int nextAssignment = assignment + 1;
			Expression assignmentTestExpression = makeComparisonToAssignedValue(variable, assignment);
			Expression elseExpression = combineVariableSubBranchExpressionsIntoIfElseIFElse(variable, nextAssignment, subBranchExpressions);
			result = IfThenElse.make(assignmentTestExpression, subBranchExpressions.get(assignment), elseExpression);
		}
		return result;
	}

	private Expression makeAssignmentTestExpression(List<TableVariable> tableVariables, ArrayList<Integer> assignment) {
		ArrayList<Expression> individualVariableConditions = mapIntegersIntoArrayList(assignment.size(), i -> makeComparisonToAssignedValue(i, tableVariables, assignment));
		Expression result = And.make(individualVariableConditions);
		return result;
	}
	
	private Expression makeComparisonToAssignedValue(TableVariable tableVariable, int assignment) {
		Symbol assignedValueExpression = createSymbol(assignment);
		Symbol variableExpression = makeVariableExpression(tableVariable);
		Expression result = Equality.make(variableExpression, assignedValueExpression);
		return result;
	}

	private Expression makeComparisonToAssignedValue(Integer variableIndex, List<TableVariable> tableVariables, ArrayList<Integer> assignment) {
		Integer assignedValue = assignment.get(variableIndex);
		Symbol assignedValueExpression = createSymbol(assignedValue);
		TableVariable tableVariable = tableVariables.get(variableIndex);
		Symbol variableExpression = makeVariableExpression(tableVariable);
		Expression result = Equality.make(variableExpression, assignedValueExpression);
		return result;
	}

	Symbol makeVariableExpression(TableVariable tableVariable) {
		Symbol result = createSymbol(tableVariable.getName());
		return result;
	}

	private Context makeContextWithVariablesFrom(TableFactor tableFactor, Theory theory) {
		Context context = new TrueContext(theory);
		for (TableVariable tableVariable : tableFactor.getVariables()) {
			context = register(tableVariable, context);
		}
		return context;
	}

	private Context register(TableVariable tableVariable, Context context) {
		Expression variableExpression = makeExpressionVariable(tableVariable);
		Expression typeExpression = makeTypeExpression(tableVariable);
		Context result = context.extendWithSymbolsAndTypes(variableExpression, typeExpression);
		return result;
	}

	Expression makeExpressionVariable(TableVariable tableVariable) {
		Symbol variableExpression = createSymbol(tableVariable.getName());
		return variableExpression;
	}

	Expression makeTypeExpression(TableVariable tableVariable) {
		int cardinality = tableVariable.getCardinality();
		Expression typeExpression = new IntegerInterval(0, cardinality-1).toExpression();
		return typeExpression;
	}

}
