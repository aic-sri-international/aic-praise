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
	
	private Theory theory;
	
	public FromTableToExpressionFactorConverter(Theory theory) {
		this.theory = theory;
	}

	public ExpressionFactor convert(TableFactor tableFactor) {
		Expression expression = makeExpressionEquivalentToTableFactor(tableFactor);
		Context context = makeContextWithVariablesFrom(tableFactor);
		ExpressionFactor expressionFactor = new DefaultExpressionFactor(expression, context);
		return expressionFactor;
	}

	private Expression makeExpressionEquivalentToTableFactor(TableFactor tableFactor) {
		List<Integer> cardinalities = mapIntoArrayList(tableFactor.getVariables(), TableVariable::getCardinality);
		CartesianProductIterator<Integer> assignmentsIterator = makeAssignmentsIterator(cardinalities);
		Expression expression = ifThenElseExpressionFromCurrentPositionOf(assignmentsIterator, tableFactor);
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

	private Expression ifThenElseExpressionFromCurrentPositionOf(CartesianProductIterator<Integer> assignmentsIterator, TableFactor tableFactor) {
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
			Expression elseExpression = ifThenElseExpressionFromCurrentPositionOf(assignmentsIterator, tableFactor);
			result = IfThenElse.make(assignmentTestExpression, potentialExpression, elseExpression);
		}
		return result;
	}

	private Expression makeAssignmentTestExpression(List<TableVariable> tableVariables, ArrayList<Integer> assignment) {
		ArrayList<Expression> individualVariableConditions = mapIntegersIntoArrayList(assignment.size(), i -> makeComparisonToAssignedValue(i, tableVariables, assignment));
		Expression result = And.make(individualVariableConditions);
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

	private Context makeContextWithVariablesFrom(TableFactor tableFactor) {
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
		Expression typeExpression = new IntegerInterval(0, cardinality).toExpression();
		return typeExpression;
	}

}
