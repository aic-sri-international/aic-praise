package com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.makeIndexExpression;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.mapIntoMap;
import static com.sri.ai.util.Util.mapValues;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.base.Pair.pair;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.UniversallyQuantifiedFormula;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.interpreter.Assignment;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMSortDeclaration;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMVariableDeclaration;
import com.sri.ai.util.NewIdentifierMaker;
import com.sri.ai.util.base.Pair;

public class FromRelationalToGroundHOGMExpressionBasedProblem {
	
	private static final LinkedList<String> BUILT_IN_TYPE_NAMES = list("Integer", "String", "Real", "Boolean");
	private static final LinkedList<String> BUILT_IN_FUNCTION_STRINGS = 
			list(
					"Normal", 
					"+", 
					"-", 
					"*", 
					"/", 
					"^",
					"=",
					"!=",
					"<",
					">",
					"<=",
					">=",
					"and",
					"or",
					"not",
					"=>",
					"<=>",
					"'if . then . else .'"
					);
	

	public static 
	Map<Assignment, HOGMExpressionBasedProblem>
	ground(HOGMExpressionBasedProblem problem) {
		
		HOGMExpressionBasedModel hogmExpressionBasedModel = problem.getHOGMExpressionBasedModel();
		Context context = hogmExpressionBasedModel.getContext();
		Expression query = problem.getQueryExpression();
		
		HOGMExpressionBasedModel groundedModel = ground(hogmExpressionBasedModel);
		
		Map<Assignment, HOGMExpressionBasedProblem> result = 
				makeMapFromAssignmentsToQueryFreeVariablesToGroundedProblems(
						query, groundedModel, context);
		
		return result;
	}

	public static IndexExpressionsSet getIndexExpressionsSet(Expression expression, Context context) {
		Pair<IndexExpressionsSet, Expression> indexExpressionsAndBody = getIndexExpressionsSetAndBody(expression);
		return indexExpressionsAndBody.first;
	}

	public static Expression groundNonQuantifiedExpressionWithAssignment(Expression expression, Assignment assignment, Context context) {
		return expression.replaceAllOccurrences(e -> groundExpression(e, assignment, context), context);
	}
	
	public static Pair<IndexExpressionsSet, Expression> getIndexExpressionsSetAndBody(Expression expression) {
		return explanationBlock("Getting index expressions and body for ", expression, code(() -> {
	
			List<Expression> indexExpressions = list();
			Expression current = expression;
			while (current.getSyntacticFormType().equals("For all")) {
				UniversallyQuantifiedFormula universallyQuantifiedCurrent = (UniversallyQuantifiedFormula) current;
				ExtensionalIndexExpressionsSet indexExpressionsSet = (ExtensionalIndexExpressionsSet) universallyQuantifiedCurrent.getIndexExpressions();
				indexExpressions.addAll(indexExpressionsSet.getList());
				current = universallyQuantifiedCurrent.getBody();
			}
			Expression body = current;
			ExtensionalIndexExpressionsSet indexExpressionsSet = new ExtensionalIndexExpressionsSet(indexExpressions);
			return pair(indexExpressionsSet, body);
	
		}), "Result: ", RESULT);
	}

	public static String getGroundedModelString(HOGMExpressionBasedModel model) {
		StringBuilder groundedModelString = new StringBuilder();
	
		groundedModelString.append(getSorts(model));
		groundedModelString.append(getVariableDeclarations(model, model.getHOGModel().getConstantDeclarations()));
		groundedModelString.append(getVariableDeclarations(model, model.getHOGModel().getRandomVariableDeclarations()));
		groundedModelString.append(getFactors(model, model.getHOGModel().getConditionedPotentials(), model.getContext()));
		
		String modelString = groundedModelString.toString();
		
		return modelString;
	}

	public static HOGMExpressionBasedModel ground(HOGMExpressionBasedModel hogmExpressionBasedModel) {
		String modelString = getGroundedModelString(hogmExpressionBasedModel);
		println(modelString);
		HOGMExpressionBasedModel groundedModel = new HOGMExpressionBasedModel(modelString);
		return groundedModel;
	}

	private static 
	Map<Assignment, HOGMExpressionBasedProblem> 
	makeMapFromAssignmentsToQueryFreeVariablesToGroundedProblems(
			Expression query,
			HOGMExpressionBasedModel groundedModel, 
			Context context) {
		
		Map<Assignment, Expression> fromAssignmentsToGroundQueries = 
				makeMapFromAssignmentsToGrounding(query, context);
		
		Map<Assignment, HOGMExpressionBasedProblem> fromAssignmentsToGroundedProblems =
				mapValues(fromAssignmentsToGroundQueries, q -> new HOGMExpressionBasedProblem(q, groundedModel));
		
		return fromAssignmentsToGroundedProblems;
	}
	
	private static String getSorts(HOGMExpressionBasedModel model) {
		return explanationBlock("Copying sorts", code(() -> {
			
			StringBuilder result = new StringBuilder();
			for (HOGMSortDeclaration sortDeclaration : model.getHOGModel().getSortDeclarations()) {
				if (!BUILT_IN_TYPE_NAMES.contains(sortDeclaration.getName().toString())) {
					result.append(sortDeclaration.toHOGMString() + "\n");
				}
			}
			result.append("\n");
			return result.toString();
			
		}));
	}
	
	private static String getVariableDeclarations(HOGMExpressionBasedModel model, List<? extends HOGMVariableDeclaration> randomVariableDeclarations) {
		return explanationBlock("Grounding variable declarations", code(() -> {

			StringBuilder result = new StringBuilder();
			for (HOGMVariableDeclaration declaration : randomVariableDeclarations) {
				if (!BUILT_IN_FUNCTION_STRINGS.contains(declaration.getName().toString())) {
					result.append(grounding(declaration, model.getContext()) + "\n");
				}
				else {
					result.append(declaration.toHOGMString() + "\n");
				}
			}
			return result.toString();

		}));
	}

	private static String grounding(HOGMVariableDeclaration declaration, Context context) {
		return explanationBlock("Grounding ", declaration, code(() -> {

			ExtensionalIndexExpressionsSet parameters = makeParameters(declaration, context);
			StringBuilder result = new StringBuilder();
			AssignmentsIterator parameterValues = new AssignmentsIterator(parameters, context);
			for (Assignment assignment : in(parameterValues)) {
				List<Expression> argumentValues = getArgumentValues(parameters, assignment);
				result.append(grounding(declaration, argumentValues, context) + "\n");
			}
			return result.toString();

		}));
	}

	private static List<Expression> getArgumentValues(ExtensionalIndexExpressionsSet parameters, Assignment assignment) {
		return mapIntoList(parameters.getList(), indexExpression -> assignment.get(indexExpression.get(0)));
	}

	private static ExtensionalIndexExpressionsSet makeParameters(HOGMVariableDeclaration declaration, Context context) {
		Function<String, String> newIdentifierMaker = new NewIdentifierMaker(s -> !context.getSymbols().contains(makeSymbol(s)));
		List<Expression> indexExpressions = mapIntoList(declaration.getParameterSorts(), e -> makeParameterIndexExpression(e, newIdentifierMaker, context));
		ExtensionalIndexExpressionsSet result = new ExtensionalIndexExpressionsSet(indexExpressions);
		return result;
	}
	
	private static Expression makeParameterIndexExpression(Expression sort, Function<String, String> newIdentifierMaker, Context context) {
		String identifier = newIdentifierMaker.apply("parameter");
		Expression indexExpression = makeIndexExpression(makeSymbol(identifier), sort);
		return indexExpression;
	}

	private static String grounding(HOGMVariableDeclaration declaration, Collection<Expression> argumentValues, Context context) {
		Expression functor = declaration.getName();
		String name = makeGroundVariableName(functor, argumentValues);
		Expression type = declaration.getRangeSort();
		String result = declaration.getHOGMModifier() + " " + name + ": " + type + ";";
		return result;
	}

	private static String makeGroundVariableName(Expression functor, Collection<Expression> argumentValues) {
		return explanationBlock("Grounding ", functor, "(", argumentValues, ")", code(() -> {

			StringBuilder name = new StringBuilder();
			name.append(functor.toString());
			if (!argumentValues.isEmpty()) {
				name.append("_");
			}
			argumentValues.forEach(a -> name.append("_" + a));
			return name.toString();

		}));
	}

	/** 
	 * Generates an expression lifting up a grounded variable.
	 * A grounded variable is represented with the name <code>functorOrSymbol__arg0_arg1_..._argn</code>.
	 * @param groundedVariable
	 * @return
	 */
	public static Expression makeRelationalExpressionFromGroundedVariable(Expression groundedVariable) {
		String name = groundedVariable.toString();
		int i = name.indexOf("__");
		if (i != -1) {
			String argumentsString = name.substring(i + 2);
			ArrayList<Expression> arguments = getArguments(argumentsString);
			Expression functor = makeSymbol(name.substring(0, i));
			return Expressions.apply(functor, arguments);
		}
		else {
			return makeSymbol(name);
		}
	}
	
	private static ArrayList<Expression> getArguments(String argumentsString) {
		ArrayList<Expression> arguments = arrayList();
		while (argumentsString.length() > 0) {
			boolean foundUnderscore;
			int end = argumentsString.indexOf("_");
			if (end == -1) {
				end = argumentsString.length();
				foundUnderscore = false;
			}
			else {
				foundUnderscore = true;
			}
			arguments.add(makeSymbol(argumentsString.substring(0, end)));
			argumentsString = argumentsString.substring(end + (foundUnderscore? 1 : 0));
		}
		return arguments;
	}

	private static String getFactors(HOGMExpressionBasedModel model, List<Expression> factors, Context context) {
		return join("\n", mapIntoList(factors, f -> groundFactor(f, context))) + "\n";
	}
	
	private static String groundFactor(Expression factor, Context context) {
		return explanationBlock("Grounding factor ", factor, code(() -> {
			
			if (IfThenElse.isIfThenElse(factor)) {
				return groundIfThenElse(factor, context);
			}
			else {
				return groundExpression(factor, context);
			}

		}));
	}

	private static String groundIfThenElse(Expression factor, Context context) {
		Expression actualFactor = getActualFactor(factor);
		return groundExpression(actualFactor, context);
	}

	private static Expression getActualFactor(Expression factor) {
		Expression actualFactor;
		Expression condition = IfThenElse.condition(factor);
		Expression thenBranch = IfThenElse.thenBranch(factor);
		Expression elseBranch = IfThenElse.elseBranch(factor);
		if (thenBranch.equals(ONE) && elseBranch.equals(ZERO)) {
			actualFactor = condition;
		}
		else {
			actualFactor = factor;
		}
		return actualFactor;
	}

	private static String groundExpression(Expression factor, Context context) {
		if (factor.getSyntacticFormType().equals("For all")) {
			return groundUniversallyQuantifiedExpression(factor, context);
		}
		else {
			return factor.toString() + ";";
		}
	}

	private static String groundUniversallyQuantifiedExpression(Expression expression, Context context) {
		Pair<IndexExpressionsSet, Expression> indexExpressionsAndBody = getIndexExpressionsSetAndBody(expression);
		IndexExpressionsSet indexExpressions = indexExpressionsAndBody.first;
		Expression body = indexExpressionsAndBody.second;
		AssignmentsIterator assignments = new AssignmentsIterator(indexExpressions, context);
		List<String> bodyGroundings = mapIntoList(assignments, a -> groundNonQuantifiedExpressionWithAssignment(body, a, context).toString() + ";");
		return join("\n", bodyGroundings);
	}

	private static Map<Assignment, Expression> makeMapFromAssignmentsToGrounding(Expression expression, Context context) {
		Pair<IndexExpressionsSet, Expression> indexExpressionsAndBody = getIndexExpressionsSetAndBody(expression);
		IndexExpressionsSet indexExpressions = indexExpressionsAndBody.first;
		Expression body = indexExpressionsAndBody.second;
		AssignmentsIterator assignments = new AssignmentsIterator(indexExpressions, context);
		Map<Assignment, Expression> result = mapIntoMap(assignments, a -> groundNonQuantifiedExpressionWithAssignment(body, a, context));
		return result;
	}

	private static Expression groundExpression(Expression expression, Assignment assignment, Context context) {
		Expression directValue = assignment.get(expression);
		if (directValue != null) {
			return directValue;
		}
		else if (isGroundableFunctionApplication(expression, context)) {
			Expression groundVariable = groundFunctionApplication(expression, assignment);
			return groundVariable;
		}
		else {
			return expression;
		}
	}

	private static boolean isGroundableFunctionApplication(Expression expression, Context context) {
		return 
				expression.getFunctor() != null
				&& 
				isNotApplicationOfBuiltInFunction(expression) 
				&& 
				context.getTypeOfRegisteredSymbol(expression.getFunctor()) instanceof FunctionType;
	}

	private static boolean isNotApplicationOfBuiltInFunction(Expression expression) {
		String functorString = expression.getFunctor().toString();
		boolean isBuiltInFunctionApplication = BUILT_IN_FUNCTION_STRINGS.contains(functorString);
		return ! isBuiltInFunctionApplication;
	}

	private static Expression groundFunctionApplication(Expression expression, Assignment assignment) {
		return explanationBlock("Grounding ", expression, " with ", assignment, code(() -> {

			List<Expression> argumentValues = mapIntoList(expression.getArguments(), a -> getAssignmentOrItself(a, assignment));
			Expression groundVariable = makeSymbol(makeGroundVariableName(expression.getFunctor(), argumentValues));
			return groundVariable;

		}), "Result is ", RESULT);
	}

	private static Expression getAssignmentOrItself(Expression expression, Assignment assignment) {
		return assignment.containsKey(expression)? assignment.get(expression) : expression;
	}

	public static IndexExpressionsSet getQueryIndexExpressionsSet(ExpressionBasedProblem problem) {
		return getIndexExpressionsSetAndBody(problem.getQueryExpression()).first;
	}

	public static Expression getQueryBody(ExpressionBasedProblem problem) {
		return getIndexExpressionsSetAndBody(problem.getQueryExpression()).second;
	}

}