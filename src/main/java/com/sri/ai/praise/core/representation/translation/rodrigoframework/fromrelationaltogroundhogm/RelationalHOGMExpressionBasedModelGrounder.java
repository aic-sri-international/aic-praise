package com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.makeIndexExpression;
import static com.sri.ai.praise.core.representation.translation.rodrigoframework.NonBooleanFactorError.assertExpressionIsValidFactor;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.base.Pair.pair;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.UniversallyQuantifiedFormula;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.interpreter.Assignment;
import com.sri.ai.grinder.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMSortDeclaration;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMVariableDeclaration;
import com.sri.ai.util.NewIdentifierMaker;
import com.sri.ai.util.base.Pair;

public class RelationalHOGMExpressionBasedModelGrounder {
	
	private HOGMExpressionBasedModel model;
	private List<String> groundedVariableNames;
	
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
	
	public RelationalHOGMExpressionBasedModelGrounder(HOGMExpressionBasedModel model) {
		this.model = model;
		this.groundedVariableNames = list();
		getGroundedModel();
	}

	public static RelationalHOGMExpressionBasedModelGrounder makeConverter(HOGMExpressionBasedModel model) {
		return new RelationalHOGMExpressionBasedModelGrounder(model);
	}
	
//	public Map<Assignment, HOGMExpressionBasedProblem> ground(HOGMExpressionBasedProblem problem) {
//		
//		HOGMExpressionBasedModel model = problem.getHOGMExpressionBasedModel();
//		
//		HOGMExpressionBasedModel hogmExpressionBasedModel = model;
//		Expression query = problem.getQueryExpression();
//		
//		HOGMExpressionBasedModel groundedModel = ground(model);
//		
//		Context context = hogmExpressionBasedModel.getContext();
//		Map<Assignment, HOGMExpressionBasedProblem> result = 
//				makeMapFromAssignmentsToQueryFreeVariablesToGroundedProblems(
//						query, groundedModel, context);
//		
//		return result;
//	}

	public static HOGMExpressionBasedModel ground(HOGMExpressionBasedModel model) {
		HOGMExpressionBasedModel groundedModel = makeConverter(model).getGroundedModel();
		return groundedModel;
	}

	public static String getGroundedModelString(HOGMExpressionBasedModel model) {
		String groundedModelString = makeConverter(model).getGroundedModelStringPrivate(model);
		return groundedModelString;
	}

	public static ExtensionalIndexExpressionsSet getIndexExpressionsSet(Expression expression, Context context) {
		Pair<ExtensionalIndexExpressionsSet, Expression> indexExpressionsAndBody = getIndexExpressionsSetAndBody(expression);
		return indexExpressionsAndBody.first;
	}

	public Expression groundNonQuantifiedExpressionWithAssignment(Expression expression, Assignment assignment, Context context) {
		return expression.replaceAllOccurrences(e -> groundExpression(e, assignment, null, context), context);
	}
	
	public static Pair<ExtensionalIndexExpressionsSet, Expression> getIndexExpressionsSetAndBody(Expression expression) {
		return explanationBlock("Getting index expressions and body for ", expression, code(() -> {

			if (expression.hasFunctor(FunctorConstants.PRODUCT)) {
				return getIndexExpressionsSetAndBodyForFunctionApplicationOnIntensionalSet(expression);
			}
			else {
				return getIndexExpressionsSetAndBodyForUniversallyQuantifiedExpression(expression);
			}

		}), "Result: ", RESULT);
	}

	public static Pair<ExtensionalIndexExpressionsSet, Expression> 
	getIndexExpressionsSetAndBodyForUniversallyQuantifiedExpression(
			Expression universallyQuantifiedExpression) {
		
		List<Expression> indexExpressions = list();
		Expression current = universallyQuantifiedExpression;
		while (current.getSyntacticFormType().equals("For all")) {
			UniversallyQuantifiedFormula universallyQuantifiedCurrent = (UniversallyQuantifiedFormula) current;
			ExtensionalIndexExpressionsSet indexExpressionsSet = (ExtensionalIndexExpressionsSet) universallyQuantifiedCurrent.getIndexExpressions();
			indexExpressions.addAll(indexExpressionsSet.getList());
			current = universallyQuantifiedCurrent.getBody();
		}
		Expression body = current;
		ExtensionalIndexExpressionsSet indexExpressionsSet = new ExtensionalIndexExpressionsSet(indexExpressions);
		return pair(indexExpressionsSet, body);
	}

	public static Pair<ExtensionalIndexExpressionsSet, Expression> 
	getIndexExpressionsSetAndBodyForFunctionApplicationOnIntensionalSet(
			Expression expression) {
		
		IntensionalSet set = (IntensionalSet) expression.get(0); // TODO won't work if expression is an intensional set but not an instance of IntensionalSet, such as a proxy or wrapper
		ExtensionalIndexExpressionsSet indexExpressionsSet = (ExtensionalIndexExpressionsSet) set.getIndexExpressions();
		Expression body = set.getHead();
		return pair(indexExpressionsSet, body);
	}

	private String getGroundedModelStringPrivate(HOGMExpressionBasedModel model) {
		
		StringBuilder groundedModelString = new StringBuilder();
	
		groundedModelString.append(getSorts(model));
		groundedModelString.append(getVariableDeclarations(model, model.getHOGModel().getConstantDeclarations()));
		groundedModelString.append(getVariableDeclarations(model, model.getHOGModel().getRandomVariableDeclarations()));
		groundedModelString.append(getFactors(model, model.getHOGModel().getConditionedPotentials(), model.getContext()));
		
		String modelString = groundedModelString.toString();
		
		return modelString;
	}

	private HOGMExpressionBasedModel groundedModel = null;
	
	public HOGMExpressionBasedModel getGroundedModel() {
		if (groundedModel == null) {
			String modelString = getGroundedModelStringPrivate(model);
			println(modelString);
			groundedModel = new HOGMExpressionBasedModel(modelString);
		}
		return groundedModel;
	}

//	private 
//	Map<Assignment, HOGMExpressionBasedProblem> 
//	makeMapFromAssignmentsToQueryFreeVariablesToGroundedProblems(
//			Expression query,
//			HOGMExpressionBasedModel groundedModel, 
//			Context context) {
//		
//		Map<Assignment, Expression> fromAssignmentsToGroundQueries = 
//				makeMapFromAssignmentsToGrounding(query, context);
//		
//		Map<Assignment, HOGMExpressionBasedProblem> fromAssignmentsToGroundedProblems =
//				mapValues(fromAssignmentsToGroundQueries, q -> new HOGMExpressionBasedProblem(q, groundedModel));
//		
//		return fromAssignmentsToGroundedProblems;
//	}
	
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
	
	private String getVariableDeclarations(HOGMExpressionBasedModel model, List<? extends HOGMVariableDeclaration> variableDeclarations) {
		return explanationBlock("Grounding variable declarations", code(() -> {

			StringBuilder result = new StringBuilder();
			for (HOGMVariableDeclaration declaration : variableDeclarations) {
				if (!BUILT_IN_FUNCTION_STRINGS.contains(declaration.getName().toString())) {
					result.append(groundedVariableDeclarationString(declaration, model.getContext()) + "\n");
				}
				else {
					result.append(declaration.toHOGMString() + "\n");
				}
			}
			return result.toString();

		}));
	}

	private String groundedVariableDeclarationString(HOGMVariableDeclaration declaration, Context context) {
		return explanationBlock("Grounding ", declaration, code(() -> {

			ExtensionalIndexExpressionsSet parameters = makeParameters(declaration, context);
			StringBuilder result = new StringBuilder();
			AssignmentsIterator parameterValues = new AssignmentsIterator(parameters, context);
			for (Assignment assignment : in(parameterValues)) {
				List<Expression> argumentValues = getArgumentValues(parameters, assignment);
				result.append(groundedVariableDeclarationString(declaration, argumentValues, context) + "\n");
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

	private String groundedVariableDeclarationString(HOGMVariableDeclaration declaration, Collection<Expression> argumentValues, Context context) {
		Expression functor = declaration.getName();
		String name = makeGroundedVariableName(functor, argumentValues);
		Expression type = declaration.getRangeSort();
		String result = declaration.getHOGMModifier() + " " + name + ": " + type + ";";
		groundedVariableNames.add(name);
		return result;
	}

	private static String makeGroundedVariableName(Expression functor, Collection<Expression> argumentValues) {
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

	private String getFactors(HOGMExpressionBasedModel model, List<Expression> factors, Context context) {
		return join("\n", mapIntoList(factors, f -> groundFactorIntoItsGroundings(f, context))) + "\n";
	}
	
	private String groundFactorIntoItsGroundings(Expression factor, Context context) {
		return explanationBlock("Grounding factor ", factor, code(() -> {
			
			assertExpressionIsValidFactor(factor, context);
			
			String factorString;
			if (IfThenElse.isIfThenElse(factor)) {
				factorString = groundIfThenElse(factor, context);
			}
			else {
				factorString = groundBooleanFactor(factor, context);
			}
			
			return factorString;

		}));
	}

	private String groundIfThenElse(Expression factor, Context context) {
		Expression actualFactor = getActualFactor(factor);
		return groundBooleanFactor(actualFactor, context);
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

	private String groundBooleanFactor(Expression factor, Context context) {
		Pair<ExtensionalIndexExpressionsSet, Expression> indexExpressionsAndBody = getIndexExpressionsSetAndBody(factor);
		ExtensionalIndexExpressionsSet indexExpressions = indexExpressionsAndBody.first;
		Expression body = indexExpressionsAndBody.second;
		AssignmentsIterator assignments = new AssignmentsIterator(indexExpressions, context);
		List<String> bodyGroundings = mapIntoList(assignments, a -> groundNonQuantifiedFactorWithAssignment(body, a, context).toString() + ";");
		return join("\n", bodyGroundings);
	}

	public Expression groundNonQuantifiedFactorWithAssignment(Expression factor, Assignment assignment, Context context) {
		Expression groundedFactor = groundNonQuantifiedExpressionWithAssignment(factor, assignment, context);
		Expression simplified = interpreter.apply(groundedFactor, context);
		return simplified;
	}

//	private Map<Assignment, Expression> makeMapFromAssignmentsToGrounding(Expression expression, Context context) {
//		Pair<IndexExpressionsSet, Expression> indexExpressionsAndBody = getIndexExpressionsSetAndBody(expression);
//		IndexExpressionsSet indexExpressions = indexExpressionsAndBody.first;
//		Expression body = indexExpressionsAndBody.second;
//		AssignmentsIterator assignments = new AssignmentsIterator(indexExpressions, context);
//		Map<Assignment, Expression> result = mapIntoMap(assignments, a -> groundNonQuantifiedExpressionWithAssignment(body, a, context));
//		return result;
//	}

	private Expression groundExpression(Expression expression, Assignment assignment, Expression enclosingRandomFunctionApplicationIfAny, Context context) {
		Expression directValue = assignment.get(expression);
		if (directValue != null) {
			return directValue;
		}
		else if (isGroundableFunctionApplication(expression, context)) {
			return groundFunctionApplication(expression, assignment, enclosingRandomFunctionApplicationIfAny, context);
		}
		else {
			return expression;
		}
	}

	private Expression groundFunctionApplication(
			Expression expression, 
			Assignment assignment,
			Expression enclosingRandomFunctionApplicationIfAny, 
			Context context) {
		
		if (enclosingRandomFunctionApplicationIfAny == null) {
			Expression groundVariable = groundFunctionApplication(expression, assignment, context);
			return groundVariable;
		}
		else {
			throw new Error("Nested random functions (structural uncertainty) are not currently supported, but got " + enclosingRandomFunctionApplicationIfAny);
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

//	private static class NonExistentVariableException extends RuntimeException {
//		private static final long serialVersionUID = 1L;
//	}
	
	private Expression groundFunctionApplication(Expression expression, Assignment assignment, Context context) {
		return explanationBlock("Grounding ", expression, " with ", assignment, code(() -> {

			List<Expression> argumentValues = mapIntoList(expression.getArguments(), a -> evaluateExpression(a, assignment, context));
			String variableName = makeGroundedVariableName(expression.getFunctor(), argumentValues);
			
//			if (!groundedVariableNames.contains(variableName)) {
//				throw new NonExistentVariableException();
//			}
			
			Expression groundVariable = makeSymbol(variableName);
			return groundVariable;

		}), "Result is ", RESULT);
	}

	private static BruteForceCommonInterpreter interpreter = new BruteForceCommonInterpreter();
	
	private static Expression evaluateExpression(Expression expression, Assignment assignment, Context context) {
		Context contextWithAssignment = Assignment.extendAssignments(assignment, context);
		return interpreter.apply(expression, contextWithAssignment);
	}

	public static IndexExpressionsSet getQueryIndexExpressionsSet(ExpressionBasedProblem problem) {
		return getIndexExpressionsSetAndBody(problem.getQueryExpression()).first;
	}

	public static Expression getQueryBody(ExpressionBasedProblem problem) {
		return getIndexExpressionsSetAndBody(problem.getQueryExpression()).second;
	}

}
