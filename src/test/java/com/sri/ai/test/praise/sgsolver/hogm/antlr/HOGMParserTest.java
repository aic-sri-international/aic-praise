package com.sri.ai.test.praise.sgsolver.hogm.antlr;

import java.util.HashSet;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.sgsolver.hogm.antlr.HOGMParserWrapper;
import com.sri.ai.praise.sgsolver.model.HOGModelError;
import com.sri.ai.praise.sgsolver.model.HOGModelException;

public class HOGMParserTest {
	protected Parser parser;
	
	public HOGMParserTest() {
		parser = new HOGMParserWrapper();
	}
	
	@Test
	public void testSortDeclaration() {
		String string;
		string = "sort People: 1000, bob, ann, mary;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "People", "1000", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "bob", "ann", "mary"))),
							  null, null, null));
		
		string = "sort people: 1000, Bob, Ann, Mary;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "people", "1000", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "Bob", "Ann", "Mary"))),
							  null, null, null));

		string = "sort Dogs: 1000;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "Dogs", "1000", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))),
							  null, null, null));

		string = "sort Dogs: 1000, rover;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "Dogs", "1000", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "rover")),
							  null, null, null));

		string = "sort Cats: Unknown;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "Cats", "Unknown", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))),
							  null, null, null));

		string = "sort Rats;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "Rats", "Unknown", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))),
							  null, null, null));
		
		string = "sort rats;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "rats", "Unknown", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))),
							  null, null, null));
	}
	
	@Test
	public void testDetectedSortErrors() {
		// Should not parse
		test("sort People: -1;", null);
		test("sort People: Something;", null);
		
		testExpectedModelError( // Too many constants 
				"sort Grade: 7, a, b, c, d, e, f, g, ng;\n",
				HOGModelError.Type.SORT_DECLARATION_IS_NOT_LEGAL);
		
		// Predefined
		testExpectedModelError("sort Boolean: 2, false, true;", HOGModelError.Type.SORT_NAME_PREDEFINED);
		testExpectedModelError("sort Boolean: 2, true, false;", HOGModelError.Type.SORT_NAME_PREDEFINED);
		testExpectedModelError("sort Boolean;", HOGModelError.Type.SORT_NAME_PREDEFINED);
		
		testExpectedModelError("sort Number: Unknown;", HOGModelError.Type.SORT_NAME_PREDEFINED);
		testExpectedModelError("sort Number: 2;", HOGModelError.Type.SORT_NAME_PREDEFINED);
		testExpectedModelError("sort Number;", HOGModelError.Type.SORT_NAME_PREDEFINED);
		
		testExpectedModelError("sort 'if . then . else .';", HOGModelError.Type.SORT_NAME_SAME_AS_IN_BUILT_FUNCTOR);
		
		testExpectedModelError("sort 'for all . : .';", HOGModelError.Type.SORT_NAME_SAME_AS_QUANTIFIER);
		testExpectedModelError("sort 'there exists . : .';", HOGModelError.Type.SORT_NAME_SAME_AS_QUANTIFIER);
		
		testExpectedModelError(
				"sort People: 2, fred, tom;\n"
				+"sort People: Unknown;", HOGModelError.Type.SORT_NAME_NOT_UNIQUE);

		testExpectedModelError(
				 "sort People: Unknown, fred, washington;\n"
				+"sort Places: Unknown, washington;", HOGModelError.Type.SORT_CONSTANT_NAME_NOT_UNIQUE);
	}
	
	@Test
	public void testConstantDeclaration() {
		String string;
		string = "constant grade: Boolean x Boolean -> Boolean;";
		test(string, expected(null, 
							  Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("constant", "grade", "2", "Boolean", "Boolean", "Boolean"),
							  null, null));

		string = "constant father: Boolean -> Boolean;";
		test(string, expected(null,
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("constant", "father", "1", "Boolean", "Boolean"),
				              null, null));

		string = "constant happy: Boolean -> Boolean;";
		test(string, expected(null,
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("constant", "happy", "1", "Boolean", "Boolean"),
				              null, null));
		
		string = "constant Happy: Boolean -> Boolean;";
		test(string, expected(null,
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("constant", "Happy", "1", "Boolean", "Boolean"),
				              null, null));

		string = "constant president: Boolean;";
		test(string, expected(null, 
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("constant", "president", "0", "Boolean"),
				              null, null));
		
		string = "constant President: Boolean;";
		test(string, expected(null,
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("constant", "President", "0", "Boolean"),
				              null, null));	
	}
	
	@Test
	public void testDetectedConstantErrors() {
		test("constant times: Number x Number;", null);
		test("constant times: x Number;", null);
		
		testExpectedModelError(			 
				 "sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"constant grade: Number -> Grade;\n"
				+"constant grade: Number -> Boolean;", HOGModelError.Type.CONSTANT_NAME_NOT_UNIQUE);

		testExpectedModelError(			 
				 "sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"constant ng: Boolean;", HOGModelError.Type.CONSTANT_NAME_SAME_AS_UNIQUE_CONSTANT);
		
		testExpectedModelError(			 
				 "sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"constant Grade: Boolean;", HOGModelError.Type.CONSTANT_NAME_SAME_AS_SORT);

		test("constant not: Boolean;", null);
		test("constant and: Boolean;", null);
		test("constant or: Boolean;", null);
		testExpectedModelError("constant 'if . then . else .': Boolean;", HOGModelError.Type.CONSTANT_NAME_SAME_AS_IN_BUILT_FUNCTOR);
		test("constant sort: Boolean;", null);
		test("constant random: Boolean;", null);
		
		testExpectedModelError("constant 'for all . : .': Boolean;", HOGModelError.Type.CONSTANT_NAME_SAME_AS_QUANTIFIER);
		testExpectedModelError("constant 'there exists . : .': Boolean;", HOGModelError.Type.CONSTANT_NAME_SAME_AS_QUANTIFIER);
		
		
		testExpectedModelError("constant times: x -> Number;", HOGModelError.Type.CONSTANT_SORT_ARGUMENT_NOT_DECLARED);
		testExpectedModelError("constant location: Place;", HOGModelError.Type.CONSTANT_SORT_ARGUMENT_NOT_DECLARED);
		testExpectedModelError("constant location: Number x Number -> Place;", HOGModelError.Type.CONSTANT_SORT_ARGUMENT_NOT_DECLARED);

		testExpectedModelError( 
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"constant goodGrade: Grade -> Boolean;\n"
				+"goodGrade(55);",
				HOGModelError.Type.CONSTANT_ARGUMENT_IS_OF_THE_INCORRECT_TYPE);
		
		testExpectedModelError( 
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"constant goodGrade: Grade -> Boolean;\n"
				+"for all X in Boolean: goodGrade(X);",
				HOGModelError.Type.CONSTANT_ARGUMENT_IS_OF_THE_INCORRECT_TYPE);
		
		testExpectedModelError( 
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"constant goodGrade: Grade -> Boolean;\n"
				+"there exists X in Boolean: goodGrade(X);",
				HOGModelError.Type.CONSTANT_ARGUMENT_IS_OF_THE_INCORRECT_TYPE);
	}
	
	@Test
	public void testRandomVariableDeclaration() {
		String string;
		string = "random grade: Boolean x Boolean -> Boolean;";
		test(string, expected(null, null,
							  Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "grade", "2", "Boolean", "Boolean", "Boolean"),
							  null));

		string = "random father: Boolean -> Boolean;";
		test(string, expected(null, null, 
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "father", "1", "Boolean", "Boolean"),
				              null));
		
		string = "random Father: Boolean -> Boolean;";
		test(string, expected(null, null, 
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "Father", "1", "Boolean", "Boolean"),
				              null));

		string = "random happy: Boolean -> Boolean;";
		test(string, expected(null, null,
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "happy", "1", "Boolean", "Boolean"),
				              null));

		string = "random president: Boolean;";
		test(string, expected(null, null,
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "president", "0", "Boolean"),
				              null));
		
		string = "random President: Boolean;";
		test(string, expected(null, null,
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "President", "0", "Boolean"),
				              null));
	}
	
	@Test
	public void testDetectedRandomVariableErrors() {
		test("random times: Number x Number;", null);
		test("random times: x Number;", null);
		
		testExpectedModelError(			 
				 "sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random grade: Number -> Grade;\n"
				+"random grade: Number -> Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_NOT_UNIQUE);

		testExpectedModelError(			 
				 "sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random ng: Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_SAME_AS_UNIQUE_CONSTANT);
		
		testExpectedModelError(			 
				 "sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random Grade: Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_SAME_AS_SORT);
		
		testExpectedModelError(			 
				 "constant president: Boolean -> Boolean;\n"
				+"random president: Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_SAME_AS_CONSTANT);

		test("random not: Boolean;", null);
		test("random and: Boolean;", null);
		test("random or: Boolean;", null);
		testExpectedModelError("random 'if . then . else .': Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_SAME_AS_IN_BUILT_FUNCTOR);
		test("random sort: Boolean;", null);
		test("random random: Boolean;", null);
		
		testExpectedModelError("random 'for all . : .': Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_SAME_AS_QUANTIFIER);
		testExpectedModelError("random 'there exists . : .': Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_SAME_AS_QUANTIFIER);
		
		
		testExpectedModelError("random times: x -> Number;", HOGModelError.Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED);
		testExpectedModelError("random location: Place;", HOGModelError.Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED);
		testExpectedModelError("random location: Number x Number -> Place;", HOGModelError.Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED);

		testExpectedModelError( 
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random goodGrade: Grade -> Boolean;\n"
				+"goodGrade(55);",
				HOGModelError.Type.RANDOM_VARIABLE_ARGUMENT_IS_OF_THE_INCORRECT_TYPE);
		
		testExpectedModelError( 
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random goodGrade: Grade -> Boolean;\n"
				+"for all X in Boolean: goodGrade(X);",
				HOGModelError.Type.RANDOM_VARIABLE_ARGUMENT_IS_OF_THE_INCORRECT_TYPE);
		
		testExpectedModelError( 
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random goodGrade: Grade -> Boolean;\n"
				+"there exists X in Boolean: goodGrade(X);",
				HOGModelError.Type.RANDOM_VARIABLE_ARGUMENT_IS_OF_THE_INCORRECT_TYPE);
	}
	
	@Test
	public void testTerms() {
		String string;
		string = "for all X in Boolean: X = false or X = true;";
		test(string, expected(null, null, null,
							  Expressions.parse("if for all X in Boolean: X = false or X = true then 1 else 0")));
		string = "there exists X in Boolean: X;";
		test(string, expected(null, null, null,
							  Expressions.parse("if there exists X in Boolean: X then 1 else 0")));
		
		string = "(for all X in Boolean: X = false or X = true) 0.8;";
		test(string, expected(null, null, null,
							  Expressions.parse("if for all X in Boolean: X = false or X = true then 0.8 else 0.2")));
		
		string = "(there exists X in Boolean: X) 0.8;";
		test(string, expected(null, null, null,
							  Expressions.parse("if there exists X in Boolean: X then 0.8 else 0.2")));

		// 'not' is to have lower precedence than equality and arithmetic functors.
		string = "sort AlarmState: 2, on, off;\n"
				+"random alarm: AlarmState;\n"
				+"not alarm = on;";	
		test(string, expected(Expressions.parse("sort(AlarmState, 2, {on, off})"), 
							  null, 
							  Expressions.parse("randomVariable(alarm, 0, AlarmState)"),
							  Expressions.parse("if not (alarm = on) then 1 else 0")));
		

		// 'else' is to attach to the closest conditional.		
		string = "sort People : 10000000, rodrigo;\n"
				+"random lucky : Boolean;\n"
				+"random winner : People;\n"				
				+"if lucky then if winner = rodrigo then 1 else 0;";	
		test(string, expected(Expressions.parse("sort(People, 10000000, {rodrigo})"), 
							  null, 
							  Expressions.parse("tuple(randomVariable(lucky, 0, Boolean), randomVariable(winner, 0, People))"),
							  Expressions.parse("if lucky then (if winner = rodrigo then 1 else 0) else 0.5")));
	}
	
	@Test
	public void testDetectedStatementErrors() {
		testExpectedModelError("goodGrade(55);", 
				HOGModelError.Type.TERM_TYPE_OF_FUNCTOR_NOT_DECLARED, 
				HOGModelError.Type.TERM_NON_CONDITIONAL_STATEMENT_MUST_BE_OF_TYPE_BOOLEAN);		

		testExpectedModelError( 
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random goodGrade: Grade -> Boolean;\n"
				+"goodGrade(a, true);",
				HOGModelError.Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION);
		testExpectedModelError( 
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random goodGrade: Grade -> Boolean;\n"
				+"goodGrade;",
				HOGModelError.Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION);	
		testExpectedModelError( 
				"random president: Boolean;\n"
				+"president(true);",
				HOGModelError.Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION);
		
		
		testExpectedModelError("president = true;",
				HOGModelError.Type.TERM_CONSTANT_NOT_DEFINED);		
		
		testExpectedModelError( 
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random goodGrade: Grade -> Boolean;\n"
				+"goodGrade(a1);", // Can't add to sort as already too large
				HOGModelError.Type.TERM_CONSTANT_NOT_DEFINED);	
		testExpectedModelError("a1 and true;", HOGModelError.Type.TERM_CONSTANT_NOT_DEFINED);
		
		testExpectedModelError(
				"random president: Boolean;\n"
				+"if 1 then president else not president;",
				HOGModelError.Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE,
				HOGModelError.Type.TERM_CONDITONAL_STATEMENT_MUST_BE_OF_TYPE_NUMERIC);
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random grade: Number -> Grade;\n"
				+"grade(55) and (grade(55) = a);",
				HOGModelError.Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE);	
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random grade: Number -> Grade;\n"
				+"grade(55) + (grade(55) = a);",
				HOGModelError.Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE,
				HOGModelError.Type.TERM_NON_CONDITIONAL_STATEMENT_MUST_BE_OF_TYPE_BOOLEAN);		
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"for all X in Grade: X;",
				HOGModelError.Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE);
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"there exists X in Grade: X;",
				HOGModelError.Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE);
		
		testExpectedModelError(
				"true = 1;",
				HOGModelError.Type.TERM_ARGUMENTS_MUST_ALL_BE_OF_THE_SAME_TYPE);
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random grade: Number -> Grade;\n"
				+"grade(55) = 55;",
				HOGModelError.Type.TERM_ARGUMENTS_MUST_ALL_BE_OF_THE_SAME_TYPE);
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random grade: Number -> Grade;\n"
				+"random president: Boolean;"
				+"if president then grade(55) else 0.3;",
				HOGModelError.Type.TERM_ARGUMENTS_MUST_ALL_BE_OF_THE_SAME_TYPE,
				HOGModelError.Type.TERM_CONDITONAL_STATEMENT_MUST_BE_OF_TYPE_NUMERIC);
		
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"grade(55) = 55;",
				HOGModelError.Type.TERM_SORT_CANNOT_BE_DETERMINED,
				HOGModelError.Type.TERM_TYPE_OF_FUNCTOR_NOT_DECLARED);
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"for all X in Something: X = a;",
				HOGModelError.Type.TERM_CONSTANT_NOT_DEFINED,
				HOGModelError.Type.TERM_SORT_CANNOT_BE_DETERMINED);
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"there exists X in Something: X = a;",
				HOGModelError.Type.TERM_CONSTANT_NOT_DEFINED,
				HOGModelError.Type.TERM_SORT_CANNOT_BE_DETERMINED);
		
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random grade: Number -> Grade;\n"
				+"random president: Boolean;"
				+"if president then grade(55) else grade(22);",
				HOGModelError.Type.TERM_CONDITONAL_STATEMENT_MUST_BE_OF_TYPE_NUMERIC);		
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random grade: Number -> Grade;\n"
				+"grade(55);",
				HOGModelError.Type.TERM_NON_CONDITIONAL_STATEMENT_MUST_BE_OF_TYPE_BOOLEAN);
	}
	
	//
	// PROTECTED
	//
	protected Expression expected(Expression sortDeclarations, Expression constantDeclarations, Expression randomVariableDeclarations, Expression statements) {
		Expression result = Tuple.make(new Object[] {ensureTuple(sortDeclarations), ensureTuple(constantDeclarations), ensureTuple(randomVariableDeclarations), ensureTuple(statements)});
		return result;
	}
	
	protected Expression ensureTuple(Expression expr) {
		Expression result = null;
		if (expr == null) {
			result = Tuple.make(new Object[0]);
		}
		else if (Tuple.isTuple(expr)) {
			result = expr;
		}
		else {
			result = Tuple.make(expr);
		}
		return result;
	}
	
	protected void test (String input) {
		test(input, true);
	}
	
	protected void test (String input, Expression expectedResult) {
		test(input, true, true, expectedResult);
	}
	
	protected void testFail (String input) {
		test(input, false);
	}
	
	protected void test (String input, boolean expectSucceed) {
		test(input, expectSucceed, false, null);
	}

	protected void test (String input, boolean expectSucceed, boolean checkResult, Expression expectedResult) {
		Expression result = parser.parse(input);
		if (expectSucceed) {
			if (checkResult) {
				Assert.assertEquals(expectedResult, result);
			}
			else {
				Assert.assertNotNull(result);
			}
		}
		else {
			Assert.assertNull(result);
		}
	}
	
	protected void testExpectedModelError(String input, HOGModelError.Type... expectedModelErrorTypes) {
		try {
			parser.parse(input);
			Assert.fail(HOGModelException.class.getName()+" should have been thrown");
		}
		catch (HOGModelException modelException) {
			Set<HOGModelError.Type> expected = new HashSet<>();
			for (HOGModelError.Type e : expectedModelErrorTypes) {
				expected.add(e);
			}
			Set<HOGModelError.Type> detected = new HashSet<>();
			modelException.getErrors().forEach(me -> detected.add(me.getErrorType()));
			
			if (!detected.equals(expected)) {
				Assert.fail("HOGModelError.Types do not match\n"
						 +  "Expected="+expected+"\n"
						 +  "Detected="+detected+"\n"
						 +  "Errors  ="+modelException.getErrors());
			}
		}
	}
}
