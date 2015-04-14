package com.sri.ai.test.praise.sgsolver.hogm.antlr;

import java.util.HashSet;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
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
							  null, null));

		string = "sort Dogs: 1000;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "Dogs", "1000", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))),
							  null, null));

		string = "sort Dogs: 1000, rover;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "Dogs", "1000", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "rover")),
							  null, null));

		string = "sort Cats: Unknown;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "Cats", "Unknown", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))),
							  null, null));

		string = "sort Rats;";
		test(string, expected(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "Rats", "Unknown", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))),
							  null, null));
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
		
		testExpectedModelError(
				"sort People: 2, fred, tom;\n"
				+"sort People: Unknown;", HOGModelError.Type.SORT_NAME_NOT_UNIQUE);

		testExpectedModelError(
				 "sort People: Unknown, fred, washington;\n"
				+"sort Places: Unknown, washington;", HOGModelError.Type.CONSTANT_NAME_NOT_UNIQUE);
	}
	
	@Test
	public void testAssociateUnassignedSortsToToSorts() {
		String string;
		string = "sort People: 1000, bob, ann, mary;\n"
				+"random partner: People -> People;\n"
				+"tom = partner(ann);";
		test(string, expected(Tuple.make(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "People", "1000", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "bob", "ann", "mary", "tom")))
							), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "partner", "1", "People", "People"), 
							IfThenElse.make(
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "tom", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partner", "ann")),
									Expressions.ONE, Expressions.ZERO)
							));
		string = "sort People: 1000, bob, ann, mary;\n"
				+"random partner: People -> People;\n"
				+"tom != partner(ann);";
		test(string, expected(Tuple.make(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "People", "1000", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "bob", "ann", "mary", "tom")))
							), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "partner", "1", "People", "People"), 
							IfThenElse.make(
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "tom", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partner", "ann")),
									Expressions.ONE, Expressions.ZERO)
							));
		
		string = "sort People: 1000, bob, ann, mary;\n"
				+"random partner: People -> People;\n"
				+"ann = partner(tom);";
		test(string, expected(Tuple.make(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "People", "1000", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "bob", "ann", "mary", "tom")))
							), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "partner", "1", "People", "People"), 
							IfThenElse.make(
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "ann", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partner", "tom")),
									Expressions.ONE, Expressions.ZERO)
							));
	}
	
	@Test
	public void testRandomVariableDeclaration() {
		String string;
		string = "random grade: Boolean x Boolean -> Boolean;";
		test(string, expected(null, 
							  Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "grade", "2", "Boolean", "Boolean", "Boolean"),
							  null));

		string = "random father: Boolean -> Boolean;";
		test(string, expected(null, 
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "father", "1", "Boolean", "Boolean"),
				              null));

		string = "random happy: Boolean -> Boolean;";
		test(string, expected(null, 
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "happy", "1", "Boolean", "Boolean"),
				              null));

		string = "random president: Boolean;";
		test(string, expected(null,
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "president", "0", "Boolean"),
				              null));	
	}
	
	@Test
	public void testDetectedRandomVariableErrors() {
		test("random times: Number x Number;", null);
		test("random times: x Number;", null);
		test("random times: x -> Number;", null);
		
		testExpectedModelError(			 
				 "sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random grade: Number -> Grade;\n"
				+"random grade: Number -> Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_NOT_UNIQUE);

		testExpectedModelError(			 
				 "sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random ng: Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_SAME_AS_CONSTANT);

		test("random not: Boolean;", null);
		test("random and: Boolean;", null);
		test("random or: Boolean;", null);
		testExpectedModelError("random 'if . then . else .': Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_SAME_AS_IN_BUILT_FUNCTOR);
		test("random sort: Boolean;", null);
		test("random random: Boolean;", null);
		
		testExpectedModelError("random 'for all . : .': Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_SAME_AS_QUANTIFIER);
		testExpectedModelError("random 'there exists . : .': Boolean;", HOGModelError.Type.RANDOM_VARIABLE_NAME_SAME_AS_QUANTIFIER);
	
		testExpectedModelError("random location: Place;", HOGModelError.Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED);
		testExpectedModelError("random location: Number x Number -> Place;", HOGModelError.Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED);

		testExpectedModelError( 
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random goodGrade: Grade -> Boolean;\n"
				+"goodGrade(55);",
				HOGModelError.Type.RANDOM_VARIABLE_ARGUMENT_IS_OF_THE_INCORRENT_TYPE);
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
				+"random grade: Number -> Grade;\n"
				+"for all X: grade(X);",
				HOGModelError.Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE);
		testExpectedModelError(
				"sort Grade: 8, a, b, c, d, e, f, g, ng;\n"
				+"random grade: Number -> Grade;\n"
				+"there exists X: grade(X);",
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
	protected Expression expected(Expression sortDeclarations, Expression randomVariableDeclarations, Expression statements) {
		Expression result = Tuple.make(new Object[] {ensureTuple(sortDeclarations), ensureTuple(randomVariableDeclarations), ensureTuple(statements)});
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
