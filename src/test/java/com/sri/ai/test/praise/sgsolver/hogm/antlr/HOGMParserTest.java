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
