package com.sri.ai.test.praise.sgsolver.hogm.antlr;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.sgsolver.hogm.antlr.HOGMParserWrapper;

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
	public void testRandomVariableDeclaration() {
		String string;
		string = "random grade: People x Class -> Number;";
		test(string, expected(null, 
							  Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "grade", "2", "People", "Class", "Number"),
							  null));

		string = "random father: People -> People;";
		test(string, expected(null, 
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "father", "1", "People", "People"),
				              null));

		string = "random happy: People -> Boolean;";
		test(string, expected(null, 
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "happy", "1", "People", "Boolean"),
				              null));

		string = "random president: People;";
		test(string, expected(null,
				              Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "president", "0", "People"),
				              null));	
	}
	
	//
	// PROTECTED
	//
	protected Expression expected(Expression sortDeclarations, Expression randomVariableDeclarations, Expression statements) {
		Expression sorts, rvs, terms;
		if (sortDeclarations == null) {
			sorts = Tuple.make(new Object[0]);
		}
		else if (Tuple.isTuple(sortDeclarations)) {
			sorts = sortDeclarations;
		}
		else {
			sorts = Tuple.make(sortDeclarations);
		}
		
		if (randomVariableDeclarations == null) {
			rvs = Tuple.make(new Object[0]);
		}
		else if (Tuple.isTuple(randomVariableDeclarations)) {
			rvs = randomVariableDeclarations;
		}
		else {
			rvs = Tuple.make(randomVariableDeclarations);
		}
		
		if (statements == null) {
			terms = Tuple.make(new Object[0]);
		}
		else if (Tuple.isTuple(statements)) {
			terms = statements;
		}
		else {
			terms = Tuple.make(statements);
		}
		Expression result = Tuple.make(new Object[] {sorts, rvs, terms});
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
}
