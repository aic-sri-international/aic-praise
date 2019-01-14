package com.sri.ai.test.praise.core.inference.representation.expression;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ZeroFactor.ZERO_FACTOR;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.theory.tuple.TupleTheory;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ConstantFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;

public class ExpressionFactorTest {
	
	@Test
	public void testMultiply1() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new DefaultExpressionFactor(a, context);
		
		Expression b = DefaultSymbol.createSymbol("b");
		ExpressionFactor bFactor = new DefaultExpressionFactor(b, context);
		
		Factor abFactor = aFactor.multiply(bFactor);
		
		assertEquals("a * b", abFactor.toString());
	}
	
	@Test
	public void testMultiply2() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new DefaultExpressionFactor(a, context);
		
		Factor result = aFactor.multiply(IDENTITY_FACTOR);
		
		assertEquals(aFactor, result);
	}
	
	@Test
	public void testMultiply3() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new DefaultExpressionFactor(a, context);
		
		Factor result = aFactor.multiply(ZERO_FACTOR);

		assertEquals(ZERO_FACTOR, result);
	}
	
	@Test
	public void testMultiply4() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new DefaultExpressionFactor(a, context);
		ConstantFactor tenFactor = new ConstantFactor(10.);
		
		Factor result = aFactor.multiply(tenFactor);

		assertEquals("10 * a", result.toString());
	}
	
	@Test
	public void testAdd1() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new DefaultExpressionFactor(a, context);
		
		Expression b = DefaultSymbol.createSymbol("b");
		ExpressionFactor bFactor = new DefaultExpressionFactor(b, context);
		
		Factor abFactor = aFactor.add(bFactor);
		
		assertEquals("a + b", abFactor.toString());
	}
	
	@Test
	public void testAdd2() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new DefaultExpressionFactor(a, context);
		
		Factor result = aFactor.add(IDENTITY_FACTOR);
		
		System.out.println(result.toString());
		
		assertEquals("1 + a", result.toString());
	}
	
	@Test
	public void testAdd3() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new DefaultExpressionFactor(a, context);
		
		Factor result = aFactor.add(ZERO_FACTOR);

		assertEquals(aFactor, result);
	}
	
	@Test
	public void testAdd4() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new DefaultExpressionFactor(a, context);
		ConstantFactor tenFactor = new ConstantFactor(10.);
		
		Factor result = aFactor.add(tenFactor);

		assertEquals("10 + a", result.toString());
	}
	
	@Test
	public void testGetVariables() {
		
		Theory theory = new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, false),
				new LinearRealArithmeticTheory(false, false),
				new TupleTheory(),
				new PropositionalTheory());
		Context context = new TrueContext(theory);
		
		Expression expression = parse("{{ (on I in 1..10) I + J }}");
		Factor factor = new DefaultExpressionFactor(expression, context);
		
		List<? extends Variable> factorFreeVariables = factor.getVariables();
		
		assertEquals(1, factorFreeVariables.size());
		assertEquals("[J]", factorFreeVariables.toString());
	}
	
	@Test
	public void testSumOut() {
		
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(
				"U", "Boolean",
				"V", "Boolean");
		
		ExpressionVariable u = new DefaultExpressionVariable(parse("U"));
		List<ExpressionVariable> variablesToSumOut = new ArrayList<>();
		variablesToSumOut.add(u);
		
		ExpressionFactor factorUV = new DefaultExpressionFactor(parse("if U and V then 2 else 3"), context);
		
		Factor summedOutFactor = factorUV.sumOut(variablesToSumOut);

		assertEquals("if V then 5 else 6", summedOutFactor.toString());
	}
	
	@Test
	public void testInvert() {
		
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(
				"U", "Boolean",
				"V", "Boolean");
		
		ExpressionFactor factorUV = new DefaultExpressionFactor(parse("if U and V then 2 else 3"), context);
		
		Factor invertedFactor = factorUV.invert();

		assertEquals("if U then if V then 0.5 else 1/3 else 1/3", invertedFactor.toString());
	}
	
	@Test
	public void testMax() {
		
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(
				"U", "Boolean",
				"V", "Boolean");
		
		ExpressionFactor factorUV = new DefaultExpressionFactor(parse("if U then if V then 4 else 2 else 3"), context);
		
		ExpressionVariable u = new DefaultExpressionVariable(parse("U"));
		List<ExpressionVariable> variablesToMaxOut = new ArrayList<>();
		variablesToMaxOut.add(u);
		Factor maxFactor = factorUV.max(variablesToMaxOut);

		assertEquals("if V then 4 else 3", maxFactor.toString());
	}

}
