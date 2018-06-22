package com.sri.ai.test.praise.core.inference.representation.expression;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import static  com.sri.ai.praise.core.representation.interfacebased.factor.core.IdentityFactor.IDENTITY_FACTOR;
import static  com.sri.ai.praise.core.representation.interfacebased.factor.core.ZeroFactor.ZERO_FACTOR;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.ConstantFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.ExpressionFactor;

public class ExpressionFactorTest {
	
	@Test
	public void testMultiply1() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new ExpressionFactor(a, context);
		
		Expression b = DefaultSymbol.createSymbol("b");
		ExpressionFactor bFactor = new ExpressionFactor(b, context);
		
		Factor abFactor = (ExpressionFactor) aFactor.multiply(bFactor);
		
		assertEquals("a * b", abFactor.toString());
	}
	
	@Test
	public void testMultiply2() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new ExpressionFactor(a, context);
		
		Factor result = (ExpressionFactor) aFactor.multiply(IDENTITY_FACTOR);
		
		assertEquals(aFactor, result);
	}
	
	@Test
	public void testMultiply3() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new ExpressionFactor(a, context);
		
		Factor result = aFactor.multiply(ZERO_FACTOR);

		assertEquals(ZERO_FACTOR, result);
	}
	
	@Test
	public void testMultiply4() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new ExpressionFactor(a, context);
		ConstantFactor tenFactor = new ConstantFactor(10.);
		
		Factor result = aFactor.multiply(tenFactor);

		assertEquals("10 * a", result.toString());
	}
	
	@Test
	public void testAdd1() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new ExpressionFactor(a, context);
		
		Expression b = DefaultSymbol.createSymbol("b");
		ExpressionFactor bFactor = new ExpressionFactor(b, context);
		
		Factor abFactor = (ExpressionFactor) aFactor.add(bFactor);
		
		assertEquals("a + b", abFactor.toString());
	}
	
	@Test
	public void testAdd2() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new ExpressionFactor(a, context);
		
		Factor result = (ExpressionFactor) aFactor.add(IDENTITY_FACTOR);
		
		System.out.println(result.toString());
		
		assertEquals("1 + a", result.toString());
	}
	
	@Test
	public void testAdd3() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new ExpressionFactor(a, context);
		
		Factor result = aFactor.add(ZERO_FACTOR);

		assertEquals(aFactor, result);
	}
	
	@Test
	public void testAdd4() {
		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression a = DefaultSymbol.createSymbol("a");
		ExpressionFactor aFactor = new ExpressionFactor(a, context);
		ConstantFactor tenFactor = new ConstantFactor(10.);
		
		Factor result = aFactor.add(tenFactor);

		assertEquals("10 + a", result.toString());
	}

}
