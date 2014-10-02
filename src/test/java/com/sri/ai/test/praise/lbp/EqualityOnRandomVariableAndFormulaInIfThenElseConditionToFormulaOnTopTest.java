package com.sri.ai.test.praise.lbp;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.praise.lbp.core.EqualityOnRandomVariableAndFormulaInIfThenElseConditionToFormulaOnTop;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.example.TrivialPQR;
import com.sri.ai.test.praise.AbstractLPITest;

public class EqualityOnRandomVariableAndFormulaInIfThenElseConditionToFormulaOnTopTest extends AbstractLPITest {

	@Test
	public void test() {
		TestData[] tests = new TestData[] {
			new LocalTestData(new TrivialPQR(), 
					"if p(a) = (X = Y) then 1 else 2",
					"if X = Y then if p(a) then 1 else 2 else (if p(a) then 2 else 1)"),	
			new LocalTestData(new TrivialPQR(), 
					"if (X = Y) = p(a) then 1 else 2",
					"if X = Y then if p(a) then 1 else 2 else (if p(a) then 2 else 1)"),
			new LocalTestData(new TrivialPQR(), 
					"if q(a, b) = (X = Y) then 1 else 2",
					"if X = Y then if q(a, b) then 1 else 2 else (if q(a, b) then 2 else 1)"),	
			new LocalTestData(new TrivialPQR(), 
					"if (X = Y) = q(a, b) then 1 else 2",
					"if X = Y then if q(a, b) then 1 else 2 else (if q(a, b) then 2 else 1)"),
			new LocalTestData(new TrivialPQR(), 
					"if r = (X = Y) then 1 else 2",
					"if X = Y then if r then 1 else 2 else (if r then 2 else 1)"),	
			new LocalTestData(new TrivialPQR(), 
					"if (X = Y) = r then 1 else 2",
					"if X = Y then if r then 1 else 2 else (if r then 2 else 1)"),
		    //
			// No Change expected as b is not a random variable value but a constant in the Trivial PQR model
			new LocalTestData(new TrivialPQR(), 
					"if b = (X = Y) then 1 else 2",
					"if b = (X = Y) then 1 else 2"),		
			new LocalTestData(new TrivialPQR(), 
					"if (X = Y) = b then 1 else 2",
					"if (X = Y) = b then 1 else 2"),
			//
			// Boolean constants
			new LocalTestData(new TrivialPQR(), 
					"if r = true then 1 else 2",
					"if r then 1 else 2"),
			new LocalTestData(new TrivialPQR(), 
					"if r = false then 1 else 2",
					"if r then 2 else 1"),					
		};
		
		perform(tests);
	}
	
	
	class LocalTestData extends TestData {
		Rewriter rewriter =  new EqualityOnRandomVariableAndFormulaInIfThenElseConditionToFormulaOnTop();
		private String 	   input;
		private Expression exprInput;
		
		public LocalTestData(Model model, String input, String expected) {
			super(Expressions.TRUE.toString(), model, false, expected);
			this.input = input;
		}
		
		@Override
		public Expression getTopExpression() {
			exprInput = parse(input);
			return exprInput;
		}
		
		@Override
		public Expression callRewrite(RewritingProcess process) {
			Expression result = rewriter.rewrite(exprInput, process);
			return result;
		}
	}
}
