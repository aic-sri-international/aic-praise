package com.sri.ai.test.praise.core.inference.representation.expression.proceduralattachment;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;

import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.eager.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.proceduralattachment.ProceduralAttachmentFactor;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;

public class ProceduralAttachmentFactorTest {

	@Test
	public void test() {
		Theory theory;
		Context context;
		ExpressionVariable variable;
		Procedure<Integer> procedure;
		ExpressionFactor factor;
		ExpressionFactorNetwork network;
		Factor queryResult;
		
		theory = new CommonTheory();
		context = new TrueContext(theory);
		variable = new DefaultExpressionVariable(parse("Thirteen"));
		context = context.extendWithSymbolsAndTypes(variable, parse("Integer"));
		
		procedure = p -> {
			println("Proceeding with procedure!");
			return 13;
		};
		
		factor = new ProceduralAttachmentFactor(variable, procedure, context);
		
		network = new ExpressionFactorNetwork(list(factor), context);
		ExactBP algorithm = new ExactBP(variable, network);
		queryResult = algorithm.apply();
		println("Result computed.");
		println("Result: " + queryResult);
		
		// TODO: make sure to check why expression is being computed before it's time to print.
		// TODO: change code to automatically detect unregistered symbols
		// TODO: deal with ease of forgetting to set up variable predicate. Should just use symbols, really.
	}

}
