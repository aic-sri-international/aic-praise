package com.sri.ai.test.praise.core.inference.representation.expression.proceduralattachment;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionFactorNetwork.expressionFactorNetwork;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionFactorNetwork;
import com.sri.ai.praise.other.integration.proceduralattachment.api.ProceduralAttachments;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;
import com.sri.ai.praise.other.integration.proceduralattachment.core.DefaultProceduralAttachments;
import com.sri.ai.util.base.Wrapper;

public class ProceduralAttachmentFactorTest {

	@Test
	public void test() {
		Theory theory;
		Context context;
		Procedure<Boolean> procedure1;
		Procedure<Boolean> procedure2;
		Procedure<Boolean> procedure3;
		Procedure<Boolean> procedure4;
		Procedure<Boolean> procedure5;
		Wrapper<Boolean> ran1 = new Wrapper<Boolean>(false);
		Wrapper<Boolean> ran2 = new Wrapper<Boolean>(false);
		Wrapper<Boolean> ran3 = new Wrapper<Boolean>(false);
		Wrapper<Boolean> ran4 = new Wrapper<Boolean>(false);
		Wrapper<Boolean> ran5 = new Wrapper<Boolean>(false);
		ExpressionFactorNetwork network;
		Factor queryResult;
		
		theory = new CommonTheory();
		context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes(
				parse("V1"), parse("Boolean"), 
				parse("V2"), parse("Boolean"), 
				parse("V3"), parse("Boolean"), 
				parse("V4"), parse("Boolean"), 
				parse("V5"), parse("Boolean"), 
				parse("O1"), parse("Boolean"), 
				parse("O2"), parse("Boolean"), 
				parse("O3"), parse("Boolean"), 
				parse("O4"), parse("Boolean"), 
				parse("O5"), parse("Boolean")
				);
		
		procedure1 = p -> {
			println("Proceeding with procedure 1!");
			ran1.value = true;
			return true;
		};
		procedure2 = p -> {
			println("Proceeding with procedure 2!");
			ran2.value = true;
			return true;
		};
		procedure3 = p -> {
			println("Proceeding with procedure 3!");
			ran3.value = true;
			return false; // THIS ONE RETURNS FALSE! This will short-circuit and procedures 4 and 5 will not be run
		};
		procedure4 = p -> {
			println("Proceeding with procedure 4!");
			ran4.value = true;
			return true;
		};
		procedure5 = p -> {
			println("Proceeding with procedure 5!");
			ran5.value = true;
			return true;
		};
		
		ProceduralAttachments proceduralAttachments =
				new DefaultProceduralAttachments(
						map(
								"O1", procedure1,
								"O2", procedure2,
								"O3", procedure3,
								"O4", procedure4,
								"O5", procedure5
								));

		List<Expression> factorExpressions = 
				list(
						parse("if V1 and O1 and V2 then 0.8 else 0.2"),
						parse("if V2 and O2 and V3 then 0.8 else 0.2"),
						parse("if V3 and O3 and V4 then 0.8 else 0.2"),
						parse("if V4 and O4 and V5 then 0.8 else 0.2"),
						parse("if V5 and O5 then 0.8 else 0.2"));

		network = expressionFactorNetwork(factorExpressions, proceduralAttachments, context);
		
		ExactBP algorithm = new ExactBP(new DefaultExpressionVariable(parse("V1")), network);
		queryResult = algorithm.apply();
		println("Result computed.");
		println("Result: " + queryResult);
		
		assertTrue(ran1.value);
		assertTrue(ran2.value);
		assertTrue(ran3.value);
		assertFalse(ran4.value);
		assertFalse(ran5.value);
		
		// TODO: deal with ease of forgetting to set up variable predicate. Should just use symbols, really.
	}

}
