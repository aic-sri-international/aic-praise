package com.sri.ai.test.praise.core.representation.expression.proceduralattachment;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionFactorNetwork.expressionFactorNetwork;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBPRootNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionFactorNetwork;
import com.sri.ai.praise.other.integration.proceduralattachment.api.ProceduralAttachments;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;
import com.sri.ai.praise.other.integration.proceduralattachment.core.DefaultProceduralAttachments;
import com.sri.ai.util.base.Wrapper;

public class ProceduralAttachmentFactorTest {

	boolean oldExact;
	int oldPrecision;

	@Before
	public void setUp() {
		oldExact = ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(false);
		oldPrecision = ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInApproximateRepresentationOfNumericalSymbols(3);
	}
	
	@After
	public void tearDown() {
		ExpressoConfiguration.setDisplayNumericsExactlyForSymbols(oldExact);
		ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInApproximateRepresentationOfNumericalSymbols(oldPrecision);
	}
	
	@Test
	public void testBooleans() {
		
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
		
		ExactBPRootNode algorithm = new ExactBPRootNode(DefaultExpressionVariable.expressionVariable(parse("V1")), network);
		queryResult = algorithm.apply();
		println("Result computed for query V1.");
		println("Result: " + queryResult);
		
		assertTrue(ran1.value);
		assertTrue(ran2.value);
		assertTrue(ran3.value);
		assertFalse(ran4.value);
		assertFalse(ran5.value);
	}

	@Test
	public void testReals() {
		
		Theory theory;
		Context context;
		Procedure<Double> procedure1;
		Procedure<Double> procedure2;
		Procedure<Double> procedure3;
		Procedure<Double> procedure4;
		Procedure<Double> procedure5;
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
				parse("O1"), parse("Real"), 
				parse("O2"), parse("Real"), 
				parse("O3"), parse("Real"), 
				parse("O4"), parse("Real"), 
				parse("O5"), parse("Real")
				);
		
		procedure1 = p -> {
			println("Proceeding with procedure 1!");
			ran1.value = true;
			return 1.0;
		};
		procedure2 = p -> {
			println("Proceeding with procedure 2!");
			ran2.value = true;
			return 2.0;
		};
		procedure3 = p -> {
			println("Proceeding with procedure 3!");
			ran3.value = true;
			return 30.0; // this is going to make the condition fail!
		};
		procedure4 = p -> {
			println("Proceeding with procedure 4!");
			ran4.value = true;
			return 4.0;
		};
		procedure5 = p -> {
			println("Proceeding with procedure 5!");
			ran5.value = true;
			return 5.0;
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
						parse("if V1 and O1 > 0.9 and O1 < 1.1 and V2 then 0.8 else 0.2"),
						parse("if V2 and O2 > 1.9 and O2 < 2.1 and V3 then 0.8 else 0.2"),
						parse("if V3 and O3 > 2.9 and O3 < 3.1 and V4 then 0.8 else 0.2"),
						parse("if V4 and O4 > 3.9 and O4 < 4.1 and V5 then 0.8 else 0.2"),
						parse("if V5 and O5 > 4.9 and O5 < 5.1 then 0.8 else 0.2"));

		network = expressionFactorNetwork(factorExpressions, proceduralAttachments, context);
		
		ExactBPRootNode algorithm = new ExactBPRootNode(DefaultExpressionVariable.expressionVariable(parse("V1")), network);
		queryResult = algorithm.apply();
		println("Result computed for query V1.");
		println("Result: " + queryResult);
		
		assertTrue(ran1.value);
		assertTrue(ran2.value);
		assertTrue(ran3.value);
		assertFalse(ran4.value);
		assertFalse(ran5.value);

		ran1.value = false;
		ran2.value = false;
		ran3.value = false;
		ran4.value = false;
		ran5.value = false;
		
// TODO: why is the test below failing?
//		// We need to create the procedural attachments and network again because procedures are run only once and then just use cached values. 
//		proceduralAttachments =
//				new DefaultProceduralAttachments(
//						map(
//								"O1", procedure1,
//								"O2", procedure2,
//								"O3", procedure3,
//								"O4", procedure4,
//								"O5", procedure5
//								));
//		network = expressionFactorNetwork(factorExpressions, proceduralAttachments, context);
//
//		algorithm = new ExactBP(DefaultExpressionVariable.expressionVariable(parse("O1")), network);
//		queryResult = algorithm.apply();
//		println("Result computed for query O1.");
//		println("Result: " + queryResult);
//		
//		assertTrue(ran1.value);
//		assertTrue(ran2.value);
//		assertTrue(ran3.value);
//		assertFalse(ran4.value);
//		assertFalse(ran5.value);
//		assertEquals(queryResult.toString(), "if O1 > 0.99 then if O1 < 1.01 then 0.006 else 0 else 0");
	}
}
