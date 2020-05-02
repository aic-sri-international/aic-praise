package com.sri.ai.test.praise.core.representation.interfacebased.polytope;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.arrayTableFactor;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.decimalPlaces;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.maximumNumberOfEntriesToShow;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.MinimumBasedFunctionConvexHullSimplification.simplify;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.LinkedList;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.DefaultFunctionConvexHull;

class MinimumBasedFunctionConvexHullSimplificationTest {

	@BeforeEach
	void before() {
		maximumNumberOfEntriesToShow = -1;
		decimalPlaces = 5;
	}
	
	@Test
	void basicTest() {
		
		TableVariable i = new TableVariable("i", 5);
		TableVariable a = new TableVariable("a", 2);
		TableVariable i0 = new TableVariable("I0", 2);
		
		Factor factor;
		LinkedList<TableVariable> indices;
		Factor expectedFactor;
		LinkedList<TableVariable> expectedIndices;
		
		indices = list(i);
		factor = arrayTableFactor(
				list(i, a), 
				(vi, va) -> vi == 0? 
						va == 0 ? 2 : 8 : 
						va == 0 ? 4 : 6);
		
		expectedIndices = list(i0);
		expectedFactor = new ArrayTableFactor(
				list(i0, a),
				new double[] {0.4, 0.6, 0.2, 0.8});
		
		FunctionConvexHull functionConvexHull = new DefaultFunctionConvexHull(indices, factor);
		Polytope actual = simplify(functionConvexHull);
		
		FunctionConvexHull expectedFunctionConvexHull = new DefaultFunctionConvexHull(expectedIndices, expectedFactor);
		Polytope expected = simplify(expectedFunctionConvexHull);
		
		println(actual);
		
		var checkEquality = expected.checkEquality(actual);
		println("Equality: " + checkEquality);
		assertTrue(checkEquality.areEqual());
		assertEquals(expected, actual);
	}
}
