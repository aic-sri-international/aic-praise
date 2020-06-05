package com.sri.ai.test.praise.core.representation.interfacebased.polytope;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.arrayTableFactor;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.decimalPlaces;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.maximumNumberOfEntriesToShow;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.IdentityPolytope.IDENTITY_POLYTOPE;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.MinimumBasedFunctionConvexHullSimplification.simplify;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.repeat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.LinkedList;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.DefaultFunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.Simplex;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;

class MinimumBasedFunctionConvexHullSimplificationTest {

	@BeforeEach
	void before() {
		maximumNumberOfEntriesToShow = -1;
		decimalPlaces = 5;
		ThreadExplanationLogger.setIsActive(true);
	}
	
	@AfterEach
	void after() {
		ThreadExplanationLogger.setIsActive(false);
	}
	
	@Test
	void testSimplifyFunctionConvexHull() {
		
		int iCardinality = 5;
		int jCardinality = 5;
		int aCardinality = 2;
		int bCardinality = 2;
		
		TableVariable i = new TableVariable("i", iCardinality);
		TableVariable j = new TableVariable("j", jCardinality);
		TableVariable a = new TableVariable("a", aCardinality);
		TableVariable b = new TableVariable("b", bCardinality);
		TableVariable i0 = new TableVariable("I0", aCardinality);
		TableVariable i1 = new TableVariable("I1", bCardinality);
		
		LinkedList<TableVariable> indices;
		Factor factor;
		LinkedList<TableVariable> expectedIndices;
		Factor expectedFactor;
		
		indices = list(i);
		factor = arrayTableFactor(
				list(i, a), 
				(vi, va) -> 
					vi == 0? 
						va == 0 ? 2 : 8 : 
						va == 0 ? 9 : 1);
		expectedIndices = list(i0);
		expectedFactor = new ArrayTableFactor(
				list(i0, a),
				new double[] {0.9, 0.1, 0.2, 0.8});
		runSimplifyFunctionConvexHullTest(indices, factor, expectedIndices, expectedFactor);

		
		
		indices = list(i, j);
		factor = arrayTableFactor(
				list(i, j, a), 
				(vi, vj, va) -> 
					vi == 0? 
						vj == 0?
								va == 0 ? 2 : 8 : 
								va == 0 ? 9 : 1 :
						vj == 0?
								va == 0 ? 3 : 7 : 
								va == 0 ? 0.5 : 0.5
						);
		expectedIndices = list(i0);
		expectedFactor = new ArrayTableFactor(
				list(i0, a),
				new double[] {0.9, 0.1, 0.2, 0.8});
		runSimplifyFunctionConvexHullTest(indices, factor, expectedIndices, expectedFactor);

		
		
		indices = list(i);
		factor = arrayTableFactor(
				list(i, a, b), 
				(vi, va, vb) -> 
					vi == 0? 
						va == 0?
								vb == 0 ? 0.2 : 0.3 : 
								vb == 0 ? 0.4 : 0.1 :
						va == 0?
								vb == 0 ? 0.05 : 0.45 : 
								vb == 0 ? 0.25 : 0.25
						);
		expectedIndices = list(i0, i1);
		expectedFactor = new ArrayTableFactor(
				list(i0, i1, a, b),
				new double[] {0.35, 0.3, 0.25, 0.1, 0.05, 0.6, 0.25, 0.1, 0.05, 0.3, 0.55, 0.1, 0.05, 0.3, 0.25, 0.4});
		runSimplifyFunctionConvexHullTest(indices, factor, expectedIndices, expectedFactor);

		
		
		indices = list(i, j);
		factor = arrayTableFactor(
				list(i, j, a, b), 
				(vi, vj, va, vb) -> 
					vi == 0? 
						va == 0?
								vb == 0 ? 0.2 : 0.3 : 
								vb == 0 ? 0.4 : 0.1 :
						va == 0?
								vb == 0 ? 0.05 : 0.45 : 
								vb == 0 ? 0.25 : 0.25
						);
		expectedIndices = list(i0, i1);
		expectedFactor = new ArrayTableFactor(
				list(i0, i1, a, b),
				new double[] {0.35, 0.3, 0.25, 0.1, 0.05, 0.6, 0.25, 0.1, 0.05, 0.3, 0.55, 0.1, 0.05, 0.3, 0.25, 0.4});
		runSimplifyFunctionConvexHullTest(indices, factor, expectedIndices, expectedFactor);

		
		
		indices = list();
		factor = arrayTableFactor(
				list(a, b), 
				(va, vb) -> 
						va == 0?
								vb == 0 ? 2 : 3 : 
								vb == 0 ? 4 : 1
						);
		expectedIndices = list(i0, i1);
		expectedFactor = new ArrayTableFactor(
				list(i0, i1, a, b),
				repeat(aCardinality * bCardinality,  new double[] {0.2, 0.3, 0.4, 0.1}));
		runSimplifyFunctionConvexHullTest(indices, factor, expectedIndices, expectedFactor);

		
		
		indices = list();
		factor = new ArrayTableFactor(list(), arrayList(1.0));
		expectedIndices = list();
		expectedFactor = new ArrayTableFactor(list(), new double[] {1.0});
		runSimplifyFunctionConvexHullTest(indices, factor, expectedIndices, expectedFactor);
	}

	private void runSimplifyFunctionConvexHullTest(
			LinkedList<TableVariable> indices,
			Factor factor,
			LinkedList<TableVariable> expectedIndices,
			Factor expectedFactor) {
		
		var functionConvexHull = new DefaultFunctionConvexHull(indices, factor);
		var forced = true;
		var actual = simplify(functionConvexHull, forced);
		
		var expected = new DefaultFunctionConvexHull(expectedIndices, expectedFactor);
		
		compare(expected, actual, functionConvexHull);
	}

	private void compare(Polytope expected, Polytope actual, Polytope polytope) {
		println();
		println(MinimumBasedFunctionConvexHullSimplificationTest.class.getSimpleName());
		println("  Polytope: " + polytope + ", length: " + polytope.length());
		println("Simplified: " + actual + ", length: " + actual.length());
		println("  Expected: " + expected);
		
		var checkEquality = expected.checkEquality(actual);
		println("Equality: " + checkEquality);
		assertTrue(checkEquality.areEqual());
		assertEquals(expected, actual);
	}
	
	@Test
	public void testSimplifyPolytope() {
		
		Polytope p;
		Polytope expected;
		
		int iCardinality = 5;
		int jCardinality = 5;
		int kCardinality = 3;
		int aCardinality = 2;
		int bCardinality = 2;
		
		TableVariable i = new TableVariable("i", iCardinality);
		TableVariable j = new TableVariable("j", jCardinality);
		TableVariable k = new TableVariable("k", kCardinality);
		TableVariable a = new TableVariable("a", aCardinality);
		TableVariable b = new TableVariable("b", bCardinality);
		TableVariable i0 = new TableVariable("I0", aCardinality);
		TableVariable i1 = new TableVariable("I1", bCardinality);

		p = IDENTITY_POLYTOPE;
		expected = IDENTITY_POLYTOPE;
		runSimplifyPolytopeTest(p, expected);
		
		p = new Simplex(i);
		expected = p;
		runSimplifyPolytopeTest(p, expected);

		p = Polytope.product(new Simplex(i), new Simplex(j));
		expected = p;
		runSimplifyPolytopeTest(p, expected);

		p = new DefaultFunctionConvexHull(
				list(i),
				arrayTableFactor(
						list(i, a), 
						(vi, va) -> 
						vi == 0? 
								va == 0 ? 2 : 8 : 
								va == 0 ? 9 : 1));
		expected = 
				new DefaultFunctionConvexHull(
						list(i0),
						new ArrayTableFactor(
								list(i0, a),
								new double[] {0.9, 0.1, 0.2, 0.8}));

		runSimplifyPolytopeTest(p, expected);

		p = Polytope.product(
				new Simplex(j),
				new Simplex(a),
				new DefaultFunctionConvexHull(
						list(i),
						arrayTableFactor(
								list(i, a), 
								(vi, va) -> 
								vi == 0? 
										va == 0 ? 2 : 8 : 
										va == 0 ? 9 : 1)));
		expected = // Simplex(a) absorbs the function convex hull
				Polytope.product(
						new Simplex(j),
						new Simplex(a));

		p = Polytope.product(
				new Simplex(j),
				new Simplex(b),
				new DefaultFunctionConvexHull(
						list(i),
						arrayTableFactor(
								list(i, a), 
								(vi, va) -> 
								vi == 0? 
										va == 0 ? 2 : 8 : 
										va == 0 ? 9 : 1)));
		expected =
				Polytope.product(
						new Simplex(j),
						new Simplex(b),
						new DefaultFunctionConvexHull(
								list(i0),
								new ArrayTableFactor(
										list(i0, a),
										new double[] {0.9, 0.1, 0.2, 0.8})));

		runSimplifyPolytopeTest(p, expected);

		// Now we have a function convex hull that is not absorbed
		// by a simplex but needs to be rolled into an atomic polytope with it
		p = Polytope.product(
				new Simplex(j),
				new Simplex(b),
				new DefaultFunctionConvexHull(
						list(i),
						arrayTableFactor(
								list(i, a, b), // <-- b 
								(vi, va, vb) -> 
								vi == 0? 
										va == 0 ? 2 : 8 : 
										va == 0 ? 9 : 1)));
		// because of Simplex(b), all value tuples of of a and b will have probability 0
		// for some combination of polytope vertices.
		// For each value tuple of a and b, we fix n - 1 of them to their minimum probability (0)
		// and compute the probability for the remaining one (1).
		// We therefore get four vertices, each of them concentrating all probability on a single value.
		expected =
				Polytope.product(
						new Simplex(j),
						new DefaultFunctionConvexHull(
								list(i0, i1),
								new ArrayTableFactor(
										list(i0, i1, a, b), 
										new double[] {
												1.0, 0.0, 0.0, 0.0, 
												0.0, 1.0, 0.0, 0.0, 
												0.0, 0.0, 1.0, 0.0, 
												0.0, 0.0, 0.0, 1.0})));

		runSimplifyPolytopeTest(p, expected);

		// Similar to previous one but b has a less extreme polytope
		p = Polytope.product(
				new Simplex(j),
				new DefaultFunctionConvexHull(
						list(k),
						arrayTableFactor(
								list(k, b),
								(vi, vb) -> 
								vi == 0? 
										vb == 0 ? 0.04 : 0.96 : 
										vb == 0 ? 0.98 : 0.02)),
				new DefaultFunctionConvexHull(
						list(i),
						arrayTableFactor(
								list(i, a, b), 
								(vi, va, vb) -> 
								vi == 0? 
										va == 0 ? 2 : 8 : 
										va == 0 ? 9 : 1)));
		// and indeed we have a slightly narrower bound now
		expected =
				Polytope.product(
						new Simplex(j),
						new DefaultFunctionConvexHull(
								list(i0, i1),
								new ArrayTableFactor(
										list(i0, i1, a, b), 
										new double[] {
												0.99, 0.004, 0.004, 0.002,
												0.008, 0.986, 0.004, 0.002,
												0.008, 0.004, 0.986, 0.002, 
												0.008, 0.004, 0.004, 0.984})));

		runSimplifyPolytopeTest(p, expected);
	}

	private void runSimplifyPolytopeTest(Polytope polytope, Polytope expected) {
		var actual = simplify(polytope, true /* forced */);
		compare(expected, actual, polytope);
	}
}
