package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.convertersolverwrapper;

import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;

import java.util.Iterator;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.DefaultFunctionConvexHull;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.computation.anytime.api.Approximation;

public class FromExactToAnytimeSolver implements BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>> {

	private BinaryFunction<Variable, FactorNetwork, Factor> innerSolver;

	public FromExactToAnytimeSolver(BinaryFunction<Variable, FactorNetwork, Factor> innerSolver) {
		this.innerSolver = innerSolver;
	}

	@Override
	public Iterator<Approximation<Factor>> apply(Variable query, FactorNetwork factorNetwork) {
		var exactSolution = innerSolver.apply(query, factorNetwork);
		var convexHull = new DefaultFunctionConvexHull(list(), exactSolution);
		return iterator(convexHull);
	}

}
