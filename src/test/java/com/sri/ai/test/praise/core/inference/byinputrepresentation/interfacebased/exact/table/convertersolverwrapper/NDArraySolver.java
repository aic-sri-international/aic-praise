package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.convertersolverwrapper;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.ndarray.NDArrayTableFactor;
import com.sri.ai.util.base.BinaryFunction;

public class NDArraySolver extends TableFactorTypeConverterSolver {

	public NDArraySolver(BinaryFunction<Variable, FactorNetwork, Factor> innerSolver) {
		super(innerSolver, NDArrayTableFactor.class);
	}

}
