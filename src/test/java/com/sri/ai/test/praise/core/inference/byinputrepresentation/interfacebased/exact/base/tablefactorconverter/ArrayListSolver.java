package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.tablefactorconverter;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.util.base.BinaryFunction;

public class ArrayListSolver extends TableFactorTypeConverterSolver {

	public ArrayListSolver(BinaryFunction<Variable, FactorNetwork, Factor> innerSolver) {
		super(innerSolver, ArrayTableFactor.class);
	}

}
