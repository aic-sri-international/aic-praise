package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.convertersolverwrapper;

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Collection;

import com.google.common.base.Function;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.util.base.BinaryFunction;

public class TableFactorTypeConverterSolver implements BinaryFunction<Variable, FactorNetwork, Factor> {

	private BinaryFunction<Variable, FactorNetwork, Factor> innerSolver;
	
	private Function<TableFactor, TableFactor> converter;
	
	private Class<? extends TableFactor> rightClass;
	
	public TableFactorTypeConverterSolver(BinaryFunction<Variable, FactorNetwork, Factor> innerSolver, Class<? extends TableFactor> rightClass) {
		this.innerSolver = innerSolver;
		this.rightClass = rightClass;
		this.converter = f -> {
			try {
				return rightClass.getConstructor(Collection.class, ArrayList.class).newInstance(f.getVariables(), f.getEntries());
			} catch (Exception e) {
				throw new Error(e);
			}
		};
	}

	@Override
	public Factor apply(Variable query, FactorNetwork factorNetwork) {
		factorNetwork = makeSureFactorNetworkUsesTheRightTypeOfTableFactors(factorNetwork);
		return innerSolver.apply(query, factorNetwork);
	}

	private FactorNetwork makeSureFactorNetworkUsesTheRightTypeOfTableFactors(FactorNetwork factorNetwork) {
		@SuppressWarnings("unchecked")
		Collection<? extends TableFactor> tableFactors = (Collection<? extends TableFactor>) factorNetwork.getFactors();
		if (tableFactors.isEmpty()) {
			return factorNetwork;
		}
		else {
			TableFactor first = getFirst(tableFactors);
			if (rightClass.isInstance(first)) {
				return factorNetwork;
			}
			else {
				var newFactors = mapIntoList(tableFactors, converter);
				return new TableFactorNetwork(newFactors);
			}
		}
	}

}
