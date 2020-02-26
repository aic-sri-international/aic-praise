package com.sri.ai.praise.core.representation.interfacebased.polytope.core;

import static com.sri.ai.util.Util.collect;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoListAndTellIfThereWasChange;

import java.util.List;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;

public class Polytopes {

	public static Polytope mapAtomicPolytopes(Polytope polytope, Function<? super AtomicPolytope, ? extends AtomicPolytope> function) {
		var updatedAtomicPolytopesAndChange = mapIntoListAndTellIfThereWasChange(polytope.getAtomicPolytopes(), function);
		var mappedAtomicPolytopes = updatedAtomicPolytopesAndChange.first;
		var thereWasChange = updatedAtomicPolytopesAndChange.second;
		Polytope result = thereWasChange
								? ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(mappedAtomicPolytopes)
								: polytope;
		return result;
	}

	public static AtomicPolytope removeIndicesSatisfying(AtomicPolytope atomicPolytope, Predicate<? super Variable> predicate) {
		if ( ! (atomicPolytope instanceof FunctionConvexHull)) {
			return atomicPolytope;
		}
		return removeIndicesSatisfying((FunctionConvexHull) atomicPolytope, predicate);
	}

	public static AtomicPolytope removeIndicesSatisfying(FunctionConvexHull functionConvexHull, Predicate<? super Variable> predicate) {
		
		List<Variable> indicesToRemove = list();
		List<Variable> remainingIndices = list();
		collect(functionConvexHull.getIndices(), predicate, indicesToRemove, remainingIndices);
		AtomicPolytope updatedAtomicPolytope;
		if (indicesToRemove.isEmpty()) {
			updatedAtomicPolytope = functionConvexHull;
		}
		else {
			updatedAtomicPolytope = functionConvexHull.newInstance(remainingIndices, functionConvexHull.getFactor());
		}
		return updatedAtomicPolytope;
	}

}
