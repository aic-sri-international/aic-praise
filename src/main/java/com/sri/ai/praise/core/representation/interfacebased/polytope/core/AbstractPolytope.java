package com.sri.ai.praise.core.representation.interfacebased.polytope.core;

import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.Polytopes.removeIndicesSatisfying;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.union;

import java.util.List;

import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;

public abstract class AbstractPolytope implements Polytope {


	@Override
	public Polytope unSumOutSimplexVariables(Predicate<? super Variable> shouldNotHaveBeenSummedOut) {
		
		var indices = union(getAtomicPolytopes(), a -> a instanceof FunctionConvexHull? ((FunctionConvexHull)a).getIndices() : list());
		var newlyFreeIndices = collectToList(indices, shouldNotHaveBeenSummedOut);
		
		Polytope updatedPolytope;
		
		if (newlyFreeIndices.isEmpty()) {
			updatedPolytope = this; 
		}
		else {
			List<AtomicPolytope> updatedAtomicPolytopes = list();
			mapIntoList(newlyFreeIndices, i -> new Simplex(i), updatedAtomicPolytopes);
			mapIntoList(getAtomicPolytopes(), a -> removeIndicesSatisfying(a, shouldNotHaveBeenSummedOut), updatedAtomicPolytopes);
			updatedPolytope = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(updatedAtomicPolytopes);
		}
		
//		if (true) {
//			println();
//			println("ProductPolytope: Updated current approximation:\n" + this);
//			println("Indices           :", join(indices));
//			println("Newly free indices:", join(newlyFreeIndices));
//			println("Previous approximation:", this);
//			println("Updated  approximation:", updatedPolytope);
//		}
		
		return updatedPolytope;
	}

}
