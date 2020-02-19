package com.sri.ai.praise.core.representation.interfacebased.polytope.api;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base.ProductPolytope;

/**
 * An atomic polytope that is not a simplex.
 * Such polytope implementations must provide a method for summing out a set of variables from a list of atomic polytopes that depend on them (contain them).
 * This is needed because {@link ProductPolytope} must provide {@link Polytope#sumOut(Collection)} but is a general class without detailed knowledge of 
 * the representation used by atomic polytopes it contains.
 *  
 * @author braz
 *
 */
public interface NonSimplexAtomicPolytope extends AtomicPolytope {

	Polytope sumOutFromDependentAtomicPolytopes(Collection<? extends Variable> eliminated, Collection<? extends Polytope> polytopesDependentOnEliminated);

	AtomicPolytope getEquivalentAtomicPolytopeOn(Variable variable, Collection<? extends AtomicPolytope> atomicPolytopes);
	
}
