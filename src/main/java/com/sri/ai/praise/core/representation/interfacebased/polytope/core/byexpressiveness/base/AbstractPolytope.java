package com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base;

import static com.sri.ai.util.Util.collect;
import static com.sri.ai.util.Util.list;

import java.util.Collection;
import java.util.List;

import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.util.Util;

public abstract class AbstractPolytope implements Polytope {

	abstract protected Polytope sumOutEliminatedVariablesFromPolytopesDependingOnThem(Collection<? extends Variable> eliminated, Collection<? extends Polytope> dependentPolytopes);
	
	@Override
	public Polytope sumOut(Collection<? extends Variable> eliminated) {
		
		List<Polytope> independentOfEliminated = list();
		List<Polytope> dependentOfEliminated = list();

		collect(
				/* original collection: */ this.getAtomicPolytopes(), 
				/* criterion: */ isIndependentOf(eliminated), 
				/* satisfy criterion: */ independentOfEliminated, 
				/* do not satisfy criterion: */ dependentOfEliminated);

		Polytope summedOutFromDependents = sumOutEliminatedVariablesFromPolytopesDependingOnThem(eliminated, dependentOfEliminated);

		List<Polytope> allAtomicPolytopesInResult = independentOfEliminated; // re-using independentOfEliminated
		allAtomicPolytopesInResult.add(summedOutFromDependents);
		Polytope result = Polytope.multiply(allAtomicPolytopesInResult);

		return result;
	}
	
	private static Predicate<Polytope> isIndependentOf(Collection<? extends Variable> variables) {
		return p -> ! Util.intersect(p.getFreeVariables(), variables);
	}

}
