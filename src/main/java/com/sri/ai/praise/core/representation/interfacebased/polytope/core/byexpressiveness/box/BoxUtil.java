package com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.box;

import static com.sri.ai.util.Util.collect;
import static com.sri.ai.util.Util.list;

import java.util.LinkedList;
import java.util.List;

import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base.Polytopes;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.intensional.IntensionalPolytope;

public class BoxUtil {

	public static Polytope BoxAPolytope(Polytope polytope, Predicate<Polytope> criteria) {
	
		List<Polytope> toBeBoxed = list();
		List<Polytope> notToBeBoxed= list();
	
		collect(
				Polytopes.getAtomicPolytopes(list(polytope)), 
				p -> p instanceof IntensionalPolytope && criteria.apply(p), 
				toBeBoxed, 
				notToBeBoxed);
	
		Polytope boxedPolytopes = boxPolytopes(toBeBoxed);
		notToBeBoxed.add(boxedPolytopes);
		Polytope result = Polytope.multiply(notToBeBoxed);
	
		return result;
	}

	public static Polytope boxPolytopes(List<Polytope> toBeBoxed) {
		LinkedList<Polytope> boxedPolytopes = list();
		for(Polytope p : toBeBoxed) {
			if(p instanceof IntensionalPolytope && 
					!((IntensionalPolytope)p).getIndices().isEmpty() &&
					((IntensionalPolytope)p).getFactor() instanceof ArrayTableFactor){
				
					//Box boxedP = TableFactorBoxBuilder.makeTableBox((IntensionalPolytope) p);
				Box boxedP = Box.boxIt((IntensionalPolytope) p);	
				boxedPolytopes.add(boxedP);
			}
			else {
				boxedPolytopes.add(p);
			}
		}
		Polytope result = Polytope.multiply(boxedPolytopes);
		return result;
	}

}
