package com.sri.ai.praise.core.representation.interfacebased.polytope.core.box;

import static com.sri.ai.util.Util.collect;
import static com.sri.ai.util.Util.list;

import java.util.LinkedList;
import java.util.List;

import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.FunctionConvexHull;

public class BoxUtil {

	public static Polytope BoxAPolytope(Polytope polytope, Predicate<Polytope> criteria) {
	
		List<Polytope> toBeBoxed = list();
		List<Polytope> notToBeBoxed= list();
	
		collect(
				polytope.getAtomicPolytopes(), 
				p -> p instanceof FunctionConvexHull && criteria.apply(p), 
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
			if(p instanceof FunctionConvexHull && 
					!((FunctionConvexHull)p).getIndices().isEmpty() &&
					((FunctionConvexHull)p).getFactor() instanceof ArrayTableFactor){
				
					//Box boxedP = TableFactorBoxBuilder.makeTableBox((FunctionConvexHull) p);
				Box boxedP = Box.boxIt((FunctionConvexHull) p);	
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
