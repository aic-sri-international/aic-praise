package com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.intensional;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.makeListWithElementsOfTwoCollections;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.subtract;
import static java.util.stream.Collectors.toList;

import java.util.Collection;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base.Polytopes;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base.Simplex;

/**
 * <pre>
 * This class offers methods to solve the problem of summing out a set of variables from a polytope:
 * 
 * sum_V polytope
 * 
 * A polytope may be of three types: atomic polytopes simplex and intensional convex hull, and products of polytopes.
 * 
 * We break product of polytopes into their atomic component and obtain the form:
 * 
 * sum_V CH_1...CH_m S_1 ... S_n
 * 
 * where
 * CH_i = {(on U_i) phi_i } is an intensional convex hull, and
 * S_j is a simplex on variable W_j.
 * 
 * We can easily factor out polytopes whose free variables do not intersect with V,
 * and assume the form above in which free variables always intersect with V.
 * Then we have:
 * 
* sum_V { (on U_1) phi_1 }...{ (on U_m) phi_m }   S_1 ... S_n
 * 
 * =
 * 
 * Union_{W_1,...,W_n} sum_{V \ {W_1,...,W_n}} {(on U_1) phi_1 }...{(on U_m) phi_m } 
 * 
 * =
 * 
 * Union_{W_1,...,W_n, U_1...U_m} { sum_{V \ ({W_1,...,W_n} union Union_I U_i)} phi_1...phi_m }
 *  
 * =
 * 
 * {(on W_1,...,W_n, U_1...U_m) sum_{V \ ({W_1,...,W_n} union Union_I U_i)} phi_1...phi_m } 
 *  
 * =
 * 
 * {(on W_1,...,W_n, U_1...U_m) phi' } 
 * 
 * which is an intensional convex hull representing the result.
 * 
 * The result can be further simplified by eliminating indices that do no appear in phi',
 * and by considering {(on ) 1} a multiplication identity polytope that can be eliminated from products.
 * 
 * Examples:
 *
 * Example 1:
 * 
 * sum_{I,K} {(on J) if I = K and I = M then 1 else 0} S_K S_M
 * =
 * S_M * sum_{I,K} {(on J) if I = K and I = M then 1 else 0} S_K
 * =
 * S_M * Union_K sum_I {(on J) if I = K and I = M then 1 else 0}
 * =
 * S_M * Union_{J,K} { sum_I if I = K and I = M then 1 else 0 }
 * =
 * S_M * Union_{J,K} { phi(K,M) }  for some phi
 * =
 * S_M * {(on J,K) phi(K,M) }
 * =
 * S_M * {(on K) phi(K,M) }
 * 
 * 
 * Example 2: m = 0.
 * 
 * sum_{I,J,K} S_K S_M
 * =
 * S_M * sum_{I,J,K} S_K
 * =
 * S_M * Union_K { sum_{I,J} 1 }
 * =
 * S_M * Union_K { 1 }
 * =
 * S_M * {(on K) 1 }
 * =
 * S_M
 * </pre>
 *
 * 
 * Example 3:
 * 
 * sum_{I,J,K} phi(I,K) S_K S_M
 * =
 * sum_{I,J,K} {(on ) phi(I,K)} S_K S_M
 * =
 * S_M * sum_{I,J,K} {(on ) phi(I,K)} S_K
 * =
 * S_M * Union_K { sum_{I,J} phi(I,K) }
 * =
 * S_M * Union_K { phi'(K) }
 * =
 * S_M * {(on K) phi'(K) }
 * </pre>

 * @author braz
 *
 */
public class IntensionalPolytopeUtil {

	/**
	 * Takes a polytope in which the only free variable is a given query variable,
	 * and returns a single equivalent {@link AtomicPolytope}.
	 * 
	 * @return
	 */
	public static AtomicPolytope getEquivalentAtomicPolytopeOn(Variable query, Polytope polytope) {
		
		myAssert(polytope.getFreeVariables().size() == 1 && polytope.getFreeVariables().contains(query), () -> "getEquivalentAtomicPolytopeOn must receive polytope whose only free variable is " + query + ", but instead got <" + polytope + "> with free variables " + polytope.getFreeVariables());
		
		final List<? extends AtomicPolytope> atomicPolytopes = Polytopes.getAtomicPolytopes(list(polytope));
	
		Simplex simplexOnVariableIfAny = (Simplex) getFirst(atomicPolytopes, p -> Polytopes.isSimplexOn(p, query));
		
		boolean thereIsSimplexOnQuerySoItDominates = simplexOnVariableIfAny != null;
		
		AtomicPolytope result;
		if (thereIsSimplexOnQuerySoItDominates) {
			result = simplexOnVariableIfAny;
		}
		else {
			// all atomicPolytopes are non-simplex, or otherwise we would have simplexes on non-query variables and the query would not be the only free variable
			result = mergeNonSimplexAtomicPolytopes(atomicPolytopes);
		}
		
		return result;
	}

	private static IntensionalPolytope mergeNonSimplexAtomicPolytopes(List<? extends AtomicPolytope> convexHulls) {
		List<Variable> indicesFromIntensionalPolytopes = collectIndicesFromPolytopesGivenTheyAreAllIntensionalPolytopes(convexHulls);
		Factor productOfFactors = makeProductOfFactors(convexHulls);
		return new IntensionalPolytope(indicesFromIntensionalPolytopes, productOfFactors);
	}

	private static List<Variable> collectIndicesFromPolytopesGivenTheyAreAllIntensionalPolytopes(List<? extends Polytope> polytopes) {
		List<Variable> indices = list();
		for (Polytope polytope : polytopes) {
			var intensionalPolytope = (IntensionalPolytope) polytope;
			indices.addAll(intensionalPolytope.getIndices());
		}
		return indices;
	}

	public static Factor makeProductOfFactors(List<? extends AtomicPolytope> convexHulls) {
		List<Factor> factors = collectFactorsFromPolytopesThatAreIntensionalPolytopes(convexHulls);
		return Factor.multiply(factors);
	}

	private static List<Factor> collectFactorsFromPolytopesThatAreIntensionalPolytopes(Collection<? extends Polytope> polytopes) {
		List<Factor> factors = list();
		for (Polytope polytope : polytopes) {
			collectFactorIfIntensionalPolytope(polytope, factors);
		}
		return factors;
	}

	private static void collectFactorIfIntensionalPolytope(Polytope polytope, List<Factor> factors) {
		if (polytope instanceof IntensionalPolytope) {
			IntensionalPolytope intensionalPolytope = (IntensionalPolytope) polytope;
			factors.add(intensionalPolytope.getFactor());
		}
	}

	////////////////////////////////////// SUMMING OUT
	
	public static Polytope sumOutGivenThatPolytopesAllDependOnEliminatedVariables(Collection<? extends Variable> eliminated, Collection<? extends Polytope> polytopesDependentOnEliminated) {
	
		// This is a bit tricky to understand, but one thing to keep in mind is that eliminated simplex variables become intensional convex hell indices by this process.
		// See full explanation in class javadoc.
		
		var simplexVariables = collectSimplexVariables(polytopesDependentOnEliminated);
		// because each simplex has a single variable and all simplices depend on eliminated, all simplex variables are in eliminated.
		
		var indicesFromIntensionalPolytopes = IntensionalPolytopeUtil.collectIndicesFromThosePolytopesWhichAreIntensionalPolytopes(polytopesDependentOnEliminated);
		
		var factors = collectFactorsFromPolytopesThatAreIntensionalPolytopes(polytopesDependentOnEliminated);
		
		var productOfFactors = Factor.multiply(factors);
		
		var variablesToBeEliminatedOnceSimplexesAreDealtWith = subtract(eliminated, simplexVariables);
		
		var summedOutFactor = productOfFactors.sumOut(variablesToBeEliminatedOnceSimplexesAreDealtWith);
		
		var finalIndices = makeListWithElementsOfTwoCollections(indicesFromIntensionalPolytopes, simplexVariables);
		
		var projectedPolytope = new IntensionalPolytope(finalIndices, summedOutFactor);
		
		return projectedPolytope;
	}

	private static List<Variable> collectSimplexVariables(Collection<? extends Polytope> polytopes) {
		return 
				polytopes.stream()
				.filter(p -> p instanceof Simplex)
				.flatMap(p -> p.getFreeVariables().stream())
				.collect(toList());
	}

	private static List<Variable> collectIndicesFromThosePolytopesWhichAreIntensionalPolytopes(Collection<? extends Polytope> polytopes) {
		return 
				polytopes.stream()
				.filter(p -> p instanceof IntensionalPolytope)
				.flatMap(c -> ((IntensionalPolytope)c).getIndices().stream())
				.collect(toList());
	}
	
	///////////////////////////////////////////// IDENTITY POLYTOPE

	public static IntensionalPolytope identityPolytope() {
		return new IntensionalPolytope(list(), IDENTITY_FACTOR);
	}

}
