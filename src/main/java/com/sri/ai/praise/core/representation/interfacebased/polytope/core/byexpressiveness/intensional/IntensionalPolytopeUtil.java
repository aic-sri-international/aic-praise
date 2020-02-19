package com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.intensional;

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base.Simplex;

public class IntensionalPolytopeUtil {

	/**
	 * Takes a polytope in which the only free variable is a given variable,
	 * and returns a single equivalent {@link AtomicPolytope}.
	 * 
	 * TODO: this is a suspicious method. It seems equivalent to eliminating all variables but the given one, which should result in an atomic polytope anyway.
	 * 
	 * @return
	 */
	public static AtomicPolytope getEquivalentAtomicPolytopeOn(Variable variable, Polytope polytope) {
		
		myAssert(polytope.getFreeVariables().size() == 1 && polytope.getFreeVariables().contains(variable), () -> "getEquivalentAtomicPolytopeOn must receive polytope whose only free variable is " + variable + ", but instead got <" + polytope + "> with free variables " + polytope.getFreeVariables());
		
		final Collection<? extends AtomicPolytope> atomicPolytopes = polytope.getAtomicPolytopes();
	
		Simplex simplexOnVariableIfAny = (Simplex) getFirst(atomicPolytopes, p -> isSimplexOn(p, variable));
		
		boolean thereIsSimplexOnQuerySoItDominates = simplexOnVariableIfAny != null;
		
		AtomicPolytope result;
		if (thereIsSimplexOnQuerySoItDominates) {
			result = simplexOnVariableIfAny;
		}
		else {
			// all atomicPolytopes are non-simplex, or otherwise we would have simplexes on non-query variables and the query would not be the only free variable
			result = makeAtomicPolytopeEquivalentToProductOfNonSimplexAtomicPolytopes(atomicPolytopes);
		}
		
		return result;
	}

	private static boolean isSimplexOn(AtomicPolytope atomicPolytope, Variable variable) {
		boolean result = 
				atomicPolytope instanceof Simplex
				&&
				((Simplex)atomicPolytope).getVariable().equals(variable);
		return result;
	}

	private static IntensionalPolytope makeAtomicPolytopeEquivalentToProductOfNonSimplexAtomicPolytopes(Collection<? extends AtomicPolytope> nonSimplexAtomicPolytopes) {
		@SuppressWarnings("unchecked")
		Collection<? extends IntensionalPolytope> intensionalPolytopes = (Collection<? extends IntensionalPolytope>) nonSimplexAtomicPolytopes;
		// The only non-simplex atomic polytopes in this implementation are intensional polytopes.
		
		List<Variable> indicesFromIntensionalPolytopes = collectIndicesFromIntensionalPolytopesGivenTheyAreAllIntensionalPolytopes(intensionalPolytopes);
		Factor productOfFactors = makeProductOfFactorsOf(intensionalPolytopes);
		return new IntensionalPolytope(indicesFromIntensionalPolytopes, productOfFactors);
	}

	private static List<Variable> collectIndicesFromIntensionalPolytopesGivenTheyAreAllIntensionalPolytopes(Collection<? extends IntensionalPolytope> intensionalPolytopes) {
		List<Variable> indices = list();
		for (IntensionalPolytope intensionalPolytope : intensionalPolytopes) {
			indices.addAll(intensionalPolytope.getIndices());
		}
		return indices;
	}

	private static Factor makeProductOfFactorsOf(Collection<? extends IntensionalPolytope> intensionalPolytopes) {
		List<Factor> factors = mapIntoList(intensionalPolytopes, IntensionalPolytope::getFactor);
		return Factor.multiply(factors);
	}
}
