package com.sri.ai.praise.core.representation.interfacebased.polytope.core;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ZeroFactor.ZERO_FACTOR;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.polytopesAreEqual;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.polytopesAreOfIncomparableClasses;
import static java.util.Collections.emptyList;

import java.util.Collection;

import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck;
import com.sri.ai.util.Enclosing;

public class IdentityPolytope implements Polytope {

	public final static IdentityPolytope IDENTITY_POLYTOPE = new IdentityPolytope();
	
	public static IdentityPolytope identityPolytope( ) {
		return IDENTITY_POLYTOPE;
	}
	
	@Override
	public Collection<? extends Variable> getFreeVariables() {
		return emptyList();
	}

	@Override
	public boolean isIdentity() {
		return true;
	}

	@Override
	public Polytope multiply(Polytope another) {
		return another;
	}

	@Override
	public Collection<? extends AtomicPolytope> getAtomicPolytopes() {
		return emptyList();
	}

	@Override
	public Polytope sumOut(Collection<? extends Variable> eliminated) {
		return this; // modulo a constant!
	}

	@Override
	public Polytope unSumOutSimplexVariables(Predicate<? super Variable> shouldNotHaveBeenSummedOut) {
		throw new Error(IdentityPolytope.class + " does not support " + (new Enclosing()).methodName());
	}

//	@Override
//	public AtomicPolytope getAtomicPolytopeEquivalentToMarginalPolytopeOf(Variable variable) {
//		throw new Error("getEquivalentAtomicPolytopeOn is not valid for IdentityPolytope");
//	}
//
	@Override
	public AtomicPolytope getEquivalentAtomicPolytope() {
		throw new Error("getEquivalentAtomicPolytope is not valid for IdentityPolytope");
	}

	@Override
	public boolean equalsModuloPermutations(Object another) {
		return another instanceof IdentityPolytope;
	}

	@Override
	public PolytopesEqualityCheck checkEquality(Polytope another) {
		if (another instanceof IdentityPolytope) {
				return polytopesAreEqual(this, another);
		}
		else {
			return polytopesAreOfIncomparableClasses(this, another);
		}
	}

	@Override
	public Factor probabilityRange() {
		return ZERO_FACTOR;
	}

	@Override
	public Polytope normalize(Collection<? extends Variable> variablesToNormalize) {
		throw new Error("normalize is not valid for IdentityPolytope");
	}
}
