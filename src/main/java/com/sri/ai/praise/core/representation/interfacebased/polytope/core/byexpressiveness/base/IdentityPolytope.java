package com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base;

import static java.util.Collections.emptyList;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;

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
	public AtomicPolytope getEquivalentAtomicPolytopeOn(Variable variable) {
		throw new Error("getEquivalentAtomicPolytopeOn is not valid for IdentityPolytope");
	}

}
