package com.sri.ai.praise.inference.gabrielstry.factors;

import static com.sri.ai.praise.inference.representation.core.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.util.Util.accumulate;
import static com.sri.ai.util.Util.in;

import java.util.Collection;
import java.util.Iterator;

import com.sri.ai.praise.inference.gabrielstry.Approximations.api.Approximation;
import com.sri.ai.praise.inference.representation.api.Factor;

public class Factors {
	public static Factor multiply(Iterator<? extends Factor> factors) {
		Factor result = IDENTITY_FACTOR;
		for(Factor f : in(factors)) {
			if(f instanceof Approximation) {
				//Doing this because simple factors don't handle multiplication by approximations 
				result = f.multiply(result);
			}
			else {
				result = result.multiply(f);
			}
		}
		accumulate(factors, Factor::multiply, IDENTITY_FACTOR);
		return result;
	}

	static Factor multiply(Collection<? extends Factor> factors) {
		Factor result = multiply(factors.iterator());
		return result;
	}}
