package com.sri.ai.praise.inference.anytimeexactbp.polytope.box;

import static com.sri.ai.util.Util.list;

import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.inference.representation.api.Factor;

public class Box extends IntensionalConvexHullOfFactors{
	
	public Box(BoxVariable boxVariable,Factor boxUnderTheFormOfAFactor) {
		super(list(boxVariable), boxUnderTheFormOfAFactor);
	}
	
	/*public Box(Factor phiMin, Factor phiMax, BoxFactorFactory factory) {
		this(factory.make(phiMin,phiMax));
	}*/
	
}
