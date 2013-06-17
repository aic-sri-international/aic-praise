/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-praise nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.praise.model.example;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.model.Model;

/**
 * A good example of how hard it is to model things without aggregate factors.
 * The intuitive model for this is that if there exists a treasure one owns,
 * then one is rich; If there exists a person that gives a treasure to one, then
 * one owns a treasure. However, without aggregate factors we cannot express
 * this existential quantifier property. In particular, if no one gives X a
 * treasure, the probability of not owning it is still nonzero.
 * 
 * <pre>
 * union(
 *       {{(on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X,Z,Y) then (if owns(Y,Z) then 1 else 0)  else 1] }},  
 *       {{(on X in People, Z in Treasure) [if owns(X,Z) then if rich(X) then 1 else 0 else 1] }})
 *       
 * random variable names=gaveTreasureTo/3,owns/2,rich/1.
 * </pre>
 */
@Beta
public class TrivialGaveTreasureToOwnsRich extends Model {

	/**
	 * A good example of how hard it is to model things without aggregate
	 * factors. The intuitive model for this is that if there exists a treasure
	 * one owns, then one is rich; If there exists a person that gives a
	 * treasure to one, then one owns a treasure. However, without aggregate
	 * factors we cannot express this existential quantifier property. In
	 * particular, if no one gives X a treasure, the probability of not owning
	 * it is still nonzero.
	 * 
	 * <pre>
	 * union(
	 *       {{(on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X,Z,Y) then (if owns(Y,Z) then 1 else 0)  else 1] }},  
	 *       {{(on X in People, Z in Treasure) [if owns(X,Z) then if rich(X) then 1 else 0 else 1] }})
	 *       
	 * random variable names=gaveTreasureTo/3,owns/2,rich/1.
	 * </pre>
	 */
	public TrivialGaveTreasureToOwnsRich() {
		super(  
				"model("
				+ "'Trivial Gave Treasure To',"
				+ "'A good example of how hard it is to model without aggregate factors.',"
				+ "sort(People, Unknown, {}), "
				+ "sort(Treasure, Unknown, {}), "
				+ "randomVariable(gaveTreasureTo, 3, People, Treasure, People, Boolean), "
				+ "randomVariable(owns, 2, People, Treasure, Boolean), "
				+ "randomVariable(rich, 1, People, Boolean), "
				+ "parfactors(union("
						+ "{{(on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X,Z,Y) then (if owns(Y,Z) then 1 else 0)  else 1] }}, "
						+ "{{(on X in People, Z in Treasure) [if owns(X,Z) then if rich(X) then 1 else 0 else 1] }}))"
				+ ")");
	}
}
