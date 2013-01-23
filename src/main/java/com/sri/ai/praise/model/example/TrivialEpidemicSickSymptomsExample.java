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
 * <pre>
 * model(
 * 'Epidemic and Sick with Symptoms Example',
 * 'An example of the interplay between symptoms.',
 * 
 * sort(People, 10, { david, rodrigo, hung, shahin, ciaran }),
 * 
 * randomVariable(epidemic, 0),
 * randomVariable(sick, 1, People, Boolean),
 * randomVariable(fever, 1, People, Boolean),
 * randomVariable(rash, 1, People, Boolean),
 * randomVariable(notAtWork, 1, People, Boolean),
 * 
 * parfactors(union(
 *     { [if epidemic then 0.001 else 0.999] },
 *     
 *     {{ (on X in People)
 *         [if epidemic then if sick(X) then 0.6 else 0.4 else if sick(X) then 0.05 else 0.95] }},
 * 
 *     {{ (on X in People)
 *         [if sick(X) then if fever(X) then 0.7 else 0.3 else if fever(X) then 0.01 else 0.99] }},
 * 
 *     {{ (on X in People) 
 *         [if sick(X) then if rash(X) then 0.6 else 0.4 else if rash(X) then 0.07 else 0.93] }},
 * 
 *     {{ (on X in People)
 *         [if sick(X) then if notAtWork(X) then 0.8 else 0.2 else if notAtWork(X) then 0.05 else 0.95] }},
 * 			  
 *    {{ (on X in People) 
 *         [if notAtWork(X) then 0.001 else 0.999] | X = david }},
 *         
 *    {{ (on X in People) 
 *         [if notAtWork(X) then 0.05 else 0.95] | X != david }},
 * 			  
 *     { (on X in People) [if rash(X) then 1 else 1] },
 * 
 *     { (on X in People) [if fever(X) then 1 else 1] },
 * 		
 *     { (on X in People) [if notAtWork(X) then 1 else 1] }
 * ))
 * )
 * </pre>
 * 
 * @author oreilly
 *
 */
@Beta
public class TrivialEpidemicSickSymptomsExample extends Model {
	/**
	 * <pre>
	 * model(
	 * 'Epidemic and Sick with Symptoms Example',
	 * 'An example of the interplay between symptoms.',
	 * 
	 * sort(People, 10, { david, rodrigo, hung, shahin, ciaran }),
	 * 
	 * randomVariable(epidemic, 0),
	 * randomVariable(sick, 1, People, Boolean),
	 * randomVariable(fever, 1, People, Boolean),
	 * randomVariable(rash, 1, People, Boolean),
	 * randomVariable(notAtWork, 1, People, Boolean),
	 * 
	 * parfactors(union(
	 *     { [if epidemic then 0.001 else 0.999] },
	 *     
	 *     {{ (on X in People)
	 *         [if epidemic then if sick(X) then 0.6 else 0.4 else if sick(X) then 0.05 else 0.95] }},
	 * 
	 *     {{ (on X in People)
	 *         [if sick(X) then if fever(X) then 0.7 else 0.3 else if fever(X) then 0.01 else 0.99] }},
	 * 
	 *     {{ (on X in People) 
	 *         [if sick(X) then if rash(X) then 0.6 else 0.4 else if rash(X) then 0.07 else 0.93] }},
	 * 
	 *     {{ (on X in People)
	 *         [if sick(X) then if notAtWork(X) then 0.8 else 0.2 else if notAtWork(X) then 0.05 else 0.95] }},
	 * 			  
 	 *    {{ (on X in People) 
	 *         [if notAtWork(X) then 0.001 else 0.999] | X = david }},
	 *         
 	 *    {{ (on X in People) 
	 *         [if notAtWork(X) then 0.05 else 0.95] | X != david }},
	 * 			  
	 *     { (on X in People) [if rash(X) then 1 else 1] },
	 * 
	 *     { (on X in People) [if fever(X) then 1 else 1] },
	 * 		
	 *     { (on X in People) [if notAtWork(X) then 1 else 1] }
	 * ))
	 * )
	 * </pre>
	 */
	public TrivialEpidemicSickSymptomsExample() {
		super("model(" +
			"'Epidemic and Sick with Symptoms Example',\n" +
			"'An example of the interplay between symptoms.',\n" +
			"\n" +
			"sort(People, 10, { david, rodrigo, hung, shahin, ciaran }),\n" +
			"\n" +
			"randomVariable(epidemic, 0),\n" +
			"randomVariable(sick, 1, People, Boolean),\n" +
			"randomVariable(fever, 1, People, Boolean),\n" +
			"randomVariable(rash, 1, People, Boolean),\n" +
			"randomVariable(notAtWork, 1, People, Boolean),\n" +
			"\n" +
			"parfactors(union(\n" +
			"    { [if epidemic then 0.001 else 0.999] },\n" +
			"\n" +
			"    {{ (on X in People) \n" +
			"        [if epidemic then if sick(X) then 0.6 else 0.4 else if sick(X) then 0.05 else 0.95] }},\n" +
			"\n" +
			"    {{ (on X in People) \n" +
			"        [if sick(X) then if fever(X) then 0.7 else 0.3 else if fever(X) then 0.01 else 0.99] }},\n" +
			"\n" +
			"    {{ (on X in People) \n" +
			"        [if sick(X) then if rash(X) then 0.6 else 0.4 else if rash(X) then 0.07 else 0.93] }},\n" +
			"\n" +
			"    {{ (on X in People) " +
			"        [if sick(X) then if notAtWork(X) then 0.8 else 0.2 else if notAtWork(X) then 0.05 else 0.95] }}," +
			"\n" +
			"    {{ (on X in People) " +
			"        [if notAtWork(X) then 0.001 else 0.999] | X = david}},\n" +
			"\n" +
			"    {{ (on X in People) " +
			"        [if notAtWork(X) then 0.05 else 0.95] | X != david}},\n" +
			"\n" +
			"    { (on X in People) [if rash(X) then 1 else 1] },\n" +
			"\n" +
			"    { (on X in People) [if fever(X) then 1 else 1] },\n" +
			"\n" +
			"    { (on X in People) [if notAtWork(X) then 1 else 1] }\n" +
			"\n" +
			"))" +
			")");
	}
}
