/*
 * Copyright (c) 2015, SRI International
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
package com.sri.ai.praise.lang;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Enumeration of the different Probabilistic Modeling Languages worked with in some way by PRAiSE.
 * 
 * @author oreilly
 *
 */
public enum ModelLanguage {
	//
	// Internal Languages	  			
	HOGMv1("Higher Order Graphical FactorNetwork Version 1",
			true,
		  	"HOGMv1",		  
		  	"This refers to the HOGM language developed and used by the SGDPLL class of algorithms.",		  	
		  	Arrays.asList(
		  			"https://github.com/aic-sri-international/aic-praise/tree/master/src/main/java/com/sri/ai/praise/model/v1",
		  			"https://github.com/aic-sri-international/aic-praise/wiki/docs/user%20guide.pdf"),
		  	".hogm"),		  	
	// 
	// External Languages
	PMTK3FactorGraph("PMTK3 ExpressionFactorNode Graph Format",
			false,
		  	"PMTK3",
		  	"PMTK is a collection of Matlab/Octave functions, which includes support for probabilisitc inference.",
		  	Arrays.asList(
		  			"https://github.com/probml/pmtk3"),
		  	".m"), // i.e. Matlab/Octave files, which are used as input to the PMTK3 interface.
	HuginDotNet("Hugin NET Language",
			false,
			"HuginDotNet",
			"The modeling language used by the commercial Hugin tool, used to represent belief networks (Bayesian). " +
			"Supported as a de-factor standards by several other inference libraries/tools (e.g. SamIam, GeNie and SMILE).",
			Arrays.asList(
					"http://www.hugin.com/",
					"http://download.hugin.com/webdocs/manuals/api-manual.pdf",
					"http://reasoning.cs.ucla.edu/samiam/",
					"http://reasoning.cs.ucla.edu/samiam/help/",
					"https://dslpitt.org/genie/"),
			".net"),	  
	Church("Church",
			false,
			"Church", 
			"A functional probabilistic programming language, based on Scheme.", 
			Arrays.asList(
					"https://probmods.org/index.html", 
					"https://github.com/probmods"),
			".church"),
	UAI("UAI Inference Competition FactorNetwork Format", 
			false,
			"UAI",
			"A simple propositional text file format to describe UAI Inference Competition problem instances (Markov or Bayesian networks). "  +
			"The format is a generalization of the Ergo file format initially developed by Noetic systems Ergo software.",
			Arrays.asList(
					"http://www.hlt.utdallas.edu/~vgogate/uai14-competition/modelformat.html",
					"http://www.hlt.utdallas.edu/~vgogate/vec-uai14.tar.gz"), // Vibhav's solver, used in the competition
			".uai");	
	
	//
	//	
	public String getName() {
		return name;
	}
	
	public boolean isInternalLanguageOfPRAiSE() {
		return internal;
	}

	public String getCode() {
		return code;
	}
	
	public String getDescription() {
		return description;
	}
	
	public List<String> getURLs() {
		return urls;
	}
	
	public String getDefaultFileExtension() {
		return defaultFileExtension;
	}
	
	//
	public static ModelLanguage getModelLangageForCode(String code) {
		for (ModelLanguage modelLanguage : ModelLanguage.values()) {
			if (modelLanguage.getCode().equals(code)) {
				return modelLanguage;
			}
		}
		throw new RuntimeException("Unrecognized model language code ["+code+"]");
	}
	
	//
	//
	private String  name;
	private boolean internal;
	private String  code;
	private String  description;
	private List<String> urls;
	private String  defaultFileExtension;
	//
	private ModelLanguage(String name, boolean internal, String code, String description, List<String> urls, String defaultFileExtension) {
		this.name                 = name;
		this.internal             = internal;
		this.code                 = code;
		this.description          = description;
		this.urls                 = Collections.unmodifiableList(urls);
		this.defaultFileExtension = defaultFileExtension;
	}
}
