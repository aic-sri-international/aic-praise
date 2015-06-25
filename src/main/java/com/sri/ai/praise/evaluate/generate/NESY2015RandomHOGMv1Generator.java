package com.sri.ai.praise.evaluate.generate;

import java.io.File;

import com.sri.ai.praise.lang.ModelLanguage;

/**
 * Codifies the generation of the HOGMv1 models used as the basis for doing experimental runtime comparisons
 * between SGDPLL and the cutting edge propositional solver, VEC, as reported in the NESY 2015 workshop 
 * paper submission.
 * 
 * @author oreilly
 *
 */
public class NESY2015RandomHOGMv1Generator {
	static long _seed = 3;
	//
	static int [] _domainSizes = new int[] {2, 4, 8, 16};
	//
	static final int _potentialIdx = 0;
	static final int _variableIdx  = 1;
	static final int _constantIdx  = 2;
	static final int _depthIdx     = 3;
	static final int _breadthIdx   = 4;
	static int [][] _params = new int[][] {	
		// #potentials, #variables, #constants, _depth, _breadth
		//
		{         5,          8,          0,      2,        2},
		{         5,          8,          0,      2,        4},
		{         5,          8,          0,      2,        8},
		{         5,          8,          0,      2,       16},
		{         5,          8,          0,      4,        4},
		{         5,          8,          0,      4,        8},
		{         5,          8,          0,      4,       16},
		{         5,          8,          2,      2,        2},
		{         5,          8,          4,      4,        4},
		{         5,          8,          8,      4,        8},
		{         5,          8,         16,      4,       16},
		//
		{         5,         10,          0,      2,        2},
		{         5,         10,          0,      2,        4},
		{         5,         10,          0,      2,        8},
		{         5,         10,          0,      2,       16},
		{         5,         10,          0,      4,        4},
		{         5,         10,          0,      4,        8},
		{         5,         10,          0,      4,       16},
		{         5,         10,          2,      2,        2},
		{         5,         10,          4,      4,        4},
		{         5,         10,          8,      4,        8},
		{         5,         10,         16,      4,       16},
		//
		{         5,         12,          0,      2,        2},
		{         5,         12,          0,      2,        4},
		{         5,         12,          0,      2,        8},
		{         5,         12,          0,      2,       16},
		{         5,         12,          0,      4,        4},
		{         5,         12,          0,      4,        8},
		{         5,         12,          0,      4,       16},
		{         5,         12,          2,      2,        2},
		{         5,         12,          4,      4,        4},
		{         5,         12,          8,      4,        8},
		{         5,         12,         16,      4,       16},
	};
	
	public static void main(String[] args) {
		if (args.length != 1) {
			throw new IllegalArgumentException("t Root NESY2015 model output directory must be specified");
		}
		File rootNESY2015OutputDirectory = validateDirectory(args[0]);
		File hogmv1ProblemDirectory      = new File(rootNESY2015OutputDirectory, ModelLanguage.HOGMv1.getCode());
		if (!hogmv1ProblemDirectory.exists()) {
			hogmv1ProblemDirectory.mkdir();
		}
		
		for (int p = 0; p < _params.length; p++) {
			int numberOfPotentials = _params[p][_potentialIdx];
			int numberOfVariables  = _params[p][_variableIdx];			
			int depth              = _params[p][_depthIdx];
			int breadth            = _params[p][_breadthIdx];
			
			for (int i = 0; i < _domainSizes.length; i++) {
				int cardinality = _domainSizes[i];
				
				int numberOfConstants = _params[p][_constantIdx];
				// #constants must be <= domain size
				if (numberOfConstants > cardinality) {
					numberOfConstants = cardinality;
				}
				
				String outputFileSuffix = "_r"+_seed+"_s"+cardinality+"_p"+numberOfPotentials+"_v"+numberOfVariables+"_c"+numberOfConstants+"_d"+depth+"_b"+breadth;
				
				RandomHOGMv1Generator.main(new String[] {
						"-r="+_seed,
						"-s="+cardinality,
						"-o="+new File(hogmv1ProblemDirectory, "sg_random_model"+outputFileSuffix+ModelLanguage.HOGMv1.getDefaultFileExtension()).getAbsolutePath(),
						"-p="+numberOfPotentials,
						"-v="+numberOfVariables,
						"-c="+numberOfConstants,
						"-d="+depth,
						"-b="+breadth
				});				
			}
		}
	}
	
	//
	// PRIVATE
	//
	private static File validateDirectory(String directoryName) {
		File result = new File(directoryName);
		if (!result.exists()) {
			throw new IllegalArgumentException("Output directory does not exist");
		}
		if (!result.isDirectory()) {
			throw new IllegalArgumentException("Output directory is not a directory: "+result.getAbsolutePath());
		}
		
		return result;
	}
}