package com.sri.ai.praise.model.export;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.StringJoiner;
import java.util.stream.Stream;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.ModelGrounding;
import com.sri.ai.praise.model.ModelGrounding.GroundedModelResult;
import com.sri.ai.praise.model.grounded.bayes.ConditionalProbabilityTable;
import com.sri.ai.praise.model.grounded.transform.XFormMarkovToBayes;
import com.sri.ai.util.base.Pair;

/**
 * Exports an LBP Model, which is grounded first, to <a href="http://www.hugin.com/">Hugin's</a>
 * Bayesian Network <a href="http://download.hugin.com/webdocs/manuals/api-manual.pdf">.net</a> format.
 * 
 * @author oreilly
 *
 */
@Beta
public class ExportToHuginDotNetBayesianNetworkFormat {	
	/**
	 * Export to Hugin .net Bayesian Network format, given a HOGM or low level model, with optionally provided sort sizes (
	 * for those not already explicitly set in the model).
	 * 
	 * @param args
	 *        [0] the path to the model file
	 *        [1] 'true' if the model files is a high level model (HOGM) or 'false' if a low level model 
	 *            (i.e. what the HOGM is translated to in order to perform lifted inference).
	 *        [2] the path to the output export file to be generated (if exists will be overwritten).
	 *        [3+] optional pairs of arguments in order where each pair represents a (sort name, sort size)
	 *             to be used when grounding the model as part of the export process.
	 *             
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		if (args.length < 3 || args.length % 2 != 1) {
			throw new IllegalArgumentException("Invalid # of arguments, must be 1=model file, 2=hogm model (true or false), 3=output file, and the optional 4 to n pairs of sort name followed by size to use when grounding the model");
		}
		
		// Read in the model declaration
		StringJoiner modelDeclarationSJ = new StringJoiner("\n");
		try (Stream<String> declarationLines = Files.lines(Paths.get(args[0]))) {
			declarationLines.sequential().forEachOrdered(line -> modelDeclarationSJ.add(line));
		}
		boolean isHOGModel = Boolean.valueOf(args[1]);
		String modelDeclaration = null;
		if (isHOGModel) {
			modelDeclaration = Model.fromRules(modelDeclarationSJ.toString()).getModelDeclaration();
		} 
		else {
			modelDeclaration = modelDeclarationSJ.toString();
		}
	
		Pair[] explicitSortSizes = new Pair[(args.length-3)/2];
		for (int i = 3, v = 0; i < args.length; i+=2, v++) {
			explicitSortSizes[v] = new Pair<String, Integer>(args[i], Integer.valueOf(args[i+1]));
		}
		
		try (final Writer writer = new BufferedWriter(new FileWriter(args[2]));){
			final GroundedModelResult groundedModelResult = ModelGrounding.groundModel(modelDeclaration, explicitSortSizes);
		
			export(groundedModelResult, writer);
			
		} catch (ModelGrounding.ModelGroundingException mge) {
			System.err.println("Model Ground Exception: "+mge.getMessage());
			for (ModelGrounding.ModelGroundingError error : mge.getErrors()) {
				System.err.println(error.getErrorType());
				System.err.println(error.getInExpression());
			}
		}
	}
	
	public static void export(ModelGrounding.GroundedModelResult groundedModelResult, final Writer writer) 
			throws IOException {
		
		RewritingProcess process = groundedModelResult.getRewritingProcess();
		GroundedModelMarkovNetwork groundedMarkovNetwork = new GroundedModelMarkovNetwork(groundedModelResult);
		
		// Output the random variable information
		for (int i = 0; i < groundedMarkovNetwork.numberVariables(); i++) {
			Expression rv = groundedMarkovNetwork.getRandomVariable(i);
			StringJoiner sj = new StringJoiner("\n");
			
			sj.add("node "+getLegalHuginId(rv));
			sj.add("{");
			sj.add("  states = "+getRange(rv, process));
			sj.add("  label  = \""+rv+"\";");
			sj.add("}");
			
			output(sj.toString(), writer);
		}
		
		// Output the CPTs
		XFormMarkovToBayes.transform(groundedMarkovNetwork, new XFormMarkovToBayes.BayesOutputListener() {
			
			@Override
			public void newCPT(ConditionalProbabilityTable cpt) {
				StringJoiner sj = new StringJoiner("\n");
				sj.add("potential "+getPotentialSignature(groundedMarkovNetwork, cpt));
				sj.add("{");
				sj.add("    data = "+getPotentialData(cpt));
				sj.add("}");
				
				output(sj.toString(), writer);
				
			}
		});
	}
	
	//
	// PRIVATE
	//
	private static void output(String toOutput, Writer writer) {
		// Write to the console so the user can see the output as it occurs
		System.out.println(toOutput);
		try {
			writer.write(toOutput);
			writer.write("\n");
		}
		catch (IOException ioe) {
			throw new RuntimeException("Exception writing to output file", ioe);
		}
	}
	
	private static String getLegalHuginId(Expression rv) {
		return rv.toString().replace('(', '_').replace(',', '_').replace(' ', '_').replace(')', '_');
	}
	
	private static String getRange(Expression rv, RewritingProcess process) {
		// (\"false\" \"true\");
		StringJoiner sj = new StringJoiner(" ", "(", ");");
		for (Expression rangeValue : Model.range(rv, process)) {
			sj.add("\""+rangeValue.toString()+"\"");
		}
		return sj.toString();
	}
	
	private static String getPotentialSignature(GroundedModelMarkovNetwork groundedMarkovNetwork, ConditionalProbabilityTable cpt) {
		StringJoiner sj = new StringJoiner(" ", "(", ")");
		
		sj.add(getLegalHuginId(groundedMarkovNetwork.getRandomVariable(cpt.getChildVariableIndex())));
		sj.add("|");
		for (Integer p : cpt.getParentVariableIndexes()) {
			sj.add(getLegalHuginId(groundedMarkovNetwork.getRandomVariable(p)));
		}
		
		return sj.toString();
	}
	
	private static String getPotentialData(ConditionalProbabilityTable cpt) {
		StringJoiner sj = new StringJoiner(" ", "(", ");");
		
		for (Double d : cpt.getTable().getEntries()) {
			sj.add(""+d);
		}
		
		return sj.toString();
	}
}