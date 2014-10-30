package com.sri.ai.praise.model.export;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.StringJoiner;
import java.util.stream.Stream;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.model.Model;

/**
 * Exports an LBP Model, which is grounded first, to <a href="http://www.hugin.com/">Hugin's</a>
 * Bayesian Network <a href="http://download.hugin.com/webdocs/manuals/api-manual.pdf">.net</a> format.
 * 
 * @author oreilly
 *
 */
public class ExportToHuginDotNetBayesianNetworkFormat {
	public static void main(String[] args) throws IOException {
		if (args.length < 1 || args.length % 2 != 1) {
			throw new IllegalArgumentException("Invalid # of arguments, must be 1=model file and the optional 2 to n pairs of sort name followed by size to use when grounding the model");
		}
		
		Parser parser = new AntlrGrinderParserWrapper();
		// Read in the model declaration
		StringJoiner modelDeclarationSJ = new StringJoiner("\n");
		try (Stream<String> declarationLines = Files.lines(Paths.get(args[0]))) {
			declarationLines.sequential().forEachOrdered(line -> modelDeclarationSJ.add(line));
		}
		// Can be called with explicitly assigned sizes to the sorts associated with the model
		Map<Expression, Expression> globalObjects = new LinkedHashMap<Expression, Expression>();
		for (int i = 1; i < args.length; i+=2) {
			globalObjects.put(parser.parse("| "+args[i]+" |"), Expressions.makeSymbol(Integer.valueOf(args[i+1])));
		}
		
		Expression modelDefinition = parser.parse(modelDeclarationSJ.toString());
		Model      modelToExport   = new Model(modelDefinition, Collections.emptySet());
		
		RewritingProcess process = LBPFactory.newLBPProcess(modelDefinition);
		process.getGlobalObjects().putAll(globalObjects);
		
		
	}
	
}
