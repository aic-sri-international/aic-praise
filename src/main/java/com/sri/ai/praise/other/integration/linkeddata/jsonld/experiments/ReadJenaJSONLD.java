package com.sri.ai.praise.other.integration.linkeddata.jsonld.experiments;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;

public class ReadJenaJSONLD {
	
	public static void main(String[] args) throws Throwable {
		String filename = "test.jsonld";
		
		Model model = ModelFactory.createDefaultModel() ;
		model.read(filename) ;		
	}

}
