package com.sri.ai.praise.other.integration.linkeddata.jsonld.experiments;

import static com.sri.ai.util.Util.println;

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.github.jsonldjava.core.JsonLdOptions;
import com.github.jsonldjava.core.JsonLdProcessor;
import com.github.jsonldjava.utils.JsonUtils;
import com.sri.ai.praise.other.integration.linkeddata.api.LinkedDataList;
import com.sri.ai.praise.other.integration.linkeddata.api.LinkedDataMap;
import com.sri.ai.praise.other.integration.linkeddata.api.LinkedDataObject;
import com.sri.ai.praise.other.integration.linkeddata.jsonld.JSONLDJavaUtil;

public class ReadJSONLD {
	
	public static void main(String[] args) throws Throwable {
		// Open a valid json(-ld) input file
		InputStream inputStream = new FileInputStream("src/main/resources/test.jsonld");
		//			InputStream inputStream = new FileInputStream("C:\\Users\\E26638\\Documents\\Research\\World Modelers\\6-month evaluation\\Example CAGs\\BBN\\bbn-m6-cag.v0.1\\bbn-m6-cag.v0.1\\bbn-m6-cag.v0.1\\cag.json-ld");
		// Read the file into an Object (The type of this object will be a List, Map, String, Boolean,
		// Number or null depending on the root object in the file).
		Object jsonObject = JsonUtils.fromInputStream(inputStream);
		// Create a context JSON map containing prefixes and definitions
		Map context = new HashMap();
		// Customise context...
		// Create an instance of JsonLdOptions with the standard JSON-LD options
		JsonLdOptions options = new JsonLdOptions();
		// Customise options...
		// Call whichever JSONLD function you want! (e.g. compact)
		Object flatten = JsonLdProcessor.flatten(jsonObject, context, options);
		// Print out the result (or don't, it's your call!)
		System.out.println(JsonUtils.toPrettyString(flatten));

		Object expanded = JsonLdProcessor.expand(jsonObject, options);
//		// Print out the result (or don't, it's your call!)
//		System.out.println(JsonUtils.toPrettyString(expanded));

//		System.out.println(JsonUtils.toPrettyString(jsonObject));
		
//		Map root = (Map) jsonObject;
//		println("root: " + root);
//		List products = (List) root.get("product");
//		println("products: " + products);
//		Map lube = (Map) products.get(1);
//		println("lube: " + lube);
//		Object companion = lube.get("companion");
//		println("companion: " + companion);
//		
		println("\nExpanded");
		Map eroot = (Map) ((List)expanded).get(0);
		println("root: " + eroot);
		List eproducts = (List) eroot.get("http://ns.example.com/store#product");
		println("products: " + eproducts);
		Map elube = (Map) eproducts.get(1);
		println("lube: " + elube);
		Object ecompanion = elube.get("http://ns.example.com/store#companion");
		println("companion: " + ecompanion);
		
		println("Linked data:");
		LinkedDataObject linkedDataRoot = JSONLDJavaUtil.getRootLinkedDataObject(jsonObject);
		println("linkedDataRoot: " + linkedDataRoot);
		LinkedDataObject linkedDataProducts = ((LinkedDataMap)linkedDataRoot).get("http://ns.example.com/store#product");
		println("linkedDataProducts: " + linkedDataProducts);
		LinkedDataObject linkedDataLube = ((LinkedDataList)linkedDataProducts).get(1);
		println("linkedDataLube: " + linkedDataLube);
		LinkedDataObject linkedDataCompanion = ((LinkedDataMap)linkedDataLube).get("http://ns.example.com/store#companion");
		println("linkedDataCompanion: " + linkedDataCompanion);
	}
}
