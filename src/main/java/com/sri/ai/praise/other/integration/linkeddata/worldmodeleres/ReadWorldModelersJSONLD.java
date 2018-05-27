package com.sri.ai.praise.other.integration.linkeddata.worldmodeleres;

import static com.sri.ai.util.Util.println;

import java.io.FileInputStream;
import java.io.InputStream;

import com.github.jsonldjava.utils.JsonUtils;
import com.sri.ai.praise.other.integration.linkeddata.api.LinkedDataMap;
import com.sri.ai.praise.other.integration.linkeddata.api.LinkedDataObject;
import com.sri.ai.praise.other.integration.linkeddata.jsonld.JSONLDJavaUtil;

public class ReadWorldModelersJSONLD {
	
	public static void main(String[] args) throws Throwable {
		// Open a valid json(-ld) input file
//		InputStream inputStream = new FileInputStream("src/main/resources/test2.jsonld");
		InputStream inputStream = new FileInputStream("src/main/resources/debug_docs_cag-0.2.0.jsonld");
		//			InputStream inputStream = new FileInputStream("C:\\Users\\E26638\\Documents\\Research\\World Modelers\\6-month evaluation\\Example CAGs\\BBN\\bbn-m6-cag.v0.1\\bbn-m6-cag.v0.1\\bbn-m6-cag.v0.1\\cag.json-ld");
		// Read the file into an Object (The type of this object will be a List, Map, String, Boolean,
		// Number or null depending on the root object in the file).
		Object jsonObject = JsonUtils.fromInputStream(inputStream);

		//JsonLdOptions options = new JsonLdOptions();
		//Object expanded = JsonLdProcessor.expand(jsonObject, options);
		//Object flattened = JsonLdProcessor.flatten(jsonObject, options);
		println("Flattened:");
		println(JsonUtils.toPrettyString(jsonObject));
		
		println("Linked data:");
		LinkedDataObject linkedDataRoot = JSONLDJavaUtil.getRootLinkedDataObject(jsonObject);
		println("linkedDataRoot: " + linkedDataRoot);
		LinkedDataObject linkedDataDocuments = ((LinkedDataMap)linkedDataRoot).get("https://github.com/clulab/eidos/wiki/JSON-LD#Documents");
		println("linkedDataDocuments: " + linkedDataDocuments);
//		LinkedDataObject linkedDataLube = ((LinkedDataList)linkedDataProducts).get(1);
//		println("linkedDataLube: " + linkedDataLube);
//		LinkedDataObject linkedDataCompanion = ((LinkedDataMap)linkedDataLube).get("http://ns.example.com/store#companion");
//		println("linkedDataCompanion: " + linkedDataCompanion);
	}
}
