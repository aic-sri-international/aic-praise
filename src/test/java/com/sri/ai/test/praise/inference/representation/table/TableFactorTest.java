package com.sri.ai.test.praise.inference.representation.table;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.println;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Iterator;

import com.sri.ai.praise.inference.treebased.representation.Table.TableFactor;
import com.sri.ai.praise.inference.treebased.representation.Table.TableFactorNetwork;
import com.sri.ai.praise.inference.treebased.representation.Table.TableVariable;
import com.sri.ai.praise.inference.treebased.representation.api.Factor;
import com.sri.ai.praise.model.v1.imports.uai.UAIModel;
import com.sri.ai.praise.model.v1.imports.uai.UAIModelReader;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.IdentityWrapper;


//TODO improve tests...
public class TableFactorTest {

	public static void test1() {
		TableVariable v1 = new TableVariable("v1", 5);
		TableVariable v2 = new TableVariable("v2", 5);
		TableVariable v3 = new TableVariable("v3", 2);
		TableVariable v4 = new TableVariable("v4", 2);
		TableVariable v5 = new TableVariable("v5", 2);
		TableVariable v6 = new TableVariable("v6", 2);
		TableVariable v7 = new TableVariable("v7", 2);

		TableFactor f1 = new TableFactor(arrayList(v1,v2,v3,v4),
				arrayList(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.));
		TableFactor f2 = new TableFactor(arrayList(v4,v3,v5,v2,v7,v6), 
				arrayList(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,
						0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,.9,1.));
		TableFactor f3 = new TableFactor(arrayList(v3), arrayList(0.7,0.8));

		long startTime = System.currentTimeMillis();
		for (int i = 0; i < 1; i++) {
			f1.multiply(f2);
		}
		
		long endTime = System.currentTimeMillis();
		println(endTime-startTime);	

		println(f2.sumOut(Util.list(v3)));

		println(f3.sumOut(Util.list(v3)));

		println(f3.sumOut(Util.list()));
	}
	
	public static void main(String[] args) {
		test1();
		
		try {
			String fileName = "1a1x.uai";
			FileReader modelFile = new FileReader(new File("").getAbsolutePath()+"/UAITests/"+fileName );
			UAIModel model = UAIModelReader.read(modelFile);
			
			// Converting the network
			TableFactorNetwork tn = new TableFactorNetwork(model);
			Iterator<IdentityWrapper<Factor>> it = tn.getAs().iterator();
			
			for (int i = 0; i < 70; i++) {
				it.next();
			}
			
			println(it.next().getObject().multiply(it.next().getObject()));
			Factor f = it.next().getObject();
			println(f);
			println(f.sumOut(Util.list(f.getVariables().get(0))));
			println(f.sumOut(f.getVariables()));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
