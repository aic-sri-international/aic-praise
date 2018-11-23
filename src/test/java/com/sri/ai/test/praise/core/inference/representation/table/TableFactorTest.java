package com.sri.ai.test.praise.core.inference.representation.table;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.print;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.apache.jena.ext.com.google.common.annotations.VisibleForTesting;
import org.junit.Test;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ConstantFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableVariable;
import com.sri.ai.util.Util;

/**
 * Class to test the TableFactor data type
 * 
 * @author Bobak
 *
 */
public class TableFactorTest {

	// CREATE TABLES TO TEST //////////////////////////////////////////////////////////////////////////////////////////
	
	TableVariable V1 = new TableVariable("V1", 2);
	TableVariable V2 = new TableVariable("V2", 3);
	TableVariable V3 = new TableVariable("V3", 4);
	TableVariable V4 = new TableVariable("V4", 2);

	TableFactor f1 = new TableFactor("f1",arrayList(V1,V2,V3),
			arrayList(1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1.,1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1., 1.));
	TableFactor f2 = new TableFactor("f2",arrayList(V2,V4), 
			arrayList(11., 12., 21., 22., 31., 32.));
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	
	
	// TEST CASES //////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Test
	public void checkInitialFactors() {
		
		println();
		println("PRINTING INITIAL FACTORS");
		println("------------------------");
		
		println(f1);
		assertEquals("f1[{V1:card=2}, {V2:card=3}, {V3:card=4}]: [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, "
															   + "1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]", f1.toString());
		println(f2);
		assertEquals("f2[{V2:card=3}, {V4:card=2}]: [11.0, 12.0, 21.0, 22.0, 31.0, 32.0]", f2.toString());

		println();
	}
	
	
	
	@Test
	public void testMultiplicationf1f2() {
		
		println();
		println("MULTIPLYING f1 * f2");
		println("-------------------");
		
		TableFactor f1f2 = (TableFactor)f1.multiply(f2);
		f1f2.setName("f1f2");
		
		println(f1f2);
		assertEquals("phi[{V1:card=2}, {V2:card=3}, {V3:card=4}, {V4:card=2}]: [11.0, 12.0, 11.0, 12.0, 11.0, 12.0, 11.0, 12.0, "
																			 + "21.0, 22.0, 21.0, 22.0, 21.0, 22.0, 21.0, 22.0, "
																			 + "31.0, 32.0, 31.0, 32.0, 31.0, 32.0, 31.0, 32.0, "
																			 + "11.0, 12.0, 11.0, 12.0, 11.0, 12.0, 11.0, 12.0, "
																			 + "21.0, 22.0, 21.0, 22.0, 21.0, 22.0, 21.0, 22.0, "
																			 + "31.0, 32.0, 31.0, 32.0, 31.0, 32.0, 31.0, 32.0]", f1.multiply(f2).toString());
		println();
	}
	
	@Test
	public void testMultiplicationf2f1() {
		
		println();
		println("MULTIPLYING f2 * f1");
		println("-------------------");
		
		TableFactor f2f1 = (TableFactor)f2.multiply(f1);
		f2f1.setName("f2f1");

		println(f2f1);
		assertEquals("phi[{V2:card=3}, {V4:card=2}, {V1:card=2}, {V3:card=4}]: [11.0, 11.0, 11.0, 11.0, 11.0, 11.0, 11.0, 11.0, "
																			 + "12.0, 12.0, 12.0, 12.0, 12.0, 12.0, 12.0, 12.0, "
																			 + "21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, "
																			 + "22.0, 22.0, 22.0, 22.0, 22.0, 22.0, 22.0, 22.0, "
																			 + "31.0, 31.0, 31.0, 31.0, 31.0, 31.0, 31.0, 31.0, "
																			 + "32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0]", f2.multiply(f1).toString());
		println();
	}
	
	
	
	@Test
	public void testf1SumOutV1() {
		
		println();
		println("SUM OUT V1 from F1");
		println("------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V1);
		TableFactor f1SumOutV1 = (TableFactor)f1.sumOut(variablesToSumOut);
		f1SumOutV1.setName("f1SumOutV1");
		
		println(f1SumOutV1);
		assertEquals("f1SumOutV1[{V2:card=3}, {V3:card=4}]: [2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0]", f1SumOutV1.toString());
		
		println();
	}
	
	@Test
	public void testf1SumOutV2() {
		
		println();
		println("SUM OUT V2 from F1");
		println("------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V2);
		TableFactor f1SumOutV2 = (TableFactor)f1.sumOut(variablesToSumOut);
		f1SumOutV2.setName("f1SumOutV2");
		
		println(f1SumOutV2);
		assertEquals("f1SumOutV2[{V1:card=2}, {V3:card=4}]: [3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0]", f1SumOutV2.toString());
		
		println();
	}
	
	@Test
	public void testf1SumOutV3() {
		
		println();
		println("SUM OUT V3 from F1");
		println("------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V3);
		TableFactor f1SumOutV3 = (TableFactor)f1.sumOut(variablesToSumOut);
		f1SumOutV3.setName("f1SumOutV3");
		
		println(f1SumOutV3);
		assertEquals("f1SumOutV3[{V1:card=2}, {V2:card=3}]: [4.0, 4.0, 4.0, 4.0, 4.0, 4.0]", f1SumOutV3.toString());
		
		println();
	}
	
	public void testf2SumOutV2() {
		
		println();
		println("SUM OUT V2 from F2");
		println("------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V2);
		TableFactor f2SumOutV2 = (TableFactor)f2.sumOut(variablesToSumOut);
		f2SumOutV2.setName("f2SumOutV2");
		
		println(f2SumOutV2);
		assertEquals("f2SumOutV2[{V4:card=2}]: [63.0, 66.0]", f2SumOutV2.toString());
		
		println();
	}
	
	public void testf2SumOutV4() {
		
		println();
		println("SUM OUT V4 from F2");
		println("------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V4);
		TableFactor f2SumOutV4 = (TableFactor)f2.sumOut(variablesToSumOut);
		f2SumOutV4.setName("f2SumOutV4");
		
		println(f2SumOutV4);
		assertEquals("f2SumOutV4[{V4:card=2}]: [13.0, 23.0, 33.0]", f2SumOutV4.toString());
		
		println();
	}
	
	
	
	@Test
	public void testf1SumOutV1V2() {
		
		println();
		println("SUM OUT V1 and V2 frpm F1");
		println("-------------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V1,V2);
		TableFactor f1SumOutV1V2 = (TableFactor)f1.sumOut(variablesToSumOut);
		f1SumOutV1V2.setName("f1SumOutV1V2");
		
		println(f1SumOutV1V2);
		assertEquals("f1SumOutV1V2[{V3:card=4}]: [6.0, 6.0, 6.0, 6.0]", f1SumOutV1V2.toString());
		
		println();
	}
	
	@Test
	public void testf1SumOutV1V3() {
		
		println();
		println("SUM OUT V1 and V3 from F1");
		println("-------------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V1,V3);
		TableFactor f1SumOutV1V3 = (TableFactor)f1.sumOut(variablesToSumOut);
		f1SumOutV1V3.setName("f1SumOutV1V3");
		
		println(f1SumOutV1V3);
		assertEquals("f1SumOutV1V3[{V2:card=3}]: [8.0, 8.0, 8.0]", f1SumOutV1V3.toString());
		
		println();
	}
	
	@Test
	public void testf1SumOutV2V3() {
		
		println();
		println("SUM OUT V2 and V3 from F1");
		println("-------------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V2,V3);
		TableFactor f1SumOutV2V3 = (TableFactor)f1.sumOut(variablesToSumOut);
		f1SumOutV2V3.setName("f1SumOutV2V3");
		
		println(f1SumOutV2V3);
		assertEquals("f1SumOutV2V3[{V1:card=2}]: [12.0, 12.0]", f1SumOutV2V3.toString());
		
		println();
	}
	
	
	
	@Test
	public void testf1SumOutV1V2V3() {
		
		println();
		println("SUM OUT V1, V2, and V3 from F1");
		println("-------------------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V1,V2,V3);
		ConstantFactor f1SumOutV1V2V3 = (ConstantFactor) f1.sumOut(variablesToSumOut);
		//f1SumOutV1V2V3.setName("f1SumOutV1V2V3");  cannot set name of a [now] ConstantFactor
		
		print("f1SumOutV1V2V3: "); println(f1SumOutV1V2V3);
		assertEquals("24.0", f1SumOutV1V2V3.toString());
		
		println();
	}
	
	@Test
	public void testf2SumOutV2V4() {
		
		println();
		println("SUM OUT V2 and V4 from F2");
		println("-------------------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V2,V4);
		ConstantFactor f2SumOutV2V4 = (ConstantFactor) f2.sumOut(variablesToSumOut);
		//f1SumOutV1V2V3.setName("f1SumOutV1V2V3");  cannot set name of a [now] ConstantFactor
		
		print("f2SumOutV2V4: "); println(f2SumOutV2V4);
		assertEquals("129.0", f2SumOutV2V4.toString());
		
		println();
	}
	
	
	
	@Test
	public void testf2f1SumOutV1() {
		
		println();
		println("SUM OUT V1 from F2*F1");
		println("------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V1);
		TableFactor f2f1SumOutV1 = (TableFactor) (f2.multiply(f1)).sumOut(variablesToSumOut);
		f2f1SumOutV1.setName("f2f1SumOutV1");
		
		println(f2f1SumOutV1);
		assertEquals("f2f1SumOutV1[{V2:card=3}, {V4:card=2}, {V3:card=4}]: [22.0, 22.0, 22.0, 22.0, "
																		 + "24.0, 24.0, 24.0, 24.0, "
																		 + "42.0, 42.0, 42.0, 42.0, "
																		 + "44.0, 44.0, 44.0, 44.0, "
																		 + "62.0, 62.0, 62.0, 62.0, "
																		 + "64.0, 64.0, 64.0, 64.0]", f2f1SumOutV1.toString());
		
		println();
	}
	
	@Test
	public void testf2f1SumOutV1V2() {
		
		println();
		println("SUM OUT V1 and V2 from F2*F1");
		println("------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V1,V2);
		TableFactor f2f1SumOutV1V2 = (TableFactor) (f2.multiply(f1)).sumOut(variablesToSumOut);
		f2f1SumOutV1V2.setName("f2f1SumOutV1V2");
		
		println(f2f1SumOutV1V2);
		assertEquals("f2f1SumOutV1V2[{V4:card=2}, {V3:card=4}]: [126.0, 126.0, 126.0, 126.0, 132.0, 132.0, 132.0, 132.0]", f2f1SumOutV1V2.toString());
		
		println();
	}
	
	@Test
	public void testf2f1SumOutV1V2V3() {
		
		println();
		println("SUM OUT V1, V2, and V3 from F2*F1");
		println("------------------");

		ArrayList<TableVariable> variablesToSumOut = Util.arrayList(V1,V2,V3);
		TableFactor f2f1SumOutV1V2V3 = (TableFactor) (f2.multiply(f1)).sumOut(variablesToSumOut);
		f2f1SumOutV1V2V3.setName("f2f1SumOutV1V2");
		
		println(f2f1SumOutV1V2V3);
		assertEquals("f2f1SumOutV1V2[{V4:card=2}]: [504.0, 528.0]", f2f1SumOutV1V2V3.toString());
		
		println();
	}
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
