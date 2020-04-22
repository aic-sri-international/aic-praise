package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.arrayTableFactor;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.AnytimeExactBPSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBPSolver;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;


public class AnytimeExactBPWithSimplificationTest {

	@Test
	public void test() {
		
		AnytimeExactBPSolver solver = new AnytimeExactBPSolver();

		FactorNetwork factorNetwork;
		Variable query;
		
		TableVariable a = new TableVariable("a", 2);
		TableVariable b = new TableVariable("b", 2);
		TableVariable c = new TableVariable("c", 2);
		
		query = b;
		
		factorNetwork = new DefaultFactorNetwork(
				arrayTableFactor(list(a), va -> va == 1? 0.8 : 0.2),
				arrayTableFactor(list(c), vc -> vc == 1? 0.7 : 0.3),
				arrayTableFactor(list(a, b, c), (va, vb, vc) -> vb == va? 1.0 : 0.0)
				);
		
		println("Exact: ", new ExactBPSolver().apply(query, factorNetwork));
		
		var it = solver.apply(query, factorNetwork);
		
		while (it.hasNext()) {
			println(it.next());
		}
	}	
}
