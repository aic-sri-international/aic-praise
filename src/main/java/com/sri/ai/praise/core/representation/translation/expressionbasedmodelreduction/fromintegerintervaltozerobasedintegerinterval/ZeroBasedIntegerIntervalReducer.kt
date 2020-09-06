package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval

import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelReducer

class ZeroBasedIntegerIntervalReducer: ExpressionBasedModelReducer {

    override fun invoke(expressionBasedModel: ExpressionBasedModel) =
            ZeroBasedIntegerIntervalReduction(expressionBasedModel)
}