package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromextensionalsets

import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelReducer

class ExtensionalSetsReducer: ExpressionBasedModelReducer {

    override fun invoke(expressionBasedModel: ExpressionBasedModel) = ExtensionalSetsReduction(expressionBasedModel)
}