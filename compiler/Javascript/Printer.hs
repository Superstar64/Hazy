-- |
-- Pretty printer for Javascript's concrete syntax tree.
-- This is closely derived from the Ecmascript Standard 16th Edition.
-- Note that a lot of the abstract syntax tree is missing because it's not used.
module Javascript.Printer
  ( module Javascript.Printer.Lexer,
    module Javascript.Printer.Tree.AdditiveExpression,
    module Javascript.Printer.Tree.ArgumentList,
    module Javascript.Printer.Tree.Arguments,
    module Javascript.Printer.Tree.ArrowFunction,
    module Javascript.Printer.Tree.ArrowParameters,
    module Javascript.Printer.Tree.AssignmentExpression,
    module Javascript.Printer.Tree.BindingElement,
    module Javascript.Printer.Tree.BindingList,
    module Javascript.Printer.Tree.BitwiseANDExpression,
    module Javascript.Printer.Tree.BitwiseORExpression,
    module Javascript.Printer.Tree.BitwiseXORExpression,
    module Javascript.Printer.Tree.Block,
    module Javascript.Printer.Tree.BreakStatement,
    module Javascript.Printer.Tree.CallExpression,
    module Javascript.Printer.Tree.ClassElementName,
    module Javascript.Printer.Tree.ConciseBody,
    module Javascript.Printer.Tree.ConditionalExpression,
    module Javascript.Printer.Tree.Declaration,
    module Javascript.Printer.Tree.EqualityExpression,
    module Javascript.Printer.Tree.ExponentiationExpression,
    module Javascript.Printer.Tree.ExportDeclaration,
    module Javascript.Printer.Tree.ExportsList,
    module Javascript.Printer.Tree.ExportSpecifier,
    module Javascript.Printer.Tree.Expression,
    module Javascript.Printer.Tree.FormalParameterList,
    module Javascript.Printer.Tree.FormalParameters,
    module Javascript.Printer.Tree.FromClause,
    module Javascript.Printer.Tree.IfStatement,
    module Javascript.Printer.Tree.ImportClause,
    module Javascript.Printer.Tree.ImportDeclaration,
    module Javascript.Printer.Tree.ImportsList,
    module Javascript.Printer.Tree.ImportSpecifier,
    module Javascript.Printer.Tree.Initializer,
    module Javascript.Printer.Tree.LabelledItem,
    module Javascript.Printer.Tree.LabelledStatement,
    module Javascript.Printer.Tree.LeftHandSideExpression,
    module Javascript.Printer.Tree.LetOrConst,
    module Javascript.Printer.Tree.LexicalBinding,
    module Javascript.Printer.Tree.LexicalDeclaration,
    module Javascript.Printer.Tree.Literal,
    module Javascript.Printer.Tree.LiteralPropertyName,
    module Javascript.Printer.Tree.LogicalANDExpression,
    module Javascript.Printer.Tree.LogicalORExpression,
    module Javascript.Printer.Tree.MemberExpression,
    module Javascript.Printer.Tree.MethodDefinition,
    module Javascript.Printer.Tree.ModuleExportName,
    module Javascript.Printer.Tree.ModuleItem,
    module Javascript.Printer.Tree.MultiplicativeExpression,
    module Javascript.Printer.Tree.NamedExports,
    module Javascript.Printer.Tree.ObjectLiteral,
    module Javascript.Printer.Tree.NamedImports,
    module Javascript.Printer.Tree.NewExpression,
    module Javascript.Printer.Tree.PrimaryExpression,
    module Javascript.Printer.Tree.PropertyDefinition,
    module Javascript.Printer.Tree.PropertyDefinitionList,
    module Javascript.Printer.Tree.PropertyName,
    module Javascript.Printer.Tree.RelationExpression,
    module Javascript.Printer.Tree.ReturnStatement,
    module Javascript.Printer.Tree.ShiftExpression,
    module Javascript.Printer.Tree.ShortCircuitExpression,
    module Javascript.Printer.Tree.SingleNameBinding,
    module Javascript.Printer.Tree.Statement,
    module Javascript.Printer.Tree.StatementListItem,
    module Javascript.Printer.Tree.UnaryExpression,
    module Javascript.Printer.Tree.UpdateExpression,
    module Javascript.Printer.Tree.WithClause,
  )
where

import Javascript.Printer.Lexer
  ( Identifier,
    Number,
    String,
    bigInt,
    identifier,
    int,
    string,
  )
import Javascript.Printer.Tree.AdditiveExpression
  ( AdditivieExpression,
    additivieExpression1,
  )
import Javascript.Printer.Tree.ArgumentList
  ( ArgumentList,
    argumentList1,
    argumentList3,
  )
import Javascript.Printer.Tree.Arguments
  ( Arguments,
    arguments1,
    arguments2,
  )
import Javascript.Printer.Tree.ArrowFunction
  ( ArrowFunction,
    arrowFunction,
  )
import Javascript.Printer.Tree.ArrowParameters
  ( ArrowParameters,
    arrowParameters1,
    arrowParameters2,
  )
import Javascript.Printer.Tree.AssignmentExpression
  ( AssignmentExpression,
    assignmentExpression1,
    assignmentExpression3,
    assignmentExpression5,
  )
import Javascript.Printer.Tree.BindingElement
  ( BindingElement,
    bindingElement1,
  )
import Javascript.Printer.Tree.BindingList
  ( BindingList,
    bindingList1,
    bindingList2,
  )
import Javascript.Printer.Tree.BitwiseANDExpression
  ( BitwiseANDExpression,
    bitwiseANDExpression1,
  )
import Javascript.Printer.Tree.BitwiseORExpression
  ( BitwiseORExpression,
    bitwiseORExpression1,
  )
import Javascript.Printer.Tree.BitwiseXORExpression
  ( BitwiseXORExpression,
    bitwiseXORExpression1,
  )
import Javascript.Printer.Tree.Block
  ( Block,
    block,
  )
import Javascript.Printer.Tree.BreakStatement
  ( BreakStatement,
    breakStatement2,
  )
import Javascript.Printer.Tree.CallExpression
  ( CallExpression,
    callExpression1,
    callExpression4,
    callExpression6,
  )
import Javascript.Printer.Tree.ClassElementName
  ( ClassElementName,
    classElementName1,
  )
import Javascript.Printer.Tree.ConciseBody
  ( ConciseBody,
    conciseBody2,
  )
import Javascript.Printer.Tree.ConditionalExpression
  ( ConditionalExpression,
    conditionalExpression1,
    conditionalExpression2,
  )
import Javascript.Printer.Tree.Declaration
  ( Declaration,
    declaration3,
  )
import Javascript.Printer.Tree.EqualityExpression
  ( EqualityExpression,
    equalityExpression1,
    equalityExpression4,
  )
import Javascript.Printer.Tree.ExponentiationExpression
  ( ExponentiationExpression,
    exponentiationExpression1,
  )
import Javascript.Printer.Tree.ExportDeclaration
  ( ExportDeclaration,
    exportDeclaration2,
  )
import Javascript.Printer.Tree.ExportSpecifier
  ( ExportSpecifier,
    exportSpecifier2,
  )
import Javascript.Printer.Tree.ExportsList
  ( ExportsList,
    exportsList1,
  )
import Javascript.Printer.Tree.Expression
  ( Expression,
    expression1,
  )
import Javascript.Printer.Tree.FormalParameterList
  ( FormalParameterList,
    formalParameterList1,
    formalParameterList2,
  )
import Javascript.Printer.Tree.FormalParameters
  ( FormalParameters,
    formalParameters1,
    formalParameters3,
  )
import Javascript.Printer.Tree.FromClause
  ( FromClause,
    fromClause1,
  )
import Javascript.Printer.Tree.IfStatement
  ( IfStatement,
    ifStatement2,
  )
import Javascript.Printer.Tree.ImportClause
  ( ImportClause,
    importClause3,
  )
import Javascript.Printer.Tree.ImportDeclaration
  ( ImportDeclaration,
    importDeclaration1,
  )
import Javascript.Printer.Tree.ImportSpecifier
  ( ImportSpecifier,
    importSpecifier2,
  )
import Javascript.Printer.Tree.ImportsList
  ( ImportsList,
    importsList1,
  )
import Javascript.Printer.Tree.Initializer
  ( Initializer,
    initializer1,
  )
import Javascript.Printer.Tree.LabelledItem
  ( LabelledItem,
    labelledItem1,
  )
import Javascript.Printer.Tree.LabelledStatement
  ( LabelledStatement,
    labelledStatement,
  )
import Javascript.Printer.Tree.LeftHandSideExpression
  ( LeftHandSideExpression,
    leftHandSideExpression1,
    leftHandSideExpression2,
  )
import Javascript.Printer.Tree.LetOrConst
  ( LetOrConst,
    letOrConst1,
    letOrConst2,
  )
import Javascript.Printer.Tree.LexicalBinding
  ( LexicalBinding,
    lexicalBinding1,
  )
import Javascript.Printer.Tree.LexicalDeclaration
  ( LexicalDeclaration,
    lexicalDeclaration,
  )
import Javascript.Printer.Tree.Literal
  ( Literal,
    literal3,
    literal4,
  )
import Javascript.Printer.Tree.LiteralPropertyName
  ( LiteralPropertyName,
    literalPropertyName1,
  )
import Javascript.Printer.Tree.LogicalANDExpression
  ( LogicalANDExpression,
    logicialANDExpression1,
  )
import Javascript.Printer.Tree.LogicalORExpression
  ( LogicalORExpression,
    logicialORExpression1,
  )
import Javascript.Printer.Tree.MemberExpression
  ( MemberExpression,
    memberExpression1,
    memberExpression3,
  )
import Javascript.Printer.Tree.MethodDefinition
  ( MethodDefinition,
    methodDefinition1,
  )
import Javascript.Printer.Tree.ModuleExportName
  ( ModuleExportName,
    moduleExportName2,
  )
import Javascript.Printer.Tree.ModuleItem
  ( ModuleItem,
    moduleItem1,
    moduleItem2,
    moduleItem3,
  )
import Javascript.Printer.Tree.MultiplicativeExpression
  ( MultiplicativeExpression,
    multiplicativeExpression1,
  )
import Javascript.Printer.Tree.NamedExports
  ( NamedExports,
    namedExports2,
  )
import Javascript.Printer.Tree.NamedImports
  ( NamedImports,
    namedImports2,
  )
import Javascript.Printer.Tree.NewExpression
  ( NewExpression,
    newExpression1,
  )
import Javascript.Printer.Tree.ObjectLiteral
  ( ObjectLiteral,
    objectLiteral1,
    objectLiteral2,
  )
import Javascript.Printer.Tree.PrimaryExpression
  ( PrimaryExpression,
    primaryExpression1,
    primaryExpression13,
    primaryExpression2,
    primaryExpression3,
    primaryExpression5,
  )
import Javascript.Printer.Tree.PropertyDefinition
  ( PropertyDefinition,
    propertyDefinition3,
    propertyDefinition4,
  )
import Javascript.Printer.Tree.PropertyDefinitionList
  ( PropertyDefinitionList,
    propertyDefinitionList1,
    propertyDefinitionList2,
  )
import Javascript.Printer.Tree.PropertyName
  ( PropertyName,
    propertyName1,
  )
import Javascript.Printer.Tree.RelationExpression
  ( RelationalExpression,
    relationalExpression1,
  )
import Javascript.Printer.Tree.ReturnStatement
  ( ReturnStatement,
    returnStatement2,
  )
import Javascript.Printer.Tree.ShiftExpression
  ( ShiftExpression,
    shiftExpression1,
  )
import Javascript.Printer.Tree.ShortCircuitExpression
  ( ShortCircuitExpression,
    shortCircuitExpression1,
  )
import Javascript.Printer.Tree.SingleNameBinding
  ( SingleNameBinding,
    singleNameBinding,
  )
import Javascript.Printer.Tree.Statement
  ( Statement,
    statement1,
    statement11,
    statement4,
    statement5,
    statement8,
    statement9,
  )
import Javascript.Printer.Tree.StatementListItem
  ( StatementListItem,
    statementListItem1,
    statementListItem2,
  )
import Javascript.Printer.Tree.UnaryExpression
  ( UnaryExpression,
    unaryExpression1,
  )
import Javascript.Printer.Tree.UpdateExpression
  ( UpdateExpression,
    updateExpression1,
  )
import Javascript.Printer.Tree.WithClause (WithClause)
