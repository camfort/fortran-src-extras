{- | Aeson instances for the Fortran AST defined in fortran-src.

As of fortran-src v0.10.0, most node types store an annotation and a 'SrcSpan'.
The general approach to instance design is as follows:

  * Annotations are placed in @anno@ fields.
  * Spans are placed in @span@ fields.
  * Where possible, we use a generic derivation that takes field names from the
    data type. (This works for most single-constructor product types.)
  * For sum types, an object is created storing an annotation, span and tag. The
    tag indicates the constructor being used. The other fields are then
    "flattened" into the tag object. (This isn't what Aeson's generic derivation
    does by default due to safety concerns, but it can be nicer for JSON.)

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.Extras.JSON() where

import Language.Fortran.Extras.JSON.Helpers
import Language.Fortran.Extras.JSON.Supporting()
import Language.Fortran.Extras.JSON.Literals()
import Data.Aeson hiding ( Value )
import Language.Fortran.AST
import qualified Data.Text as Text
import Data.Text ( Text )

-- Assorted
-- DerivingVia needs GHC 8.6
--deriving via String instance ToJSON (Comment a)
instance ToJSON (Comment a) where
    toJSON     (Comment str) = toJSON     str
    toEncoding (Comment str) = toEncoding str

instance ToJSON BaseType where
    toJSON     = toJSON     . aesonBaseTypeHelper
    toEncoding = toEncoding . aesonBaseTypeHelper

aesonBaseTypeHelper :: BaseType -> Text
aesonBaseTypeHelper = \case
  TypeInteger         -> "integer"
  TypeReal            -> "real"
  TypeDoublePrecision -> "double_precision"
  TypeComplex         -> "complex"
  TypeDoubleComplex   -> "double_complex"
  TypeLogical         -> "logical"
  TypeCharacter       -> "character"
  TypeCustom a        -> "custom:"<>Text.pack a
  TypeByte            -> "byte"
  ClassStar           -> "star"
  ClassCustom a       -> "custom:class:"<>Text.pack a

instance ToJSON Intent where
    toJSON     = gtj $ jcEnumDrop ""
    toEncoding = gte $ jcEnumDrop ""

-- USE statements
instance ToJSON ModuleNature where
    toJSON     = gtj $ jcEnumDrop "Mod"
    toEncoding = gte $ jcEnumDrop "Mod"
instance ToJSON Only where
    toJSON     = gtj $ jcEnumDrop ""
    toEncoding = gte $ jcEnumDrop ""

-- Expressions
instance ToJSON UnaryOp where
    toJSON     = gtj $ jcSumDrop ""
    toEncoding = gte $ jcSumDrop ""
instance ToJSON BinaryOp where
    toJSON     = gtj $ jcSumDrop ""
    toEncoding = gte $ jcSumDrop ""

--------------------------------------------------------------------------------

-- standalone apart from a, SrcSpan
instance ToJSON a => ToJSON (Prefix a) where toJSON = gtj $ jcSumDrop "Pfx"

instance ToJSON a => ToJSON (Value a) where
    toJSON     = gtj $ jcSumDrop "Val"
    toEncoding = gte $ jcSumDrop "Val"

instance ToJSON a => ToJSON (Selector a) where
    toJSON     = gtj $ jcProdDrop "selector"
    toEncoding = gte $ jcProdDrop "selector"

instance ToJSON a => ToJSON (TypeSpec a) where
    toJSON     = gtj $ jcProdDrop "typeSpec"
    toEncoding = gte $ jcProdDrop "typeSpec"

instance ToJSON a => ToJSON (DimensionDeclarator a) where
    toJSON     = gtj $ jcProdDrop "dimDecl"
    toEncoding = gte $ jcProdDrop "dimDecl"

instance ToJSON a => ToJSON (Declarator a) where
    toJSON     d = object $ fieldsMain <> fieldsType
      where
        fieldsMain =
          [ "anno"     .= declaratorAnno     d
          , "span"     .= declaratorSpan     d
          , "variable" .= declaratorVariable d
          , "length"   .= declaratorLength   d
          , "initial"  .= declaratorInitial  d
          ]
        fieldsType = case declaratorType d of
                       ScalarDecl      -> [ "type" .= String "scalar" ]
                       ArrayDecl  dims -> [ "type" .= String "array"
                                          , "dims" .= toJSON dims ]
    -- TODO toEncoding

instance ToJSON a => ToJSON (Suffix a) where
    toJSON (SfxBind a s e) = tja "bind" a s ["expression" .= e]
    -- TODO toEncoding

instance ToJSON a => ToJSON (Attribute a) where
    toJSON = \case
      AttrParameter    a s      -> tja "parameter" a s []
      AttrPublic       a s      -> tja "public" a s []
      AttrProtected    a s      -> tja "protected" a s []
      AttrPrivate      a s      -> tja "private" a s []
      AttrAllocatable  a s      -> tja "allocatable" a s []
      AttrDimension    a s dims -> tja "dimension" a s ["dimensions" .= dims]
      AttrExternal     a s      -> tja "external" a s []
      AttrIntent       a s int  -> tja "intent" a s ["intent" .= int]
      AttrOptional     a s      -> tja "optional" a s []
      AttrPointer      a s      -> tja "pointer" a s []
      AttrSave         a s      -> tja "save" a s []
      AttrTarget       a s      -> tja "target" a s []
      AttrIntrinsic    a s      -> tja "intrinsic" a s []
      AttrAsynchronous a s      -> tja "asynchronous" a s []
      AttrSuffix       a s sfx  -> tja "suffix" a s ["suffix" .= sfx]
      AttrValue        a s      -> tja "value" a s []
      AttrVolatile     a s      -> tja "volatile" a s []
    -- TODO toEncoding

--------------------------------------------------------------------------------

instance ToJSON a => ToJSON (StructureItem a) where
    toJSON = \case
      StructFields a s t attrs decls -> tja "fields" a s
        ["type" .= t, "attributes" .= attrs, "declarators" .= decls]
      StructUnion a s maps -> tja "union" a s ["maps" .= maps]
      StructStructure a s name fname decls -> tja "structure" a s
        ["name" .= fname, "substructure_name" .= name, "fields" .= decls]
    -- TODO toEncoding
instance ToJSON a => ToJSON (UnionMap a) where
    toJSON     = gtj $ jcProdDrop "unionMap"
    toEncoding = gte $ jcProdDrop "unionMap"

-- TODO rec: Expression
instance ToJSON a => ToJSON (DataGroup a) where
    toJSON     = gtj $ jcProdDrop "dataGroup"
    toEncoding = gte $ jcProdDrop "dataGroup"

-- TODO rec: Expression (only ExpValue (ValVariable))
instance ToJSON a => ToJSON (Namelist a) where
    toJSON     = gtj $ jcProdDrop "namelist"
    toEncoding = gte $ jcProdDrop "namelist"

instance ToJSON a => ToJSON (CommonGroup a) where
    toJSON     = gtj $ jcProdDrop "commonGroup"
    toEncoding = gte $ jcProdDrop "commonGroup"

-- TODO not in original package, no field names
instance ToJSON a => ToJSON (FormatItem a) where
    toJSON     = gtj $ jcSumDrop "FI"
    toEncoding = gte $ jcSumDrop "FI"

instance ToJSON a => ToJSON (ImpList a) where
    toJSON     = gtj $ jcProdDrop "impList"
    toEncoding = gte $ jcProdDrop "impList"

instance ToJSON a => ToJSON (ImpElement a) where
    toJSON     = gtj $ jcProdDrop "impElement"
    toEncoding = gte $ jcProdDrop "impElement"

-- random
instance ToJSON a => ToJSON (ControlPair a) where
    toJSON     = gtj $ jcProdDrop "controlPair"
    toEncoding = gte $ jcProdDrop "controlPair"

instance ToJSON a => ToJSON (FlushSpec a) where
    toJSON     = \case
      FSUnit   a s e -> tja "unit" a s ["expression" .= e]
      FSIOStat a s e -> tja "unit" a s ["expression" .= e]
      FSIOMsg  a s e -> tja "unit" a s ["expression" .= e]
      FSErr    a s e -> tja "unit" a s ["expression" .= e]
    -- TODO toEncoding

instance ToJSON a => ToJSON (AllocOpt a) where
    toJSON     = \case
      AOStat   a s e -> tja "stat" a s ["expression" .= e]
      AOErrMsg a s e -> tja "stat" a s ["expression" .= e]
      AOSource a s e -> tja "stat" a s ["expression" .= e]
    -- TODO toEncoding

instance ToJSON a => ToJSON (Use a) where
    toJSON     = \case
      UseRename a s eLocal eUse -> tja "rename" a s
        [ "local" .= eLocal, "use" .= eUse ]
      UseID     a s e           -> tja "id"     a s ["name" .= e]
    -- TODO toEncoding

instance ToJSON a => ToJSON (ProcInterface a) where
    toJSON     = \case
      ProcInterfaceName a s e -> tja "name" a s ["name" .= e]
      ProcInterfaceType a s t -> tja "type" a s ["type" .= t]
    -- TODO toEncoding

instance ToJSON a => ToJSON (ProcDecl a) where
    toJSON     = gtj $ jcProdDrop "procDecl"
    toEncoding = gte $ jcProdDrop "procDecl"

-- depends on statement, expression
instance ToJSON a => ToJSON (DoSpecification a) where
    toJSON     = gtj $ jcProdDrop "doSpec"
    toEncoding = gte $ jcProdDrop "doSpec"

instance ToJSON a => ToJSON (Index a) where
  toJSON idx = case idx of
    IxSingle a s nm e    -> tja "single" a s ["name" .= nm, "index" .= e]
    IxRange  a s l  u st -> tja "range"  a s
        ["lower" .= l, "upper" .= u, "stride" .= st]
    -- TODO toEncoding

instance ToJSON a => ToJSON (Argument a) where
    toJSON     = gtj $ jcProdDrop "argument"
    toEncoding = gte $ jcProdDrop "argument"

-- weird part of the AST due to annotations and naming
instance ToJSON a => ToJSON (ArgumentExpression a) where
    toJSON     = gtj $ jcSumDrop "Arg"
    toEncoding = gte $ jcSumDrop "Arg"

instance ToJSON a => ToJSON (ForallHeader a) where
    toJSON     = gtj $ jcProdDrop "forallHeader"
    toEncoding = gte $ jcProdDrop "forallHeader"

instance ToJSON a => ToJSON (ForallHeaderPart a) where
    toJSON     = gtj $ jcProdDrop "forallHeaderPart"
    toEncoding = gte $ jcProdDrop "forallHeaderPart"

instance ToJSON MetaInfo where
    toJSON     = gtj $ jcSumDrop "mi"
    toEncoding = gte $ jcSumDrop "mi"

instance ToJSON a => ToJSON (ProgramFile a) where
    toJSON     = gtj $ jcProdDrop "programFile"
    toEncoding = gte $ jcProdDrop "programFile"

instance ToJSON a => ToJSON (Expression a) where
    toJSON = \case
      ExpValue          a s val      ->
        tja "value"          a s ["value" .= val]
      ExpBinary         a s op el er ->
        tja "binary"         a s ["op" .= op, "left" .= el, "right" .= er]
      ExpUnary          a s op e ->
        tja "unary"          a s ["op" .= op, "expression" .= e]
      ExpSubscript      a s e idxs ->
        tja "subscript"      a s ["expression" .= e, "indices" .= idxs]
      ExpDataRef        a s e1 e2 ->
        tja "deref"          a s ["expression" .= e1, "field" .= e2]
      ExpFunctionCall   a s fn args ->
        tja "function_call"  a s ["function" .= fn, "arguments" .= args]
      ExpImpliedDo      a s exps spec ->
        tja "implied_do"     a s ["expressions" .= exps, "do_spec" .= spec]
      ExpInitialisation a s exps ->
        tja "initialisation" a s ["expressions" .= exps]
      ExpReturnSpec     a s tgt  ->
        tja "return_spec"    a s ["target" .= tgt]
    -- TODO toEncoding

instance ToJSON a => ToJSON (Block a) where
    toJSON = \case
      BlStatement a s l st -> tja "statement" a s
        ["label" .= l, "statement" .= st]
      BlIf a s l nm conds blocks endlabel -> tja "if" a s
        [ "label"      .= l
        , "name"       .= nm
        , "conditions" .= conds
        , "blocks"     .= blocks
        , "end_label"  .= endlabel
        ]
      BlCase a s l nm scrut ranges blocks endlabel -> tja "case" a s
        [ "label"     .= l
        , "name"      .= nm
        , "scrutinee" .= scrut
        , "ranges"    .= ranges
        , "blocks"    .= blocks
        , "end_label" .= endlabel
        ]
      BlDo a s l nm target dospec body endlabel -> tja "do" a s
        [ "label"     .= l
        , "name"      .= nm
        , "target"    .= target
        , "do_spec"   .= dospec
        , "body"      .= body
        , "end_label" .= endlabel
        ]
      BlDoWhile a s l nm target cond body endlabel -> tja "do_while" a s
        [ "label"     .= l
        , "name"      .= nm
        , "target"    .= target
        , "condition" .= cond
        , "body"      .= body
        , "end_label" .= endlabel
        ]
      BlInterface a s l decls blocks _ -> tja "interface" a s
        ["label" .= l, "declarations" .= decls, "blocks" .= blocks]
      BlForall a s ml mn h bs mel -> tja "forall" a s
        [ "label"     .= ml
        , "name"      .= mn
        , "header"    .= h
        , "blocks"    .= bs
        , "end_label" .= mel
        ]
      BlAssociate a s ml mn abbrevs bs mel -> tja "associate" a s
        [ "label"     .= ml
        , "name"      .= mn
        , "abbrevs"   .= abbrevs
        , "blocks"    .= bs
        , "end_label" .= mel
        ]
      BlComment a s c -> tja "comment" a s ["comment" .= c]
    -- TODO toEncoding

instance ToJSON a => ToJSON (ProgramUnit a) where
    toJSON = \case
      PUMain a s name blocks pus -> tja "main" a s
        ["name" .= name, "blocks" .= blocks, "subprograms" .= pus]
      PUModule a s name blocks pus -> tja "module" a s
        ["name" .= name, "blocks" .= blocks, "subprograms" .= pus]
      PUSubroutine a s pfxsfx name args blocks pus -> tja "subroutine" a s
        [ "name"        .= name
        , "arguments"   .= args
        , "blocks"      .= blocks
        , "subprograms" .= pus
        , "options"     .= pfxsfx
        ]
      PUFunction a s t _ name args res blocks pus -> tja "function" a s
        [ "name" .= name
        , "type" .= t
        , "arguments" .= args
        , "blocks" .= blocks
        , "result" .= res
        , "subprograms" .= pus
        ]
      PUBlockData a s name blocks -> tja "block_data" a s
        ["name" .= name, "blocks" .= blocks]
      PUComment a s c -> tja "comment" a s ["comment" .= c]
    -- TODO toEncoding

instance ToJSON a => ToJSON (Statement a) where
    toJSON st = case st of
      StOptional    a s es -> tja "optional"  a s ["vars" .= es]
      StPublic      a s es -> tja "public"    a s ["vars" .= es]
      StPrivate     a s es -> tja "private"   a s ["vars" .= es]
      StProtected   a s es -> tja "protected" a s ["vars" .= es]
      StExternal    a s es -> tja "external"  a s ["vars" .= es]
      StIntrinsic   a s es -> tja "intrinsic" a s ["vars" .= es]

      StDimension    a s ds -> tja "dimension"    a s ["declarators" .= ds]
      StAllocatable  a s ds -> tja "allocatable"  a s ["declarators" .= ds]
      StAsynchronous a s ds -> tja "asynchronous" a s ["declarators" .= ds]
      StPointer      a s ds -> tja "pointer"      a s ["declarators" .= ds]
      StTarget       a s ds -> tja "target"       a s ["declarators" .= ds]
      StValue        a s ds -> tja "value"        a s ["declarators" .= ds]
      StVolatile     a s ds -> tja "volatile"     a s ["declarators" .= ds]
      StParameter    a s ds -> tja "parameter"    a s ["declarators" .= ds]
      StAutomatic    a s ds -> tja "automatic"    a s ["declarators" .= ds]
      StStatic       a s ds -> tja "static"       a s ["declarators" .= ds]

      StDeclaration a s t attrs ds -> tja "declaration" a s
        ["type" .= t, "attributes" .= attrs, "declarators" .= ds]
      StStructure a s name ds -> tja "structure" a s
        ["name" .= name, "fields" .= ds]
      StIntent a s intent es      -> tja "intent" a s
        ["intent" .= intent, "vars" .= es]

      StSave a s args -> tja "save" a s ["vars" .= args]

      StData a s args     -> tja "data" a s ["data_groups" .= args]
      StNamelist a s nls -> tja "namelist" a s ["namelists" .= nls]
      StCommon a s args -> tja "common" a s ["common_groups" .= args]
      StEquivalence a s args -> tja "equivalence" a s ["groups" .= args]
      StFormat a s fis -> tja "format" a s ["parts" .= fis]
      StImplicit a s itms    -> tja "implicit" a s ["items" .= itms]
      StEntry a s v args r -> tja "entry" a s
        ["name" .= v, "args" .= args, "return" .= r]
      StInclude a s path blocks -> tja "include" a s
        ["path" .= path, "blocks" .= blocks]

      StDo      a s nm lbl spec -> tja "do"       a s
        ["name" .= nm, "label" .= lbl, "do_spec" .= spec]
      StDoWhile a s nm lbl cond -> tja "do_while" a s
        ["name" .= nm, "label" .= lbl, "condition" .= cond]
      StEnddo   a s nm          -> tja "end_do"   a s ["name" .= nm]

      StCycle a s v -> tja "cycle" a s ["var" .= v]
      StExit  a s v -> tja "exit"  a s ["var" .= v]

      StFormatBogus a s fmt -> tja "format" a s ["format" .= fmt]

      StForallStatement a s h stmt -> tja "forall_statement" a s
        ["header" .= h, "statement" .= stmt]

      StIfLogical a s cond stmt -> tja "if_logical" a s
        ["condition" .= cond, "statement" .= stmt]
      StIfArithmetic a s e lt eq gt -> tja "if_arithmetic" a s
        [ "expression"    .= e
        , "less"    .= lt
        , "equal"   .= eq
        , "greater" .= gt ]

      StSelectCase a s nm e    -> tja "select_case" a s
        ["name" .= nm, "expression" .= e]
      StCase       a s nm idxs -> tja "case"        a s
        ["name" .= nm, "indices" .= idxs]
      StEndcase    a s nm      -> tja "end_select"  a s
        ["name" .= nm]

      StFunction a s fn args body -> tja "function" a s
        ["name" .= fn, "arguments" .= args, "body" .= body]

      StExpressionAssign a s tgt e     -> tja "assign_expression" a s
        ["target" .= tgt, "expression" .= e]
      StPointerAssign    a s eFrom eTo -> tja "assign_pointer"    a s
        ["target" .= eFrom, "expression" .= eTo]
      StLabelAssign      a s lbl tgt   -> tja "assign_label"      a s
        ["target" .= tgt, "label" .= lbl]

      StGotoUnconditional a s tgt      -> tja "goto"          a s
        ["target" .= tgt]
      StGotoAssigned      a s tgt lbls -> tja "goto_assigned" a s
        ["target" .= tgt, "labels" .= lbls]
      StGotoComputed      a s lbls tgt -> tja "goto_computed" a s
        ["target" .= tgt, "labels" .= lbls]

      StCall   a s fn args -> tja "call"   a s
        ["function" .= fn, "arguments" .= args]
      StReturn a s tgt     -> tja "return" a s
        ["span" .= s, "target" .= tgt]

      StContinue a s     -> tja "continue" a s []
      StStop     a s msg -> tja "stop"     a s ["message" .= msg]
      StPause    a s msg -> tja "pause"    a s ["message" .= msg]

      StRead      a s fmt args -> tja "read"  a s
        ["format" .= fmt, "arguments" .= args]
      StRead2     a s fmt args -> tja "read2" a s
        ["format" .= fmt, "arguments" .= args]
      StWrite     a s fmt args -> tja "write" a s
        ["format" .= fmt, "arguments" .= args]
      StPrint     a s fmt args -> tja "print" a s
        ["format" .= fmt, "arguments" .= args]
      StTypePrint a s fmt args -> tja "type_print"  a s
        ["format" .= fmt, "arguments" .= args]

      StOpen       a s spec -> tja "open"       a s ["specification" .= spec]
      StClose      a s spec -> tja "close"      a s ["specification" .= spec]
      StFlush      a s spec -> tja "flush"      a s ["specification" .= spec]
      StInquire    a s spec -> tja "inquire"    a s ["specification" .= spec]
      StRewind     a s spec -> tja "rewind"     a s ["specification" .= spec]
      StRewind2    a s spec -> tja "rewind2"    a s ["specification" .= spec]
      StBackspace  a s spec -> tja "backspace"  a s ["specification" .= spec]
      StBackspace2 a s spec -> tja "backspace2" a s ["specification" .= spec]
      StEndfile    a s spec -> tja "endfile"    a s ["specification" .= spec]
      StEndfile2   a s spec -> tja "endfile2"   a s ["specification" .= spec]

      StAllocate   a s t es os -> tja "allocate"   a s
        ["type" .= t, "pointers" .= es, "options" .= os]
      StNullify    a s es      -> tja "nullify"    a s
        ["pointers" .= es]
      StDeallocate a s es os   -> tja "deallocate" a s
        ["pointers" .= es, "options" .= os]

      StWhere a s e asn -> tja "where" a s
        ["expression" .= e, "assignment" .= asn]

      StWhereConstruct a s nm e -> tja "where_start" a s
        ["name" .= nm, "expression" .= e]
      StElsewhere      a s nm e -> tja "elsewhere"   a s
        ["name" .= nm, "expression" .= e]
      StEndWhere       a s nm   -> tja "end_where"   a s
        ["name" .= nm]

      StUse a s nm mn only imports -> tja "use" a s
        ["module" .= nm, "nature" .= mn, "only" .= only, "import" .= imports]
      StModuleProcedure a s vs -> tja "module_procedure" a s
        ["procedures" .= vs]

      StType    a s attrs nm -> tja "type"     a s
        ["attributes" .= attrs, "name" .= nm]
      StEndType a s       nm -> tja "end_type" a s
        ["name" .= nm]

      StSequence a s -> tja "sequence" a s []

      StForall    a s nm h -> tja "forall"     a s ["name" .= nm, "header" .= h]
      StEndForall a s nm   -> tja "end_forall" a s ["name" .= nm]

      StProcedure a s iface attrs decls -> tja "procedure" a s
        ["interface" .= iface, "attributes" .= attrs, "declarations" .= decls]

      StImport a s nms -> tja "import" a s ["names" .= nms]

      StEnum       a s ->       tja "enum"       a s []
      StEnumerator a s decls -> tja "enumerator" a s ["declarators" .= decls]
      StEndEnum    a s ->       tja "end_enum"   a s []

    -- TODO toEncoding
