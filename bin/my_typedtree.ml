open Migrate_parsetree.Ast_411

let pp_boiler _ _ = ()

let pp_ta_boiler _ _ _ = ()

let to_yojson_boiler _ = `String ""

let to_yojson_ta_boiler _ _ = `String ""

module Stdlib = struct
  include Stdlib

  let ref_to_yojson to_yojson ref = to_yojson !ref
end

module Location = struct
  include Location

  let pp _ _ = ()

  let to_yojson t =
    `String (Format.asprintf ("%a\n" [@reason.raw_literal "%a\\n"]) pp t)
end

module Ident = struct
  include Ident

  module Set = struct
    include Set

    let pp = Ident.Set.print

    let to_yojson t =
      `String (Format.asprintf ("%a\n" [@reason.raw_literal "%a\\n"]) pp t)
  end

  let pp = Ident.print

  let to_yojson t =
    `String (Format.asprintf ("%a\n" [@reason.raw_literal "%a\\n"]) pp t)
end

module Path = struct
  include Path

  type t = [%import: Path.t] [@@deriving show { with_path = false }, to_yojson]
end

module Env = struct
  include Env

  let pp formatter _env =
    Format.fprintf formatter ("env" [@reason.raw_literal "env"])

  let to_yojson t =
    `String (Format.asprintf ("%a\n" [@reason.raw_literal "%a\\n"]) pp t)
end

module Longident = struct
  include Longident

  type t = [%import: Longident.t]
  [@@deriving show { with_path = false }, to_yojson]
end

module Typedtree = struct
  include Typedtree

  type partial = [%import: Typedtree.partial]
    [@@deriving show { with_path = false }, to_yojson]
  
  type attribute = [%import: Typedtree.attribute]
    [@@deriving show { with_path = false }, to_yojson]
  
  type attributes = [%import: Typedtree.attributes]
    [@@deriving show { with_path = false }, to_yojson]
  
  type value = [%import: Typedtree.value]
    [@@deriving show { with_path = false }, to_yojson]
  
  type computation = [%import: Typedtree.computation]
    [@@deriving show { with_path = false }, to_yojson]
  
  type 'a pattern_category = [%import: 'a Typedtree.pattern_category]
    [@@deriving show { with_path = false }, to_yojson]
  
  type pattern = [%import: Typedtree.pattern]
    [@@deriving show { with_path = false }, to_yojson]
  
  and 'k general_pattern = [%import: 'k Typedtree.general_pattern]
    [@@deriving show { with_path = false }, to_yojson]
  
  and 'a pattern_data = [%import: 'k Typedtree.pattern_data]
    [@@deriving show { with_path = false }, to_yojson]
  
  and pat_extra = [%import: Typedtree.pat_extra]
    [@@deriving show { with_path = false }, to_yojson]
  
  and 'k pattern_desc = [%import: 'k Typedtree.pattern_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and tpat_value_argument = [%import: Typedtree.tpat_value_argument]
    [@@deriving show { with_path = false }, to_yojson]
  
  and expression = [%import: Typedtree.expression]
    [@@deriving show { with_path = false }, to_yojson]
  
  and exp_extra = [%import: Typedtree.exp_extra]
    [@@deriving show { with_path = false }, to_yojson]
  
  and expression_desc = [%import: Typedtree.expression_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  type meth = [%import: Typedtree.meth]
    [@@deriving show { with_path = false }, to_yojson]
  
  and 'k case = [%import: 'k Typedtree.case]
    [@@deriving show { with_path = false }, to_yojson]
  
  and record_label_definition = [%import: Typedtree.record_label_definition]
    [@@deriving show { with_path = false }, to_yojson]
  
  and binding_op = [%import: Typedtree.binding_op]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_expr = [%import: Typedtree.class_expr]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_expr_desc = [%import: Typedtree.class_expr_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_structure = [%import: Typedtree.class_structure]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_field = [%import: Typedtree.class_field]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_field_kind = [%import: Typedtree.class_field_kind]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_field_desc = [%import: Typedtree.class_field_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and module_expr = [%import: Typedtree.module_expr]
    [@@deriving show { with_path = false }, to_yojson]
  
  and module_type_constraint = [%import: Typedtree.module_type_constraint]
    [@@deriving show { with_path = false }, to_yojson]
  
  and functor_parameter = [%import: Typedtree.functor_parameter]
    [@@deriving show { with_path = false }, to_yojson]
  
  and module_expr_desc = [%import: Typedtree.module_expr_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and structure = [%import: Typedtree.structure]
    [@@deriving show { with_path = false }, to_yojson]
  
  and structure_item = [%import: Typedtree.structure_item]
    [@@deriving show { with_path = false }, to_yojson]
  
  and structure_item_desc = [%import: Typedtree.structure_item_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and module_binding = [%import: Typedtree.module_binding]
    [@@deriving show { with_path = false }, to_yojson]
  
  and value_binding = [%import: Typedtree.value_binding]
    [@@deriving show { with_path = false }, to_yojson]
  
  and module_coercion = [%import: Typedtree.module_coercion]
    [@@deriving show { with_path = false }, to_yojson]
  
  and module_type = [%import: Typedtree.module_type]
    [@@deriving show { with_path = false }, to_yojson]
  
  and module_type_desc = [%import: Typedtree.module_type_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and primitive_coercion = [%import: Typedtree.primitive_coercion]
    [@@deriving show { with_path = false }, to_yojson]
  
  and signature = [%import: Typedtree.signature]
    [@@deriving show { with_path = false }, to_yojson]
  
  type signature_item = [%import: Typedtree.signature_item]
    [@@deriving show { with_path = false }, to_yojson]
  
  and signature_item_desc = [%import: Typedtree.signature_item_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and module_declaration = [%import: Typedtree.module_declaration]
    [@@deriving show { with_path = false }, to_yojson]
  
  and module_substitution = [%import: Typedtree.module_substitution]
    [@@deriving show { with_path = false }, to_yojson]
  
  and module_type_declaration = [%import: Typedtree.module_type_declaration]
    [@@deriving show { with_path = false }, to_yojson]
  
  and 'a open_infos = [%import: 'a Typedtree.open_infos]
    [@@deriving show { with_path = false }, to_yojson]
  
  and open_description = [%import: Typedtree.open_description]
    [@@deriving show { with_path = false }, to_yojson]
  
  and open_declaration = [%import: Typedtree.open_declaration]
    [@@deriving show { with_path = false }, to_yojson]
  
  and 'a include_infos = [%import: 'a Typedtree.include_infos]
    [@@deriving show { with_path = false }, to_yojson]
  
  and include_description = [%import: Typedtree.include_description]
    [@@deriving show { with_path = false }, to_yojson]
  
  and include_declaration = [%import: Typedtree.include_declaration]
    [@@deriving show { with_path = false }, to_yojson]
  
  and with_constraint = [%import: Typedtree.with_constraint]
    [@@deriving show { with_path = false }, to_yojson]
  
  and core_type = [%import: Typedtree.core_type]
    [@@deriving show { with_path = false }, to_yojson]
  
  and core_type_desc = [%import: Typedtree.core_type_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and package_type = [%import: Typedtree.package_type]
    [@@deriving show { with_path = false }, to_yojson]
  
  and row_field = [%import: Typedtree.row_field]
    [@@deriving show { with_path = false }, to_yojson]
  
  and row_field_desc = [%import: Typedtree.row_field_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and object_field = [%import: Typedtree.object_field]
    [@@deriving show { with_path = false }, to_yojson]
  
  and object_field_desc = [%import: Typedtree.object_field_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and value_description = [%import: Typedtree.value_description]
    [@@deriving show { with_path = false }, to_yojson]
  
  and type_declaration = [%import: Typedtree.type_declaration]
    [@@deriving show { with_path = false }, to_yojson]
  
  and type_kind = [%import: Typedtree.type_kind]
    [@@deriving show { with_path = false }, to_yojson]
  
  and label_declaration = [%import: Typedtree.label_declaration]
    [@@deriving show { with_path = false }, to_yojson]
  
  and constructor_declaration = [%import: Typedtree.constructor_declaration]
    [@@deriving show { with_path = false }, to_yojson]
  
  and constructor_arguments = [%import: Typedtree.constructor_arguments]
    [@@deriving show { with_path = false }, to_yojson]
  
  and type_extension = [%import: Typedtree.type_extension]
    [@@deriving show { with_path = false }, to_yojson]
  
  and type_exception = [%import: Typedtree.type_exception]
    [@@deriving show { with_path = false }, to_yojson]
  
  and extension_constructor = [%import: Typedtree.extension_constructor]
    [@@deriving show { with_path = false }, to_yojson]
  
  and extension_constructor_kind = [%import: Typedtree.extension_constructor_kind]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_type = [%import: Typedtree.class_type]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_type_desc = [%import: Typedtree.class_type_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_signature = [%import: Typedtree.class_signature]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_type_field = [%import: Typedtree.class_type_field]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_type_field_desc = [%import: Typedtree.class_type_field_desc]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_declaration = [%import: Typedtree.class_declaration]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_description = [%import: Typedtree.class_description]
    [@@deriving show { with_path = false }, to_yojson]
  
  and class_type_declaration = [%import: Typedtree.class_type_declaration]
    [@@deriving show { with_path = false }, to_yojson]
  
  and 'a class_infos = [%import: 'a Typedtree.class_infos]
    [@@deriving show { with_path = false }, to_yojson]
  
  type pattern_action = [%import: Typedtree.pattern_action]
    [@@deriving show { with_path = false }, to_yojson]
  
  type pattern_transformation = [%import: Typedtree.pattern_transformation]
    [@@deriving show { with_path = false }, to_yojson]
  
  type pattern_predicate = [%import: Typedtree.pattern_predicate]
    [@@deriving show { with_path = false }, to_yojson]
  
end
