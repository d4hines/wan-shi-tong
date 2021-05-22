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

module Asttypes = struct
  include Asttypes

  type constant = [%import: Asttypes.constant]
  [@@deriving show { with_path = false }, to_yojson]

  and rec_flag = [%import: Asttypes.rec_flag]
  [@@deriving show { with_path = false }, to_yojson]

  and direction_flag = [%import: Asttypes.direction_flag]
  [@@deriving show { with_path = false }, to_yojson]

  and private_flag = [%import: Asttypes.private_flag]
  [@@deriving show { with_path = false }, to_yojson]

  and mutable_flag = [%import: Asttypes.mutable_flag]
  [@@deriving show { with_path = false }, to_yojson]

  and virtual_flag = [%import: Asttypes.virtual_flag]
  [@@deriving show { with_path = false }, to_yojson]

  and override_flag = [%import: Asttypes.override_flag]
  [@@deriving show { with_path = false }, to_yojson]

  and closed_flag = [%import: Asttypes.closed_flag]
  [@@deriving show { with_path = false }, to_yojson]

  and label = [%import: Asttypes.label]
  [@@deriving show { with_path = false }, to_yojson]

  and arg_label = [%import: Asttypes.arg_label]
  [@@deriving show { with_path = false }, to_yojson]

  and variance = [%import: Asttypes.variance]
  [@@deriving show { with_path = false }, to_yojson]

  and 'a loc = [%import: 'a Asttypes.loc]
  [@@deriving show { with_path = false }, to_yojson]
end

module Primitive = struct
  include Primitive

  type native_repr = [%import: Primitive.native_repr]
  [@@deriving show { with_path = false }, to_yojson]

  and boxed_integer = [%import: Primitive.boxed_integer]
  [@@deriving show { with_path = false }, to_yojson]

  type to_show_description = {
    prim_name : string;
    prim_arity : int;
    prim_alloc : bool;
    prim_native_name : string;
    prim_native_repr_args : native_repr list;
    prim_native_repr_res : native_repr;
  }
  [@@deriving show { with_path = false }, to_yojson]

  let do_the_magic
      ({
         prim_name;
         prim_arity;
         prim_alloc;
         prim_native_name;
         prim_native_repr_args;
         prim_native_repr_res;
       } :
        description) =
    {
      prim_name;
      prim_arity;
      prim_alloc;
      prim_native_name;
      prim_native_repr_args;
      prim_native_repr_res;
    }

  let pp_description formatter descr =
    pp_to_show_description formatter (do_the_magic descr)

  let description_to_yojson desc =
    desc |> do_the_magic |> to_show_description_to_yojson
end

module Parsetree = struct
  include Parsetree

  type constant = [%import: Parsetree.constant]
  [@@deriving show { with_path = false }, to_yojson]

  and location_stack = [%import: Parsetree.location_stack]
  [@@deriving show { with_path = false }, to_yojson]

  and attribute = [%import: Parsetree.attribute]
  [@@deriving show { with_path = false }, to_yojson]

  and payload = [%import: Parsetree.payload]
  [@@deriving show { with_path = false }, to_yojson]

  and extension = [%import: Parsetree.extension]
  [@@deriving show { with_path = false }, to_yojson]

  and attributes = [%import: Parsetree.attributes]
  [@@deriving show { with_path = false }, to_yojson]

  and core_type = [%import: Parsetree.core_type]
  [@@deriving show { with_path = false }, to_yojson]

  and core_type_desc = [%import: Parsetree.core_type_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and package_type = [%import: Parsetree.package_type]
  [@@deriving show { with_path = false }, to_yojson]

  and row_field = [%import: Parsetree.row_field]
  [@@deriving show { with_path = false }, to_yojson]

  and row_field_desc = [%import: Parsetree.row_field_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and object_field = [%import: Parsetree.object_field]
  [@@deriving show { with_path = false }, to_yojson]

  and object_field_desc = [%import: Parsetree.object_field_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and pattern = [%import: Parsetree.pattern]
  [@@deriving show { with_path = false }, to_yojson]

  and pattern_desc = [%import: Parsetree.pattern_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and expression = [%import: Parsetree.expression]
  [@@deriving show { with_path = false }, to_yojson]

  and expression_desc = [%import: Parsetree.expression_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and case = [%import: Parsetree.case]
  [@@deriving show { with_path = false }, to_yojson]

  and letop = [%import: Parsetree.letop]
  [@@deriving show { with_path = false }, to_yojson]

  and binding_op = [%import: Parsetree.binding_op]
  [@@deriving show { with_path = false }, to_yojson]

  and value_description = [%import: Parsetree.value_description]
  [@@deriving show { with_path = false }, to_yojson]

  and type_declaration = [%import: Parsetree.type_declaration]
  [@@deriving show { with_path = false }, to_yojson]

  and type_kind = [%import: Parsetree.type_kind]
  [@@deriving show { with_path = false }, to_yojson]

  and label_declaration = [%import: Parsetree.label_declaration]
  [@@deriving show { with_path = false }, to_yojson]

  and constructor_declaration = [%import: Parsetree.constructor_declaration]
  [@@deriving show { with_path = false }, to_yojson]

  and constructor_arguments = [%import: Parsetree.constructor_arguments]
  [@@deriving show { with_path = false }, to_yojson]

  and type_extension = [%import: Parsetree.type_extension]
  [@@deriving show { with_path = false }, to_yojson]

  and extension_constructor = [%import: Parsetree.extension_constructor]
  [@@deriving show { with_path = false }, to_yojson]

  and type_exception = [%import: Parsetree.type_exception]
  [@@deriving show { with_path = false }, to_yojson]

  and extension_constructor_kind =
    [%import: Parsetree.extension_constructor_kind]
  [@@deriving show { with_path = false }, to_yojson]

  and class_type = [%import: Parsetree.class_type]
  [@@deriving show { with_path = false }, to_yojson]

  and class_type_desc = [%import: Parsetree.class_type_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and class_signature = [%import: Parsetree.class_signature]
  [@@deriving show { with_path = false }, to_yojson]

  and class_type_field = [%import: Parsetree.class_type_field]
  [@@deriving show { with_path = false }, to_yojson]

  and class_type_field_desc = [%import: Parsetree.class_type_field_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and class_description = [%import: Parsetree.class_description]
  [@@deriving show { with_path = false }, to_yojson]

  and class_type_declaration = [%import: Parsetree.class_type_declaration]
  [@@deriving show { with_path = false }, to_yojson]

  and class_expr = [%import: Parsetree.class_expr]
  [@@deriving show { with_path = false }, to_yojson]

  and class_expr_desc = [%import: Parsetree.class_expr_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and class_structure = [%import: Parsetree.class_structure]
  [@@deriving show { with_path = false }, to_yojson]

  and class_field = [%import: Parsetree.class_field]
  [@@deriving show { with_path = false }, to_yojson]

  and class_field_desc = [%import: Parsetree.class_field_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and class_field_kind = [%import: Parsetree.class_field_kind]
  [@@deriving show { with_path = false }, to_yojson]

  and class_declaration = [%import: Parsetree.class_declaration]
  [@@deriving show { with_path = false }, to_yojson]

  and 'a class_infos = [%import: 'a Parsetree.class_infos]
  [@@deriving show { with_path = false }, to_yojson]

  and 'a open_infos = [%import: 'a Parsetree.open_infos]
  [@@deriving show { with_path = false }, to_yojson]

  and 'a include_infos = [%import: 'a Parsetree.include_infos]
  [@@deriving show { with_path = false }, to_yojson]

  and module_type = [%import: Parsetree.module_type]
  [@@deriving show { with_path = false }, to_yojson]

  and module_type_desc = [%import: Parsetree.module_type_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and functor_parameter = [%import: Parsetree.functor_parameter]
  [@@deriving show { with_path = false }, to_yojson]

  and signature = [%import: Parsetree.signature]
  [@@deriving show { with_path = false }, to_yojson]

  and signature_item = [%import: Parsetree.signature_item]
  [@@deriving show { with_path = false }, to_yojson]

  and signature_item_desc = [%import: Parsetree.signature_item_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and module_declaration = [%import: Parsetree.module_declaration]
  [@@deriving show { with_path = false }, to_yojson]

  and module_substitution = [%import: Parsetree.module_substitution]
  [@@deriving show { with_path = false }, to_yojson]

  and module_type_declaration = [%import: Parsetree.module_type_declaration]
  [@@deriving show { with_path = false }, to_yojson]

  and open_description = [%import: Parsetree.open_description]
  [@@deriving show { with_path = false }, to_yojson]

  and open_declaration = [%import: Parsetree.open_declaration]
  [@@deriving show { with_path = false }, to_yojson]

  and include_description = [%import: Parsetree.include_description]
  [@@deriving show { with_path = false }, to_yojson]

  and include_declaration = [%import: Parsetree.include_declaration]
  [@@deriving show { with_path = false }, to_yojson]

  and with_constraint = [%import: Parsetree.with_constraint]
  [@@deriving show { with_path = false }, to_yojson]

  and module_expr = [%import: Parsetree.module_expr]
  [@@deriving show { with_path = false }, to_yojson]

  and module_expr_desc = [%import: Parsetree.module_expr_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and structure = [%import: Parsetree.structure]
  [@@deriving show { with_path = false }, to_yojson]

  and structure_item = [%import: Parsetree.structure_item]
  [@@deriving show { with_path = false }, to_yojson]

  and structure_item_desc = [%import: Parsetree.structure_item_desc]
  [@@deriving show { with_path = false }, to_yojson]

  and value_binding = [%import: Parsetree.value_binding]
  [@@deriving show { with_path = false }, to_yojson]

  and module_binding = [%import: Parsetree.module_binding]
  [@@deriving show { with_path = false }, to_yojson]

  and toplevel_phrase = [%import: Parsetree.toplevel_phrase]
  [@@deriving show { with_path = false }, to_yojson]

  and toplevel_directive = [%import: Parsetree.toplevel_directive]
  [@@deriving show { with_path = false }, to_yojson]

  and directive_argument = [%import: Parsetree.directive_argument]
  [@@deriving show { with_path = false }, to_yojson]

  and directive_argument_desc = [%import: Parsetree.directive_argument_desc]
  [@@deriving show { with_path = false }, to_yojson]
end

module Typedtree = struct
  open Parsetree
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

  (* type 'a pattern_category = [%import: 'a Typedtree.pattern_category]
     [@@deriving show { with_path = false }, to_yojson] *)
  type pattern = [%import: Typedtree.pattern]
  [@@deriving show { with_path = false }, to_yojson]

  and 'k general_pattern = [%import: 'k Typedtree.general_pattern]
  [@@deriving show { with_path = false }, to_yojson]

  and 'a pattern_data = [%import: 'k Typedtree.pattern_data]
  [@@deriving show { with_path = false }, to_yojson]

  (*
      
      
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
        [@@deriving show { with_path = false }, to_yojson] *)

  (* and functor_parameter = [%import: Typedtree.functor_parameter]
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
       [@@deriving show { with_path = false }, to_yojson] *)
end
